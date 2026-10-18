//go:build js && wasm

package main

import (
	"bytes"
	"path/filepath"
	pathpkg "path"
	"sort"
	"strings"
	"syscall/js"
	"archive/zip"

	"github.com/tim-hardcastle/pipefish/source/err"
	"github.com/tim-hardcastle/pipefish/source/filesystem"
	"github.com/tim-hardcastle/pipefish/source/initializer"
	"github.com/tim-hardcastle/pipefish/source/pf"
	"github.com/tim-hardcastle/pipefish/source/text"
	"github.com/tim-hardcastle/pipefish/source/values"
	"github.com/tim-hardcastle/pipefish/source/vm"
	"github.com/tim-hardcastle/pipefish/web-component/generated-go/registry"
)

var (
	service       *pf.Service
	mainFilepath  string
	fs            *filesystem.VFS
	authorFS      *filesystem.VFS
    bookFS        *filesystem.VFS
	oH            *vm.CapturingOutHandler
	originalFiles map[string][]byte
)

func compile(this js.Value, args []js.Value) any {
	files := args[0]
	fs = filesystem.NewVFS()
	originalFiles = make(map[string][]byte)
	mainFilepath = ""

	for i := 0; i < files.Length(); i++ {
		item := files.Index(i)
		path := item.Get("path").String()
		dataValue := item.Get("data")
		data := make([]byte, dataValue.Get("byteLength").Int())
		js.CopyBytesToGo(data, dataValue)

		if err := fs.WriteFile(path, data); err != nil {
			return err.Error()
		}

		originalFiles[path] = data

		if mainFilepath == "" && strings.HasSuffix(path, ".pf") {
			mainFilepath = path
		}
	}

	var (
		main []byte 
		err error
	)
	if mainFilepath != "" {
		main, err = fs.ReadFile(mainFilepath)
		if err != nil {
			return err.Error()
		}
	}

	service = pf.NewService().Update(pf.Dependencies{FileSystem: fs})

	if err := service.InitializeFromCode(string(main)); err != nil {
		errorReport, _ := service.GetErrorReport()
		return errorReport
	}
	service.Update(pf.Dependencies{OutHandler: service.MakeCapturingOutHandler()})
	return nil
}

func getFilepath(this js.Value, args []js.Value) any {
	return mainFilepath
}

func updateFile(this js.Value, args []js.Value) any {
	path := args[0].String()
	data := []byte(args[1].String())

	if err := fs.WriteFile(path, data); err != nil {
		return err.Error()
	}

	return nil
}

func compileMain(this js.Value, args []js.Value) any {
	main, err := fs.ReadFile(mainFilepath)
	if err != nil {
		return err.Error()
	}
	if err := service.InitializeFromCode(string(main)); err != nil {
		return err.Error()
	}
	service.Update(pf.Dependencies{OutHandler: service.MakeCapturingOutHandler()})
	return nil
}

func do(this js.Value, args []js.Value) any {
	var result values.Value
	if args[0].String() == "" {
		compileMain(this, []js.Value{})
	} else {
		result, _ = service.Do(args[0].String())
	}
	if errorsExist, _ := service.ErrorsExist(); errorsExist {
		errors, _ := service.GetErrorReport()
		return text.GetTuiRenderer(-1)(errors)
	}
	if args[0].String() == "" {
		return ""
	}
	if result.T == pf.ERROR {
		e := result.V.(*pf.Error)
		if e.Message == "" {
			e = err.CreateErr(e.ErrorId, e.Token, e.Args...)
		}
		return text.GetTuiRenderer(-1)("[0] " + text.ERROR + e.Message + err.DescribePos(e.Token) + ".")
	}
	if service.PostHappened() {
		dump, _ := service.Dump()
		return dump
	}
	return service.ToString(result)
}

func getFileTree(this js.Value, args []js.Value) any {
	if fs == nil {
		return "filesystem has not been initialized"
	}
	return buildDirectoryTreeFor(fs, ".")
}

func buildDirectoryTreeFor(vfs *filesystem.VFS, directory string) js.Value {
    result := js.Global().Get("Object").New()
    result.Set("type", "folder")
    result.Set("path", directory)

    name := directory
    if directory == "." {
        name = ""
    } else {
        name = filepath.Base(directory)
    }
    result.Set("name", name)

    children := js.Global().Get("Array").New()

    directories, err := vfs.GetDirectoryNames(directory, false)
    if err != nil {
        result.Set("children", children)
        return result
    }
    sort.Strings(directories)

    for _, child := range directories {
        children.Call("push", buildDirectoryTreeFor(vfs, child))
    }

    files, err := vfs.GetFilenames(directory, false)
    if err != nil {
        result.Set("children", children)
        return result
    }
    sort.Strings(files)

    for _, file := range files {
        child := js.Global().Get("Object").New()
        child.Set("type", "file")
        child.Set("name", filepath.Base(file))
        child.Set("path", file)
        children.Call("push", child)
    }

    result.Set("children", children)
    return result
}

func initAuthorFiles(this js.Value, args []js.Value) any {
    if len(args) < 1 || !args[0].Truthy() {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": "missing files array",
        })
    }

    authorFS = filesystem.NewVFS()
    bookFS = nil

    files := args[0]

    for i := 0; i < files.Length(); i++ {
        item := files.Index(i)
        path := item.Get("path").String()
        dataValue := item.Get("data")

        data := make([]byte, dataValue.Get("byteLength").Int())
        js.CopyBytesToGo(data, dataValue)

        if err := authorFS.WriteFile(path, data); err != nil {
            authorFS = nil

            return js.ValueOf(map[string]any{
                "ok":    false,
                "error": err.Error(),
            })
        }
    }

    return js.ValueOf(map[string]any{
        "ok": true,
    })
}

func getAuthorFileTree(this js.Value, args []js.Value) any {
    if authorFS == nil {
        return js.Null()
    }

    return buildDirectoryTreeFor(authorFS, ".")
}

func openBook(this js.Value, args []js.Value) any {
    if authorFS == nil {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": "author filesystem is not initialized",
        })
    }

    if len(args) < 1 {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": "missing directory path",
        })
    }

    directory := args[0].String()

    copied, err := authorFS.CopySubtree(directory)
    if err != nil {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": err.Error(),
        })
    }

    bookFS = copied

    return js.ValueOf(map[string]any{
        "ok":   true,
        "tree": buildDirectoryTreeFor(bookFS, "."),
    })
}

func getBookFileTree(this js.Value, args []js.Value) any {
    if bookFS == nil {
        return js.Null()
    }

    return buildDirectoryTreeFor(bookFS, ".")
}


func readAuthorFile(this js.Value, args []js.Value) any {
    if authorFS == nil {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": "author filesystem has not been initialized",
        })
    }

    if len(args) == 0 {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": "expected a file path",
        })
    }

    data, err := authorFS.ReadFile(args[0].String())
    if err != nil {
        return js.ValueOf(map[string]any{
            "ok":    false,
            "error": err.Error(),
        })
    }

    result := js.Global().Get("Object").New()
    result.Set("ok", true)

    bytes := js.Global().Get("Uint8Array").New(len(data))
    js.CopyBytesToJS(bytes, data)
    result.Set("data", bytes)

    return result
}

// TODO --- use code above, either converting to string with this as a mere wrapper or having just the
// one function returning bytes.
func readBookFile(this js.Value, args []js.Value) interface{} {
	if bookFS == nil {
		result := js.Global().Get("Object").New()
		result.Set("ok", false)
		result.Set("error", "no book is open")
		return result
	}

	if len(args) == 0 {
		result := js.Global().Get("Object").New()
		result.Set("ok", false)
		result.Set("error", "expected a file path")
		return result
	}

	data, err := bookFS.ReadFile(args[0].String())
	if err != nil {
		result := js.Global().Get("Object").New()
		result.Set("ok", false)
		result.Set("error", err.Error())
		return result
	}

	result := js.Global().Get("Object").New()
	result.Set("ok", true)
	result.Set("data", string(data))
	return result
}

func writeBookFile(this js.Value, args []js.Value) interface{} {
    result := js.Global().Get("Object").New()

    if bookFS == nil {
        result.Set("ok", false)
        result.Set("error", "no book is open")
        return result
    }

    if len(args) < 2 {
        result.Set("ok", false)
        result.Set("error", "expected a file path and contents")
        return result
    }

    path := args[0].String()
    contents := args[1].String()

    if err := bookFS.WriteFile(path, []byte(contents)); err != nil {
        result.Set("ok", false)
        result.Set("error", err.Error())
        return result
    }

    result.Set("ok", true)
    return result
}

func readFile(this js.Value, args []js.Value) any {
	result := js.Global().Get("Object").New()

	if fs == nil {
		result.Set("ok", false)
		result.Set("error", "filesystem has not been initialized")
		return result
	}

	data, err := fs.ReadFile(args[0].String())
	if err != nil {
		result.Set("ok", false)
		result.Set("error", err.Error())
		return result
	}

	result.Set("ok", true)
	result.Set("data", string(data))
	return result
}

func createDirectory(this js.Value, args []js.Value) any {
	if fs == nil {
		return "filesystem has not been initialized"
	}
	if err := fs.CreateDirectory(args[0].String()); err != nil {
		return err.Error()
	}
	return nil
}

func deleteFile(this js.Value, args []js.Value) any {
	if fs == nil {
		return "filesystem has not been initialized"
	}

	if err := fs.DeleteFile(args[0].String()); err != nil {
		return err.Error()
	}
	return nil
}

func deleteDirectory(this js.Value, args []js.Value) any {
	if fs == nil {
		return "filesystem has not been initialized"
	}

	if err := fs.DeleteDirectory(args[0].String()); err != nil {
		return err.Error()
	}
	return nil
}

func renamePath(this js.Value, args []js.Value) any {
	if fs == nil {
		return "filesystem has not been initialized"
	}

	oldPath := args[0].String()
	newPath := args[1].String()

	if err := fs.Rename(oldPath, newPath); err != nil {
		return err.Error()
	}
	return nil
}

func revertFile(this js.Value, args []js.Value) any {
    if fs == nil {
        return "filesystem has not been initialized"
    }

    name := pathpkg.Clean(strings.TrimPrefix(args[0].String(), "./"))

    // Revert an individual file.
    if fs.FileExists(name) {
        original, ok := originalFiles[name]
        if !ok {
            return "no original version exists for " + name
        }

        if err := fs.WriteFile(name, original); err != nil {
            return err.Error()
        }

        return nil
    }

    // Revert a folder: remove its current files first.
    currentFiles, _ := fs.GetFilenames(name, true)
    for _, file := range currentFiles {
        if err := fs.DeleteFile(file); err != nil {
            return err.Error()
        }
    }

    // Restore every original file belonging to this folder.
    for file, original := range originalFiles {
        belongs := name == "." ||
            file == name ||
            strings.HasPrefix(file, name+"/")

        if !belongs {
            continue
        }

        if err := fs.WriteFile(file, original); err != nil {
            return err.Error()
        }
    }

    return nil
}

func zipDirectory(this js.Value, args []js.Value) any {
    if fs == nil {
        return errorResult("filesystem has not been initialized")
    }

    directory := pathpkg.Clean(strings.TrimPrefix(args[0].String(), "./"))
    if directory == "" {
        directory = "."
    }

    filenames, err := fs.GetFilenames(directory, true)
    if err != nil {
        return errorResult(err.Error())
    }

    sort.Strings(filenames)

    var buffer bytes.Buffer
    archive := zip.NewWriter(&buffer)

    for _, filename := range filenames {
        name := filename

        if directory != "." {
            name = strings.TrimPrefix(
                strings.TrimPrefix(filename, directory),
                "/",
            )
        }

        contents, err := fs.ReadFile(filename)
        if err != nil {
            archive.Close()
            return errorResult(err.Error())
        }

        writer, err := archive.Create(name)
        if err != nil {
            archive.Close()
            return errorResult(err.Error())
        }

        if _, err := writer.Write(contents); err != nil {
            archive.Close()
            return errorResult(err.Error())
        }
    }

    if err := archive.Close(); err != nil {
        return errorResult(err.Error())
    }

    result := js.Global().Get("Object").New()
    result.Set("ok", true)

    bytesJS := js.Global().Get("Uint8Array").New(buffer.Len())
    js.CopyBytesToJS(bytesJS, buffer.Bytes())
    result.Set("data", bytesJS)

    return result
}

func errorResult(message string) js.Value {
    result := js.Global().Get("Object").New()
    result.Set("ok", false)
    result.Set("error", message)
    return result
}

func main() {
	initializer.RegisterWasmGoPackages(registry.Packages)

	js.Global().Set(
		"pipefishCompile",
		js.FuncOf(compile),
	)

	js.Global().Set(
		"pipefishDo",
		js.FuncOf(do),
	)

	js.Global().Set(
		"pipefishGetFilepath",
		js.FuncOf(getFilepath),
	)

	js.Global().Set(
		"pipefishHighlight",
		js.FuncOf(func(
			this js.Value,
			args []js.Value,
		) interface{} {
			return text.BlockHighlighter(
				args[0].String(),
			)
		}),
	)

	js.Global().Set(
		"pipefishRenderMdAsHtml",
		js.FuncOf(func(
			this js.Value,
			args []js.Value,
		) interface{} {
			return text.RenderMdAsBookHtml(
				args[0].String(),
			)
		}),
	)

	js.Global().Set(
		"pipefishWasmReady",
		true,
	)

	js.Global().Set(
		"pipefishUpdateFile",
		js.FuncOf(updateFile),
	)

	js.Global().Set(
		"pipefishCompileMain",
		js.FuncOf(compileMain),
	)

	js.Global().Set(
		"pipefishGetFileTree",
		js.FuncOf(getFileTree),
	)

	js.Global().Set(
		"pipefishReadFile",
		js.FuncOf(readFile),
	)

	js.Global().Set(
		"pipefishCreateDirectory",
		js.FuncOf(createDirectory),
	)

	js.Global().Set(
		"pipefishDeleteFile",
		js.FuncOf(deleteFile),
	)

	js.Global().Set(
		"pipefishDeleteDirectory",
		js.FuncOf(deleteDirectory),
	)

	js.Global().Set(
		"pipefishRenamePath",
		js.FuncOf(renamePath),
	)

	js.Global().Set(
		"pipefishRevertFile", 
		js.FuncOf(revertFile),
	)

	js.Global().Set(
		"pipefishZipDirectory",
		js.FuncOf(zipDirectory),
	)

	js.Global().Set(
		"pipefishInitAuthorFiles",
		js.FuncOf(initAuthorFiles),
	)

	js.Global().Set(
		"pipefishGetAuthorFileTree",
		js.FuncOf(getAuthorFileTree),
	)

	js.Global().Set(
		"pipefishOpenBook",
		js.FuncOf(openBook),
	)

	js.Global().Set(
		"pipefishGetBookFileTree",
		js.FuncOf(getBookFileTree),
	)

	js.Global().Set(
		"pipefishReadBookFile",
		js.FuncOf(readBookFile),
	)

	js.Global().Set(
		"pipefishReadAuthorFile",
		js.FuncOf(readAuthorFile),
	)

	js.Global().Set(
		"pipefishWriteBookFile",
		js.FuncOf(writeBookFile),
	)

	select {}
}
