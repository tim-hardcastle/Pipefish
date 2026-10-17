//go:build js && wasm

package filesystem

import (
	"errors"
	"path/filepath"
	"strings"
	"time"
    "fmt"
)

func (vfs *VFS) ReadFile(path string) ([]byte, error) {
	path = vfs.cleanPath(path)
	data, ok := vfs.files.Get(path)
	if !ok {
		return nil, errors.New("file does not exist")
	}
	// We don't let callers modify the VFS's stored copy.
	return append([]byte(nil), data...), nil
}

func (vfs *VFS) WriteFile(path string, data []byte) error {
    path = vfs.cleanPath(path)
    if err := vfs.CreateDirectory(filepath.Dir(path)); err != nil {
        return err
    }
    vfs.files.Set(path, append([]byte(nil), data...))
    vfs.dirs.Set(path, false)
    return nil
}

func (vfs *VFS) DeleteFile(path string) error {
	path = vfs.cleanPath(path)
	if _, exists := vfs.files.Get(path); !exists {
		return errors.New("file does not exist")
	}
	vfs.files.Delete(path)
	vfs.dirs.Delete(path)
	return nil
}

func (vfs *VFS) FileExists(path string) bool {
	path = vfs.cleanPath(path)
	_, exists := vfs.files.Get(path)
	return exists
}

func (vfs *VFS) CopySubtree(directory string) (*VFS, error) {
    directory = vfs.cleanPath(directory)

    if directory != "." {
        isDir, exists := vfs.dirs.Get(directory)
        if !exists || !isDir {
            return nil, fmt.Errorf(
                "directory %q does not exist",
                directory,
            )
        }
    }

    copy := NewVFS()

    relativePath := func(path string) string {
        if directory == "." {
            return path
        }

        return strings.TrimPrefix(
            strings.TrimPrefix(path, directory),
            "/",
        )
    }

    // Copy directories first, including empty directories.
    directories, err := vfs.GetDirectoryNames(directory, true)
    if err != nil {
        return nil, err
    }

    for _, dir := range directories {
        if err := copy.CreateDirectory(relativePath(dir)); err != nil {
            return nil, err
        }
    }

    // Copy file contents. ReadFile and WriteFile both copy
    // their byte slices, so the two VFS instances are independent.
    filenames, err := vfs.GetFilenames(directory, true)
    if err != nil {
        return nil, err
    }

    for _, filename := range filenames {
        data, err := vfs.ReadFile(filename)
        if err != nil {
            return nil, err
        }

        if err := copy.WriteFile(relativePath(filename), data); err != nil {
            return nil, err
        }
    }

    return copy, nil
}

type vfsFileInfo struct {
	name  string
	isDir bool
}

func (info vfsFileInfo) Name() string {
	return info.name
}

func (info vfsFileInfo) IsDir() bool {
	return info.isDir
}

func (vfs *VFS) CreateDirectory(path string) error {
    path = vfs.cleanPath(path)

    if path == "." {
        return nil
    }

    // Create every directory in the path, starting with its parents.
    current := ""
    for _, part := range strings.Split(path, "/") {
        if current == "" {
            current = part
        } else {
            current += "/" + part
        }

        if isDir, exists := vfs.dirs.Get(current); exists {
            if !isDir {
                return fmt.Errorf("%q is a file, not a directory", current)
            }
            continue
        }

        vfs.dirs.Set(current, true)
    }

    return nil
}

func (vfs *VFS) cleanPath(path string) string {
	path = filepath.ToSlash(path)
	path = strings.TrimPrefix(path, "./")
	path = strings.TrimPrefix(path, "/")
	if path == "" {
		return "."
	}
	return path
}

func (fs *VFS) GetFilenames(directory string, recursive bool) ([]string, error) {
    result := []string{}

	for pair := fs.files.Oldest(); pair != nil; pair = pair.Next() {

		path := pair.Key
        if recursive {
			if filepath.Dir(path) == directory ||
				directory == "." ||
				strings.HasPrefix(path, directory+string(filepath.Separator)) {
				result = append(result, path)
			}
        } else if filepath.Dir(path) == directory {
            result = append(result, path)
        }
    }

    return result, nil
}

func (fs *VFS) GetDirectoryNames(directory string, recursive bool) ([]string, error) {
    result := []string{}

	for pair := fs.dirs.Oldest(); pair != nil; pair = pair.Next() {

    	path, isDir := pair.Key, pair.Value
        if !isDir || path == directory {
            continue
        }

        if recursive {
            if filepath.Dir(path) == directory ||
                strings.HasPrefix(path, directory+string(filepath.Separator)) {
                result = append(result, path)
            }
        } else if filepath.Dir(path) == directory {
            result = append(result, path)
        }
    }
    return result, nil
}

// TODO --- will need to do this properly to implement the hub.
func (fs VFS) ModTime(path string) time.Time {
    return time.Time{}
}

func (vfs *VFS) DeleteDirectory(path string) error {
    path = vfs.cleanPath(path)

    if path == "." {
        return errors.New("cannot delete the root directory")
    }

    isDir, exists := vfs.dirs.Get(path)
    if !exists || !isDir {
        return fmt.Errorf("directory %q does not exist", path)
    }

    prefix := path + "/"

    // Remove files in the directory and all its descendants.
	for pair := vfs.files.Oldest(); pair != nil; pair = pair.Next() {
    	name := pair.Key
        if name == path || strings.HasPrefix(name, prefix) {
            vfs.files.Delete(name)
        }
    }

    // Remove the directory and all descendant entries.
	for pair := vfs.dirs.Oldest(); pair != nil; pair = pair.Next() {
    	name := pair.Key
        if name == path || strings.HasPrefix(name, prefix) {
            vfs.dirs.Delete(name)
        }
    }

    return nil
}

func (vfs *VFS) Rename(oldPath, newPath string) error {
    oldPath = vfs.cleanPath(oldPath)
    newPath = vfs.cleanPath(newPath)

    if oldPath == "." || newPath == "." {
        return errors.New("cannot rename the root directory")
    }

    if oldPath == newPath {
        return nil
    }

    isDir, exists := vfs.dirs.Get(oldPath)
    if !exists {
        return fmt.Errorf("%q does not exist", oldPath)
    }

    if _, exists := vfs.dirs.Get(newPath); exists {
        return fmt.Errorf("%q already exists", newPath)
    }

    // A directory cannot be moved inside itself.
    if isDir && strings.HasPrefix(newPath, oldPath+"/") {
        return errors.New("cannot move a directory inside itself")
    }

    if isDir {
        oldPrefix := oldPath + "/"
        newPrefix := newPath + "/"

        // Collect directory entries before changing the map.
        dirs := make(map[string]bool)
        for pair := vfs.dirs.Oldest(); pair != nil; pair = pair.Next() {
			name, value := pair.Key, pair.Value
            if strings.HasPrefix(name, oldPrefix) {
                dirs[newPrefix+strings.TrimPrefix(name, oldPrefix)] = value
            }
        }

        // Collect file contents before changing the map.
        files := make(map[string][]byte)
        for pair := vfs.files.Oldest(); pair != nil; pair = pair.Next() {
			name, data := pair.Key, pair.Value
            if strings.HasPrefix(name, oldPrefix) {
                files[newPrefix+strings.TrimPrefix(name, oldPrefix)] = data
            }
        }

        // Remove the old directory tree.
        for pair := vfs.dirs.Oldest(); pair != nil; pair = pair.Next() {
			name := pair.Key
            if name == oldPath || strings.HasPrefix(name, oldPrefix) {
                vfs.dirs.Delete(name)
            }
        }
        for pair := vfs.files.Oldest(); pair != nil; pair = pair.Next() {
			name, _ := pair.Key, pair.Value
            if strings.HasPrefix(name, oldPrefix) {
                vfs.files.Delete(name)
            }
        }

        // Install the renamed directory tree.
        vfs.dirs.Set(newPath, true)
        for name, value := range dirs {
            vfs.dirs.Set(name, value)
        }
        for name, data := range files {
            vfs.files.Set(name, data)
        }

        return nil
    }

    // Rename a file.
    data, _ := vfs.files.Get(oldPath)
    vfs.files.Delete(oldPath)
    vfs.dirs.Delete(oldPath)

    vfs.files.Set(newPath, data)
    vfs.dirs.Set(newPath, false)

    return nil
}

