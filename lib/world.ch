import

gocode "math/rand"
gocode "fmt"
gocode "bufio"
gocode "os"
gocode "io/ioutil"
gocode "time"

def

Random = struct(params single)

TimeUnit = enum SECONDS, MILLISECONDS, NANOSECONDS

UnixClock = struct(unit TimeUnit)

Terminal = struct()

Output = struct()

Prompt = struct(message string)

File = struct(filepath string, asType type)

File(filepath string) :
    File(filepath, string) 

FileExists = struct(filepath string)

cmd

get (x ast) from (rng Random) :
     x varname = randomFunctionOf(rng)  

get (x ast) from (clock UnixClock) :
    x varname = goGetUnixClock(string (clock[unit]))

get (x ast) from (prompt Prompt) :
     x varname = goGetFromPrompt(prompt[message])

post (x string) to (output Output) :
    goPostToOutput(x)

post (x string) to (terminal Terminal) :
    goPrintln(x)

get (contents ast) from (fileAccess File) :
    fileAccess[asType] == string :
        contents varname = goGetFileAsString(fileAccess[filepath])
    fileAccess[asType] == list :
        contents varname = goGetFileAsList(fileAccess[filepath])
    else :
        error "can't get file as type <" + string(fileAccess[asType]) + ">"

put (s string) into (fileAccess File) : 
    goPutStringInFile(s, fileAccess[filepath])

post (s string) to (fileAccess File) : 
    goPostStringInFile(s, fileAccess[filepath])

get (x ast) from (fileAccess FileExists) :
    x varname = goFileExists(fileAccess[filepath])

delete (fileAccess File) :
    goDeleteFile(fileAccess[filepath])


def

// Snippets of go

goRandomInt(i int) : gocode {
    return rand.Intn(i)
}

goGetUnixClock(s string) : gocode {
    switch s {
        case "SECONDS" :
            return int(time.Now().Unix())
        case "MILLISECONDS" :
            return int(time.Now().UnixMilli())
        case "NANOSECONDS" : 
            return int(time.Now().UnixNano())
        default :
            return &object.Error{Message: "this error should never be thrown and should be reported as a bug if seen"}  
    }
}

goPrintln(s string) : gocode {
    fmt.Println(s)
    return &object.SuccessfulAssignment{}
}

goFileExists(fname string) : gocode {
    if _, err := os.Stat(fname); err == nil {
      return object.TRUE
    } else {
      return object.FALSE
    }
}

goDeleteFile(fname string) : gocode {
    err := os.Remove(fname) 
    if err != nil {
        return &object.Error{Message: "can't delete file '" + fname + "'"}
    } else {
        return &object.SuccessfulAssignment{}
    }
}

goGetFileAsString(fname string) : gocode {
    fileContent, err := ioutil.ReadFile(fname)
    if err != nil {
        return &object.Error{Message : "can't find file '" + fname + "'"}
    }
    return &object.String{Value : string(fileContent)}
}

goGetFileAsList(fname string) : gocode {
    file, err := os.Open(fname)
    if err != nil {
        return &object.Error{Message : "can't find file '" + fname + "'"}
    }
    defer file.Close()

    result := &object.List{Elements: []object.Object{}}
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        result.Elements =
            append(result.Elements, &object.String{Value: scanner.Text()})
    }
    return result
}

goPutStringInFile(output string, fname string) : gocode {
    f, err := os.Create(fname)
    if err != nil {
        return &object.Error{Message: "can't access file '" + fname + "'"}
    }
    defer f.Close()
    _, err2 := f.WriteString(output)

    if err2 != nil {
        return &object.Error{Message: "can't write to file '" + fname + "'"}
    }
    return &object.SuccessfulAssignment{}
}

goPostStringInFile(output string, fname string) : gocode {
    if _, err := os.Stat(fname); err == nil {
        return &object.Error{Message: "file '" + fname + "' already exists"}
    }
    f, err2 := os.Create(fname)
    if err2 != nil {
        return &object.Error{Message: "can't access file '" + fname + "'"}
    }
    defer f.Close()
    _, err3 := f.WriteString(output)

    if err3 != nil {
        return &object.Error{Message: "can't write to file '" + fname + "'"}
    }
    return &object.SuccessfulAssignment{}
}

// Handles different parameters for Random.

randomFunctionOf(randomizer) :
    type parameter == int : 
        parameter <= 0 :
            error "range of Random object cannot be <= 0"
        else :
            goRandomInt(parameter)
    type parameter == list :
        parameter == [] :
            error "can't take random element of empty list"
        else :
            parameter[goRandomInt(len parameter)]
    else :
        error "can't randomize things of type " + string(type parameter)
given :
    parameter = randomizer[params]




var z single = 0   // TODO: For testing --- remove.