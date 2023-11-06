import

gocode "os"
gocode "math/rand"
gocode "fmt"
gocode "bufio"
gocode "io/ioutil"
gocode "time"

languages

SQL

def

Random = struct(params single)

RandomSeed = struct()

Clock = struct()

Time = struct(year, month, day, hour, min, sec, nsec int?, loc string?)

Terminal = struct()

Output = struct()

Input = struct(prompt string)

File = struct(filepath string, asType type)

File(filepath string) :
    File(filepath, string) 

FileExists = struct(filepath string)

cmd

get (x ref) from (rng Random) :
     x = randomFunctionOf(rng)  

put (seed int) into (randomizer RandomSeed) :
    goRandomize(seed)

get (x ref) from (c Clock) :
    x = goGetClock()


post (x tuple) :
    post x to Output()

// Only the evaluator, which can see the context, knows which database the hub
// is pointing at.
post (x SQL) : builtin "post_to_SQL" // Which of these to use is up to the user.
put (x SQL) : builtin "post_to_SQL"
delete (x SQL) : builtin "post_to_SQL"
get (x ref) as (t type) from (y SQL) :
     x = builtinGetSQL t, y
builtinGetSQL(t type, s SQL) : builtin "get_from_SQL"

// And the contacts also have to be built in.
post (x contact) : builtin "post_to_contact" // Which of these to use is up to the user.
put (x contact) : builtin "post_to_contact"
delete (x contact) : builtin "post_to_contact"
get (x ref) from (c contact) :
    x = builtinGetContact c
builtinGetContact(c contact) : builtin "get_from_contact"

post (x tuple) to (terminal Terminal) :
    goPrintln(literal x)

// Note that these can't be implemented here nor indeed as builtins, since this has to be done
// by the evaluator, which can see the context and knows where to input from and output to.
// So we'll hijack the evalBuiltin method like we did to implement the 'for' loop.
post (x tuple) to (output Output) : builtin "post_to_output"
get (x ref) from (input Input) :
    x = builtinGet input[prompt]
builtinGet(s string) : builtin "get_from_input"

get (contents ref) from (fileAccess File) : 
    fileAccess[asType] == string :
        contents = goGetFileAsString(fileAccess[filepath])
    fileAccess[asType] == list :
        contents = goGetFileAsList(fileAccess[filepath])
    else :
        error "can't get file as type <" + string(fileAccess[asType]) + ">"

put (s string) into (fileAccess File) : 
    goPutStringInFile(s, fileAccess[filepath])

get (x ref) from (fileAccess FileExists) :
    x = goFileExists(fileAccess[filepath])

delete (fileAccess File) :
    goDeleteFile(fileAccess[filepath])

def

// Snippets of go

goRandomInt(i int) : gocode {
    return rand.Intn(i)
}

goRandomize(i int) : gocode {
    rand.Seed(int64(i))
    return object.SUCCESS
}

goGetClock() : gocode {
    goNow := time.Now()
    charmNow := &object.Struct{Name: "Time", Labels: []string{"year", "month", "day", "hour", "min", "sec", "nsec", "loc"}, Value: make(map[string]object.Object)}
    charmNow.Value["year"] = &object.Integer{Value: goNow.Year()}
    charmNow.Value["month"] = &object.Integer{Value: int(goNow.Month())}
    charmNow.Value["day"] = &object.Integer{Value: goNow.Day()}
    charmNow.Value["hour"] = &object.Integer{Value: goNow.Hour()}
    charmNow.Value["min"] = &object.Integer{Value: goNow.Minute()}
    charmNow.Value["sec"] = &object.Integer{Value: goNow.Second()}
    charmNow.Value["nsec"] = &object.Integer{Value: goNow.Nanosecond()}
    charmNow.Value["loc"] = &object.String{Value: goNow.Location().String()}
    return charmNow
}

goPrintln(s string) : gocode {
    fmt.Println(s)
    return object.SUCCESS
}

goFileExists(fname string) : gocode {
    _, err := os.Stat(fname)
    return err == nil
}

goDeleteFile(fname string) : gocode {
    err := os.Remove(fname) 
    if err != nil {
        return &object.Error{Message: "can't delete file '" + fname + "'"}
    } else {
        return object.SUCCESS
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
    return object.SUCCESS
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

