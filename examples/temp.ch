

def

Person = struct(name string, age int)

cmd // We wrap some commands around SQL.

create :
    put SQL ---
        CREATE TABLE IF NOT EXISTS People (
            name VARCHAR(32),
            age INT
        )

add (name string, age int) :
    put SQL ---
        INSERT INTO People
        VALUES ({name, age})

show (name string) :
    get personList as Person from SQL ---
        SELECT * FROM People
        WHERE name=({name}) 
    post prettyFmt(personList) to Terminal()

show all :
    get peopleList as Person from SQL ---
        SELECT * FROM People
    post prettyFmt(peopleList) to Terminal()

def // Some functions to do the formatting.

HEADING = "\n" + spaceOut("Name", "Age") + repeat(36, "-") + "\n"

prettyFmt(people list) :
    people == [] :
        "No records match.\n"
    else :
        HEADING + (people >> spaceOut(that[name], string that[age]) -> sum)

spaceOut(s, t) : s + repeat(33 - len(s), " ") + t + "\n"

repeat(n int, s string) : 0::n -> range >> s -> sum 

