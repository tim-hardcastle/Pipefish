

cmd

create :
    post SQL ---
        CREATE TABLE IF NOT EXISTS People (
            name VARCHAR(32),
            age INT
        )

add (name string, age int) :
    post SQL ---
        INSERT INTO People
        VALUES ({name, age})

show (name string) :
    get person from SQL ---
        SELECT * FROM People
        WHERE name={name} 
    post prettyPrint(peopleList) to Terminal()

show all :
    get peopleList from SQL ---
        SELECT * FROM table_name
    post prettyPrint(peopleList) to Terminal()


