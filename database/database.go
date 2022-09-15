package database

import (
	"database/sql"
	"errors"
	"fmt"
	"os"
	"sort"

	"golang.org/x/crypto/bcrypt"

	"charm/text"

	// SQL drivers

	_ "github.com/go-sql-driver/mysql"  // MariaDB & MySQL
	_ "github.com/lib/pq"               // Postgres
	_ "github.com/nakagami/firebirdsql" // Firebird
	_ "github.com/sijms/go-ora"         // Oracle
	_ "modernc.org/sqlite"              // SQLite
)

// List of SQL drivers for when I want to import more: https://zchee.github.io/golang-wiki/SQLDrivers/


var (
	drivers = map[string]string {"Firebird SQL" : "firebirdsql", "MariaDB" : "mysql", "MySQL" : "mysql",
								 "Oracle" : "oracle", "Postgres" : "postgres", "SQLite" : "sqlite"}
)
 
func GetdB(driver, host, port, db, user, password string) (*sql.DB, error) {
	
	connectionString := fmt.Sprintf( "host=%v port=%v dbname=%v user=%v password=%v sslmode=disable", 
		host, port, db, user, password)


	sqlObj, connectionError := sql.Open(drivers[driver], connectionString); if connectionError != nil {
		return nil, connectionError	}

	err := sqlObj.Ping()

	if err != nil { return nil, err }
	
	return sqlObj, nil
}

func GetDriverOptions() string {
	result := "The following SQL drivers are available: \n\n"
	for k, v := range(GetSortedDrivers()) {
		result = result + fmt.Sprintf("  [%v] %v\n", k, v)
	}
	result = result + "\nPick a number"
	return result
}

func GetSortedDrivers() []string {
	dr := []string{}
	for k := range(drivers) {
		dr = append(dr, k)
	}
	sort.Strings(dr)
	return dr
}

func AddAdmin(db *sql.DB, username, firstName, lastName, email, password, serviceName string) error {

	query := 
`CREATE TABLE IF NOT EXISTS _Users (
    username varchar(32),
    firstName varchar(32),
    lastName varchar(32),
    password varchar(60),
    email varchar(60),
	serviceName varchar(32),
PRIMARY KEY (username));

CREATE TABLE IF NOT EXISTS _Groups (
    groupName varchar(32),
PRIMARY KEY (groupName));

CREATE TABLE IF NOT EXISTS _GroupMemberships (
    username varchar(32) REFERENCES _Users ON DELETE CASCADE,
    groupName varchar(32) REFERENCES _Groups ON DELETE CASCADE,
    owner BOOLEAN DEFAULT FALSE,
PRIMARY KEY (username, groupName));

CREATE TABLE IF NOT EXISTS _GroupServices (
    groupName varchar(32) REFERENCES _Groups ON DELETE CASCADE,
	serviceName varchar(32),
PRIMARY KEY (groupName, serviceName));

INSERT INTO _Groups (groupName)
VALUES('Admin')
ON CONFLICT DO NOTHING;

INSERT INTO _Groups (groupName)
VALUES('Users')
ON CONFLICT DO NOTHING;

INSERT INTO _Groups (groupName)
VALUES('Guests')
ON CONFLICT DO NOTHING;`
	_, err := db.Exec(query)
	if err != nil {
		return err
	}

	err = AddUser(db, username, firstName, lastName, email, password, serviceName)
	if err != nil {
		return err
	}
	for _, group := range([]string{"Admin", "Guests", "Users"}) {
		err = AddUserToGroup(db, username, group, true)
		if err != nil {
			return err
		}
	}

	// This should only ever happen to a hub once. We create the file "rsc/admin.dat"
	// to prove that it has.
	_, err = os.Create("rsc/admin.dat")
	return err
} 

func AddUserToGroup(db *sql.DB, username, groupName string, owner bool) error {
	query := 
`INSERT INTO _GroupMemberships(username, groupName, owner)
	VALUES ($1, $2, $3)`
	_, err := db.Exec(query, username, groupName, owner)
	return err
}

func LetGroupUseService(db *sql.DB, groupName, serviceName string) error {
	query := 
`INSERT INTO _GroupServices(groupName, serviceName)
	VALUES ($1, $2)`
	_, err := db.Exec(query, groupName, serviceName)
	return err
}

func UpdateService(db *sql.DB, username, serviceName string) error {
	query := 
`UPDATE _Users
SET serviceName = $2
WHERE username = $1`
	_, err := db.Exec(query, username, serviceName)
	return err
}

type groupRow struct {
	username string
	groupName string
	owner bool
}

func GetGroupsForUser(db *sql.DB, username string) (string, error) {
	rows, err := db.Query("SELECT * FROM _GroupMemberships WHERE username = $1", username)
    if err != nil {
        return "", err
    }
    defer rows.Close()

    var groups []groupRow

    for rows.Next() {
        var group groupRow
		if err := rows.Scan(&group.username, &group.groupName, &group.owner); err != nil {
            return "", err
        }
        groups = append(groups, group)
    }
    
	result := "\n"

	ownerGroups := []string{}
	userGroups := []string{}
	for _, v := range(groups) {
		if v.owner {
			ownerGroups = append(ownerGroups, v.groupName)
		}	else {
			userGroups = append(userGroups, v.groupName)
		}
	}
	sort.Strings(ownerGroups)
	if len(ownerGroups) > 0 {
		result = result + "You are an owner of the following groups:\n\n"
		for _, v := range(ownerGroups) {
			result = result + text.BULLET + v + "\n"
		}
		result = result + "\n"
	}

	sort.Strings(userGroups)
	if len(userGroups) > 0 {
		result = result + "You are a member of the following groups:\n\n"
		for _, v := range(userGroups) {
			result = result + text.BULLET + v + "\n"
		}
		result = result + "\n"
	}

	return result, nil
}


func GetAccessForUser(db *sql.DB, username string) (string, error) {
	rows, err := db.Query(
`SELECT _GroupServices.serviceName FROM _GroupMemberships 
INNER JOIN _GroupServices
ON _GroupMemberships.groupName = _GroupServices.groupName
WHERE username = $1`, username)
    if err != nil {
        return "", err
    }
    defer rows.Close()

    var services []string

    for rows.Next() {
        var service string
		if err := rows.Scan(&service); err != nil {
            return "", err
        }
        services = append(services, service)
    }
    
	result := "\n"

	sort.Strings(services)
	result = result + "You have access to the following services:\n\n"
	for _, v := range(services) {
		if v != "" { result = result + text.BULLET + v + "\n" }
	}

	if result == "\n" { return "\nYou do not have access to any services.\n\n", nil}

	return result + "\n", nil
}

func IsUserGroupOwner(db *sql.DB, username, groupName string) error {
	var count int

	row := db.QueryRow("SELECT COUNT (*) FROM _GroupMemberships WHERE username = $1 AND groupName = $2 AND owner = TRUE",
				username, groupName)
	err := row.Scan(&count)
	if err != nil {
		return err
	}
	if count == 0 {
		return errors.New("you aren't an owner of a group '" + groupName + "'.")
	}
	return nil
}

func IsUserAdmin(db *sql.DB, username string) (bool, error) {
	return IsUserInGroup(db, username, "Admin")
}

func IsUserInGroup(db *sql.DB, username, groupName string) (bool, error) {
	var count int

	row := db.QueryRow("SELECT COUNT (*) FROM _GroupMemberships WHERE username = $1 AND groupName = $2",
				username, groupName)
	err := row.Scan(&count)
	if err != nil {
		return false, err
	}
	return count == 1, nil
}

func DoesUserHaveAccess(db *sql.DB, username, serviceName string) (bool, error) {

	if serviceName == "" { return true, nil }

	var count int

	row := db.QueryRow(
`SELECT COUNT (*) FROM _GroupMemberships 
INNER JOIN _GroupServices
USING (groupName)
WHERE _GroupMemberships.username = $1 AND _GroupServices.serviceName = $2`,
		username, serviceName)
	err := row.Scan(&count)
	if err != nil {
		return false, err
	}
	return count == 1, nil
}

type userRow struct {
	password string
	serviceName string
}

func ValidateUser(db *sql.DB, username, password string) (string, error) {
	var userData userRow
	
	rows, err := db.Query("SELECT password, serviceName FROM _Users WHERE username = $1", username)
	if err != nil {
		return "", err
	}
	for rows.Next() {
		if err := rows.Scan(&userData.password, &userData.serviceName); err != nil {
			return "", err
		}
		if err = bcrypt.CompareHashAndPassword([]byte(userData.password), []byte(password)); err != nil {
			return "", errors.New("the hub doesn't recognize that combination of username and password")
		}
		
		return userData.serviceName, nil
		
	}
	// The case where there are no rows.
	return "", errors.New("the hub doesn't recognize that combination of username and password")
}

func AddUser(db *sql.DB, username, firstName, lastName, email, password, serviceName string) error {
		query := 
`INSERT INTO _Users(username, firstName, lastName, password, email, serviceName)
	VALUES ($1, $2, $3, $4, $5, $6)`
	_, err := db.Exec(query, username, firstName, lastName, encrypt(password), encrypt(email), serviceName)

	return err
} 

func AddGroup(db *sql.DB, groupName string) error {
	query := 
`INSERT INTO _Groups(groupName)
VALUES ($1)`
_, err := db.Exec(query, groupName)

return err
} 

func encrypt(s string) string{
	result, _ := bcrypt.GenerateFromPassword([]byte(s), bcrypt.DefaultCost)
	return string(result)
}