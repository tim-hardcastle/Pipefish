package database

// When I do the SQL integration I can just put this stuff in the hub, replacing it with more generic
// methods for interacting with Charm code. Hence, no efforts to keep it DRY and minimal efforts at
// error-handling.

import (
	"database/sql"
	"errors"
	"fmt"
	"os"
	"sort"

	"golang.org/x/crypto/bcrypt"

	"pipefish/source/text"

	// SQL drivers

	_ "github.com/go-sql-driver/mysql"  // MariaDB & MySQL
	_ "github.com/lib/pq"               // Postgres
	_ "github.com/nakagami/firebirdsql" // Firebird
	_ "github.com/sijms/go-ora"         // Oracle
	_ "modernc.org/sqlite"              // SQLite
)

// List of SQL drivers for when I want to import more: https://zchee.github.io/golang-wiki/SQLDrivers/

var (
	drivers = map[string]string{"Firebird SQL": "firebirdsql", "MariaDB": "mysql", "MySQL": "mysql",
		"Oracle": "oracle", "Postgres": "postgres", "SQLite": "sqlite"}
)

func GetdB(driver, host, port, db, user, password string) (*sql.DB, error) {

	connectionString := fmt.Sprintf("host=%v port=%v dbname=%v user=%v password=%v sslmode=disable",
		host, port, db, user, password)

	sqlObj, connectionError := sql.Open(drivers[driver], connectionString)
	if connectionError != nil {
		return nil, connectionError
	}

	err := sqlObj.Ping()

	if err != nil {
		return nil, err
	}

	return sqlObj, nil
}

func GetDriverOptions() string {
	result := "The following SQL drivers are available: \n\n"
	for k, v := range GetSortedDrivers() {
		result = result + fmt.Sprintf("  [%v] %v\n", k, v)
	}
	result = result + "\nPick a number"
	return result
}

func GetSortedDrivers() []string {
	dr := []string{}
	for k := range drivers {
		dr = append(dr, k)
	}
	sort.Strings(dr)
	return dr
}

func AddAdmin(db *sql.DB, username, firstName, lastName, email, password, serviceName, dir string) error {

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
	for _, group := range []string{"Admin", "Guests", "Users"} {
		err = AddUserToGroup(db, username, group, true)
		if err != nil {
			return err
		}
	}

	// This should only ever happen to a hub once. We create the file "user/admin.dat"
	// to prove that it has.
	_, err = os.Create(dir + "user/admin.dat")
	return err
}

func AddUserToGroup(db *sql.DB, username, groupName string, owner bool) error {
	query :=
		`INSERT INTO _GroupMemberships(username, groupName, owner)
	VALUES ($1, $2, $3)`
	_, err := db.Exec(query, username, groupName, owner)
	return err
}

func UnAddUserToGroup(db *sql.DB, username, groupName string) error {
	query :=
		`DELETE FROM _GroupMemberships WHERE username = $1 AND groupName = $2)`
	_, err := db.Exec(query, username, groupName)
	return err
}

func LetGroupUseService(db *sql.DB, groupName, serviceName string) error {
	query :=
		`INSERT INTO _GroupServices(groupName, serviceName)
	VALUES ($1, $2)`
	_, err := db.Exec(query, groupName, serviceName)
	return err
}

func UnLetGroupUseService(db *sql.DB, groupName, serviceName string) error {
	query :=
		`DELETE FROM _GroupServices WHERE groupName = $1 AND serviceName = $2)`
	_, err := db.Exec(query, groupName, serviceName)
	return err
}

func LetUserOwnGroup(db *sql.DB, username, groupName string) error {
	return AddUserToGroup(db, username, groupName, true)
}

func UnLetUserOwnGroup(db *sql.DB, username, groupName string) error {
	result, err := IsUserInGroup(db, username, groupName)
	if err != nil {
		return err
	}
	if !result {
		return nil
	}
	return AddUserToGroup(db, username, groupName, false)
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
	username  string
	groupName string
	owner     bool
}

func GetGroupsOfUser(db *sql.DB, username string, ownGroups bool) (string, error) {
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

	if len(groups) == 0 {
		if ownGroups {
			return "You are not a member of any groups.", nil
		} else {
			return text.Emph(username) + " is not a member of any groups.", nil
		}
	}

	result := "\n"

	ownerGroups := []string{}
	userGroups := []string{}
	for _, v := range groups {
		if v.owner {
			ownerGroups = append(ownerGroups, v.groupName)
		} else {
			userGroups = append(userGroups, v.groupName)
		}
	}
	sort.Strings(ownerGroups)
	if len(ownerGroups) > 0 {
		if ownGroups {
			result = result + "You are an owner of the following groups:\n\n"
		} else {
			result = result + text.Emph(username) + " is an owner of the following groups:\n\n"
		}
		for _, v := range ownerGroups {
			result = result + text.BULLET + v + "\n"
		}
		result = result + "\n"
	}

	sort.Strings(userGroups)
	if len(userGroups) > 0 {
		if ownGroups {
			result = result + "You are an user of the following groups:\n\n"
		} else {
			result = result + text.Emph(username) + " is a user of the following groups:\n\n"
		}
		for _, v := range userGroups {
			result = result + text.BULLET + v + "\n"
		}
		result = result + "\n"
	}

	return result, nil
}

func GetUsersOfGroup(db *sql.DB, groupName string) (string, error) {
	rows, err := db.Query("SELECT * FROM _GroupMemberships WHERE groupName = $1", groupName)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	var users []groupRow

	for rows.Next() {
		var user groupRow
		if err := rows.Scan(&user.username, &user.groupName, &user.owner); err != nil {
			return "", err
		}
		users = append(users, user)
	}

	if len(users) == 0 {
		return text.Emph(groupName) + " has no users.", nil
	}

	result := "\n"

	owners := []string{}
	usersOnly := []string{}
	for _, v := range users {
		if v.owner {
			owners = append(owners, v.username)
		} else {
			usersOnly = append(usersOnly, v.username)
		}
	}

	sort.Strings(owners)
	if len(owners) > 0 {
		result = result + text.Emph(groupName) + " has the following owners:\n\n"
		for _, v := range owners {
			result = result + text.BULLET + v + "\n"
		}
		result = result + "\n"
	}

	sort.Strings(usersOnly)
	if len(usersOnly) > 0 {
		result = result + text.Emph(groupName) + " has the following users:\n\n"
		for _, v := range usersOnly {
			result = result + text.BULLET + v + "\n"
		}
		result = result + "\n"
	}

	return result, nil
}

func GetServicesOfUser(db *sql.DB, username string, ownServices bool) (string, error) {
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

	if len(services) == 0 {
		if ownServices {
			return "\nYou do not have access to any services.\n\n", nil
		} else {
			return text.Emph(username) + " does not have access to any services.\n\n", nil
		}
	}

	result := "\n"

	sort.Strings(services)
	if ownServices {
		result = result + "You have access to the following services:\n\n"
	} else {
		result = result + text.Emph(username) + " has access to the following services:\n\n"
	}
	for _, v := range services {
		if v != "" {
			result = result + text.BULLET + v + "\n"
		}
	}

	return result + "\n", nil
}

func GetUsersOfService(db *sql.DB, serviceName string) (string, error) {
	rows, err := db.Query(
		`SELECT _GroupMemberships.username FROM _GroupServices 
INNER JOIN _GroupMemberships
ON _GroupMemberships.groupName = _GroupServices.groupName
WHERE serviceName = $1`, serviceName)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	var users []string

	for rows.Next() {
		var user string
		if err := rows.Scan(&user); err != nil {
			return "", err
		}
		users = append(users, user)
	}

	if len(users) == 0 {
		return text.Emph(serviceName) + " does not have any users.\n\n", nil
	}

	result := "\n"

	sort.Strings(users)
	result = result + text.Emph(serviceName) + " has the following users:\n\n"
	for _, v := range users {
		if v != "" {
			result = result + text.BULLET + v + "\n"
		}
	}

	return result + "\n", nil
}

func GetServicesOfGroup(db *sql.DB, groupName string) (string, error) {
	rows, err := db.Query("SELECT serviceName FROM _GroupServices WHERE groupName = $1", groupName)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	var services []string

	for rows.Next() {
		var srv string
		if err := rows.Scan(&srv); err != nil {
			return "", err
		}
		services = append(services, srv)
	}

	if len(services) == 0 {
		return text.Emph(groupName) + " has access to no services.", nil
	}

	result := "\n"

	sort.Strings(services)
	result = result + text.Emph(groupName) + " has access to the following services:\n\n"
	for _, v := range services {
		result = result + text.BULLET + v + "\n"
	}
	result = result + "\n"
	return result, nil
}

func GetGroupsOfService(db *sql.DB, serviceName string) (string, error) {
	rows, err := db.Query("SELECT groupName FROM _GroupServices WHERE serviceName = $1", serviceName)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	var groups []string

	for rows.Next() {
		var grp string
		if err := rows.Scan(&grp); err != nil {
			return "", err
		}
		groups = append(groups, grp)
	}

	if len(groups) == 0 {
		return text.Emph(serviceName) + " has no groups that can access it.", nil
	}

	result := "\n"

	sort.Strings(groups)
	result = result + text.Emph(serviceName) + " can be accessed by the following groups:\n\n"
	for _, v := range groups {
		result = result + text.BULLET + v + "\n"
	}
	result = result + "\n"
	return result, nil
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

	if serviceName == "" {
		return true, nil
	}

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
	password    string
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

func encrypt(s string) string {
	result, _ := bcrypt.GenerateFromPassword([]byte(s), bcrypt.DefaultCost)
	return string(result)
}
