package db

import (
	"database/sql"
	"fmt"
	"os"

	_ "github.com/mattn/go-sqlite3"
)

type Db interface {
	CreateRevision(revision string) error
	FastState(revision string) (*State, error)
	UpdateFastState(revision, state string) error
	SlowState(revision string) (*State, error)
	UpdateSlowState(revision, state string) error
	Close() error
}

type db struct {
	directoryPath   string
	isolationSuffix string
	instance        *sql.DB
}

func New(directoryPath, isolationSuffix string) Db {

	db := &db{directoryPath: directoryPath, isolationSuffix: isolationSuffix}

	db.init()

	return db
}

func (d *db) Close() error {
	return d.instance.Close()
}

func (d *db) CreateRevision(revision string) error {
	tx, err := d.instance.Begin()
	if err != nil {
		return err
	}
	defer tx.Rollback() // Rollback if we return early due to an error

	result, err := tx.Exec(`
        INSERT INTO revisions (sha) VALUES (?)
    `, revision)
	if err != nil {
		return err
	}
	revisionId, err := result.LastInsertId()
	if err != nil {
		return err
	}
	_, err = tx.Exec(`
		INSERT INTO fast_state (revision_id, state) VALUES (?, ?)
	`, revisionId, "new")
	if err != nil {
		return err
	}

	_, err = tx.Exec(`
		INSERT INTO slow_state (revision_id, state) VALUES (?, ?)
	`, revisionId, "new")
	if err != nil {
		return err
	}

	return tx.Commit()
}

func (d *db) FastState(revision string) (*State, error) {
	// query fast_state table for revision by joining with revisions table
	row := d.instance.QueryRow(`
		SELECT state, revisions.sha
		FROM fast_state 
		JOIN revisions ON fast_state.revision_id = revisions.id
		WHERE revisions.sha = ?
	`, revision)

	// parse row into state
	var state State
	err := row.Scan(&state.State, &state.Revision)
	if err != nil {
		return nil, err
	}

	return &state, nil
}

func (d *db) UpdateFastState(revision, state string) error {
	_, err := d.instance.Exec(`
		UPDATE fast_state SET state = ? WHERE revision_id = (SELECT id FROM revisions WHERE sha = ?)
	`, state, revision)
	return err
}

func (d *db) SlowState(revision string) (*State, error) {
	// query slow_state table for revision by joining with revisions table
	row := d.instance.QueryRow(`
		SELECT state, revisions.sha
		FROM slow_state 
		JOIN revisions ON slow_state.revision_id = revisions.id
		WHERE revisions.sha = ?
	`, revision)

	// parse row into state
	var state State
	err := row.Scan(&state.State, &state.Revision)
	if err != nil {
		return nil, err
	}
	return &state, nil
}

func (d *db) UpdateSlowState(revision, state string) error {
	_, err := d.instance.Exec(`
		UPDATE slow_state SET state = ? WHERE revision_id = (SELECT id FROM revisions WHERE sha = ?)
	`, state, revision)
	return err
}

func (d *db) init() {

	filePath := d.directoryPath + "/left-ci-" + d.isolationSuffix + ".db"

	// create directory if it doesn't exist
	if _, err := os.Stat(d.directoryPath); os.IsNotExist(err) {
		os.Mkdir(d.directoryPath, 0755)
	}

	db, err := sql.Open("sqlite3", filePath)
	if err != nil {
		fmt.Println(err)
		return
	}

	d.instance = db

	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS revisions (
			id INTEGER PRIMARY KEY,
			sha TEXT
		);
	`)
	if err != nil {
		fmt.Println(err)
		return
	}

	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS fast_state (
			id INTEGER PRIMARY KEY,
			revision_id INTEGER,
			state TEXT,
			FOREIGN KEY (revision_id) REFERENCES revisions (id)
		);
	`)
	if err != nil {
		fmt.Println(err)
		return
	}

	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS slow_state (
			id INTEGER PRIMARY KEY,
			revision_id INTEGER,
			state TEXT,
			FOREIGN KEY (revision_id) REFERENCES revisions (id)
		);
	`)
	if err != nil {
		fmt.Println(err)
		return
	}

}

type State struct {
	State    string
	Revision string
}
