package db

import (
	"fmt"
	"os"
	"testing"
)

var dbDirPath string

func createTestDatabaseDirectory() {
	// get tmpdir
	dir, err := os.MkdirTemp("", "left-ci-test")
	if err != nil {
		panic(err)
	}
	dbDirPath = dir
}

func TestMain(m *testing.M) {
	// Setup code here
	createTestDatabaseDirectory()
	fmt.Println("dbDirPath", dbDirPath)

	// Run all tests
	m.Run()

	// Cleanup code here (if needed)
}

func TestCreatingRevisionAndGettingState(t *testing.T) {
	db := New(dbDirPath, "isolation-suffix-1")
	defer db.Close()
	db.CreateRevision("revision-1")
	state, err := db.FastState("revision-1")
	if err != nil {
		t.Error(err)
	}
	if state == nil {
		t.Fatal("Expected state to be non-nil")
	}
	if state.State != "new" {
		t.Error("Expected state to be 'new'")
	}
	if state.Revision != "revision-1" {
		t.Error("Expected revision to be 'revision-1'")
	}
}
