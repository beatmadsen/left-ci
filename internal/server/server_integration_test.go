package server

import (
	"net/http"
	"net/http/httptest"
	"os"
	"testing"
)

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

	// Run all tests
	m.Run()

	// Cleanup code here (if needed)
}
func TestAdvanceRevisionReturnsOKStatus(t *testing.T) {
	// Arrange
	ts := httptest.NewServer(newHandler(dbDirPath))
	defer ts.Close()

	// Act
	resp, err := http.Post(ts.URL+"/revision/abc123/slow/advance", "application/json", nil)
	if err != nil {
		t.Fatalf("Failed to perform request: %v", err)
	}
	defer resp.Body.Close()

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Errorf("Expected status code %d, got %d", http.StatusOK, resp.StatusCode)
	}
}
