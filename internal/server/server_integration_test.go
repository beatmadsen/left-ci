package server

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestAdvanceRevisionReturnsOKStatus(t *testing.T) {
	// Arrange
	ts := httptest.NewServer(newHandler())
	defer ts.Close()

	payload := `{"next_state": "something"}`

	// Act
	resp, err := http.Post(ts.URL+"/revision/abc123/advance", "application/json", strings.NewReader(payload))
	if err != nil {
		t.Fatalf("Failed to perform request: %v", err)
	}
	defer resp.Body.Close()

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Errorf("Expected status code %d, got %d", http.StatusOK, resp.StatusCode)
	}
}
