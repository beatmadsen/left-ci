package server

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestAdvanceRevision(t *testing.T) {
	srv := NewServer()

	payload := `{"next_state": "something"}`
	req := httptest.NewRequest("POST", "/revision/abc123/advance", strings.NewReader(payload))
	w := httptest.NewRecorder()

	srv.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status code %d, got %d", http.StatusOK, w.Code)
	}
}
