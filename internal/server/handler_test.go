package server

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestAdvanceFast(t *testing.T) {
	// Arrange
	s := &serviceMock{}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("POST", "/revision/abc123/fast/advance", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if s.advanceFastCalled != true {
		t.Errorf("Expected advanceFast to be called, but it was not")
	}
}

type serviceMock struct {
	advanceFastCalled bool
}

func (s *serviceMock) advanceSlow(revision string) error {
	return nil
}

func (s *serviceMock) advanceFast(revision string) error {
	s.advanceFastCalled = true
	return nil
}
