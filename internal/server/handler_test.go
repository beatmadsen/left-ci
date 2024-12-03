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
	if s.advanceFastCalledWith != "abc123" {
		t.Errorf("Expected advanceFast to be called with 'abc123', but it was not")
	}
}

func TestAdvanceSlow(t *testing.T) {
	// Arrange
	s := &serviceMock{}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("POST", "/revision/abc123/slow/advance", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if s.advanceSlowCalledWith != "abc123" {
		t.Errorf("Expected advanceSlow to be called with 'abc123', but it was not")
	}
}

type serviceMock struct {
	advanceFastCalledWith string
	advanceSlowCalledWith string
}

func (s *serviceMock) advanceSlow(revision string) error {
	s.advanceSlowCalledWith = revision
	return nil
}

func (s *serviceMock) advanceFast(revision string) error {
	s.advanceFastCalledWith = revision
	return nil
}
