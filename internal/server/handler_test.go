package server

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	svc "github.com/beatmadsen/left-ci/internal/server/service"
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

func TestFailingAdvanceFastReturns500(t *testing.T) {
	// Arrange
	s := &serviceStub{failing: true}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("POST", "/revision/abc123/fast/advance", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if w.Code != http.StatusInternalServerError {
		t.Errorf("Expected status code to be 500, but it was %d", w.Code)
	}
}

func TestFailingAdvanceSlowReturns500(t *testing.T) {
	// Arrange
	s := &serviceStub{failing: true}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("POST", "/revision/abc123/slow/advance", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if w.Code != http.StatusInternalServerError {
		t.Errorf("Expected status code to be 500, but it was %d", w.Code)
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

func TestStateIsCalledWithRevision(t *testing.T) {
	// Arrange
	s := &serviceMock{}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("GET", "/revision/abc123", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if s.stateCalledWith != "abc123" {
		t.Errorf("Expected state to be called with 'abc123', but it was not")
	}
}

func TestStateReturnsState(t *testing.T) {
	// Arrange
	s := &serviceStub{}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("GET", "/revision/abc123", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	expected := `{"revision":"abc123","fast":2,"slow":5}` + "\n"
	if w.Body.String() != expected {
		t.Errorf("Unexpected response body: %s ; expected %s", w.Body.String(), expected)
	}
}

func TestFailingStateLookupReturns500(t *testing.T) {
	// Arrange
	s := &serviceStub{failing: true}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("GET", "/revision/abc123", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if w.Code != http.StatusInternalServerError {
		t.Errorf("Expected status code to be 500, but it was %d", w.Code)
	}
}

func TestUnknownPathReturns400(t *testing.T) {
	// Arrange
	s := &serviceStub{}
	h := &simpleHandler{service: s}
	req, err := http.NewRequest("POST", "/unknown", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()

	// Act
	h.ServeHTTP(w, req)

	// Assert
	if w.Code != http.StatusBadRequest {
		t.Errorf("Expected status code to be 500, but it was %d", w.Code)
	}
}

type serviceStub struct {
	failing bool
}

func (s *serviceStub) AdvanceSlow(revision string) error {
	if s.failing {
		return fmt.Errorf("failed")
	}
	return nil
}

func (s *serviceStub) FailSlow(revision string) error {
	if s.failing {
		return fmt.Errorf("failed")
	}
	return nil
}

func (s *serviceStub) AdvanceFast(revision string) error {
	if s.failing {
		return fmt.Errorf("failed")
	}
	return nil
}

func (s *serviceStub) FailFast(revision string) error {
	if s.failing {
		return fmt.Errorf("failed")
	}
	return nil
}

func (s *serviceStub) State(revision string) (svc.State, error) {
	if s.failing {
		return svc.State{}, fmt.Errorf("failed")
	}
	return svc.State{Revision: revision, Fast: 2, Slow: 5}, nil
}

func (s *serviceStub) Close() error {
	return nil
}

type serviceMock struct {
	advanceFastCalledWith string
	failFastCalledWith    string
	advanceSlowCalledWith string
	failSlowCalledWith    string
	stateCalledWith       string
}

func (s *serviceMock) AdvanceSlow(revision string) error {
	s.advanceSlowCalledWith = revision
	return nil
}

func (s *serviceMock) FailSlow(revision string) error {
	s.failSlowCalledWith = revision
	return nil
}

func (s *serviceMock) AdvanceFast(revision string) error {
	s.advanceFastCalledWith = revision
	return nil
}

func (s *serviceMock) FailFast(revision string) error {
	s.failFastCalledWith = revision
	return nil
}

func (s *serviceMock) State(revision string) (svc.State, error) {
	s.stateCalledWith = revision
	return svc.State{Revision: revision, Fast: 0, Slow: 0}, nil
}

func (s *serviceMock) Close() error {
	return nil
}
