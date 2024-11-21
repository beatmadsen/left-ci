package main

import (
	"net/http"
	"testing"
)

func TestSomething(t *testing.T) {
	// given that I specify mode "server"
	// Create a strategy that tracks when it's runServerCalled

	mock := &mock{}

	sm := newEngine("server", mock.ListenAndServe)
	// when I run the program
	sm.execute()
	// then the server should start

	// Verify the strategy was invoked
	if !mock.listenAndServeCalled {
		t.Error("Expected strategy to be called but it wasn't")
	}
}

type mock struct {
	listenAndServeCalled bool
}

func (m *mock) ListenAndServe(addr string, handler http.Handler) error {
	m.listenAndServeCalled = true
	return nil
}
