package main

import (
	"net/http"
	"testing"
)

func TestServerStartsWhenModeIsServer(t *testing.T) {
	// given that I specify mode "server"
	// Create a strategy that tracks when it's runServerCalled

	mock := &mock{}

	sm, err := newEngine("server", mock.ListenAndServe)
	if err != nil {
		t.Error("Expected no error but got", err)
	}
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
