package main

import (
	"net/http"
	"testing"

	"github.com/beatmadsen/left-ci/internal/engine"
)

func TestServerStartsWhenModeIsServer(t *testing.T) {
	// given that I specify mode "server"
	// Create a strategy that tracks when it's runServerCalled

	mock := &mock{}

	sm, err := engine.NewEngine("server", mock.ListenAndServe)
	if err != nil {
		t.Error("Expected no error but got", err)
	}
	// when I run the program
	err = sm.Execute()
	if err != nil {
		t.Error("Expected no error but got", err)
	}
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
