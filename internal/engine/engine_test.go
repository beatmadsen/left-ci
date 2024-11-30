package engine

import (
	"net/http"
	"testing"
)

func TestThatUnknownModeReturnsError(t *testing.T) {
	engine, err := NewEngine("unknown", listenAndServeStub)

	if err == nil {
		t.Error("expected an error, got nil")
	}
	if engine != nil {
		t.Error("expected nil engine when error occurs")
	}
}

func TestThatNewEngineReturnsServerEngineWhenModeIsServer(t *testing.T) {
	engine, err := NewEngine("server", listenAndServeStub)

	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}
	if engine == nil {
		t.Error("expected engine to not be nil")
	}
}

func listenAndServeStub(addr string, handler http.Handler) error {
	return nil
}
