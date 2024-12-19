package engine

import (
	"net/http"
	"testing"
)

type argParserStub struct {
	mode string
	port uint16
}

func (a *argParserStub) Mode() (string, error) {
	return a.mode, nil
}

func (a *argParserStub) Port() (uint16, error) {
	return a.port, nil
}

func TestThatUnknownModeReturnsError(t *testing.T) {
	engine, err := NewEngine(&argParserStub{mode: "unknown"}, listenAndServeStub)

	if err == nil {
		t.Error("expected an error, got nil")
	}
	if engine != nil {
		t.Error("expected nil engine when error occurs")
	}
}

func TestThatNewEngineReturnsServerEngineWhenModeIsServer(t *testing.T) {
	engine, err := NewEngine(&argParserStub{mode: "server"}, listenAndServeStub)

	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}
	if engine == nil {
		t.Error("expected engine to not be nil")
	}
}

func TestGivenServerModeAndPortWhenExecutingItStartsServerOnPort(t *testing.T) {
	mock := &mock{}
	engine, err := NewEngine(&argParserStub{mode: "server", port: 9090}, mock.listenAndServe)

	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}
	if engine == nil {
		t.Error("expected engine to not be nil")
	}

	engine.Execute()

	if mock.addr != ":9090" {
		t.Errorf("expected addr to be :9090, got %s", mock.addr)
	}
}

func listenAndServeStub(addr string, handler http.Handler) error {
	return nil
}

type mock struct {
	addr string
}

func (m *mock) listenAndServe(addr string, handler http.Handler) error {
	m.addr = addr
	return nil
}
