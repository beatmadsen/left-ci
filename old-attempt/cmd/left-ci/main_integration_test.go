package main

import (
	"errors"
	"fmt"
	"net/http"
	"os"
	"testing"
)

func TestServerStartsWhenModeIsServer(t *testing.T) {
	// given that I specify mode "server"
	// Create a strategy that tracks when it's runServerCalled

	mock := &mock{}

	// when I run the program
	run([]string{"server"}, mock.ListenAndServe, make(chan os.Signal), mock.Exit)

	// then the server should start

	// Verify the strategy was invoked
	if !mock.listenAndServeCalled {
		t.Error("Expected strategy to be called but it wasn't")
	}

	// and the exit code should be 0
	if mock.exitCode != 0 {
		t.Error("Expected exit code to be 0 but got", mock.exitCode)
	}
}

func TestServerExitsWhenSigIntIsSent(t *testing.T) {
	mock := &mock{}
	blockingStub := &blockingStub{}

	sigChan := make(chan os.Signal, 1)

	go func() {
		// will block until engine errors or SIGINT is sent
		run([]string{"server"}, blockingStub.ListenAndServe, sigChan, mock.Exit)
	}()

	// when we send a SIGINT signal
	sigChan <- os.Interrupt
	// then the server should exit normally
	if mock.exitCode != 0 {
		t.Error("Expected exit code to be 0 but got", mock.exitCode)
	}
}

func TestServerExitsWhenEngineCreationErrors(t *testing.T) {
	mock := &mock{}
	sigChan := make(chan os.Signal, 1)

	// when we run the program with an unknown mode
	run([]string{"unknown"}, mock.ListenAndServe, sigChan, mock.Exit)
	// then the server should exit abnormally
	if mock.exitCode != 1 {
		t.Error("Expected exit code to be 1 but got", mock.exitCode)
	}
}

func TestServerExitsWhenEngineExecutionErrors(t *testing.T) {
	mock := &mock{}
	failingStub := &failingStub{}
	sigChan := make(chan os.Signal, 1)

	run([]string{"server"}, failingStub.ListenAndServe, sigChan, mock.Exit)

	// then the server should exit abnormally
	if mock.exitCode != 1 {
		t.Error("Expected exit code to be 1 but got", mock.exitCode)
	}
}

type blockingStub struct {
}

func (s *blockingStub) ListenAndServe(addr string, handler http.Handler) error {
	fmt.Println("ListenAndServe method was invoked")
	select {}
}

type failingStub struct {
}

func (s *failingStub) ListenAndServe(addr string, handler http.Handler) error {
	return errors.New("failed")
}

type mock struct {
	listenAndServeCalled bool
	exitCode             int
}

func (m *mock) ListenAndServe(addr string, handler http.Handler) error {
	m.listenAndServeCalled = true
	return nil
}

func (m *mock) Exit(code int) {
	m.exitCode = code
}
