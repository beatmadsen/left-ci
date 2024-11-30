package main

import (
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"

	"github.com/beatmadsen/left-ci/internal/argparser"
	"github.com/beatmadsen/left-ci/internal/engine"
	"github.com/beatmadsen/left-ci/internal/server"
)

func main() {
	// Setup signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)
	args := os.Args[1:]
	run(args, http.ListenAndServe, sigChan, os.Exit)
}

func run(args []string, listenAndServeFunc server.ListenAndServeFunc, sigChan chan os.Signal, exitFunc func(int)) {
	engine, err := newEngine(listenAndServeFunc, args...)
	if err != nil {
		fmt.Println("Error creating engine:", err)
		exitFunc(1)
		return
	}

	// Run engine in goroutine
	errChan := make(chan error, 1)
	go func() {
		errChan <- engine.Execute()
	}()

	// Wait for either error or interrupt
	select {
	case err := <-errChan:
		if err != nil {
			fmt.Println("Error running engine:", err)
			exitFunc(1)
			return
		}
	case <-sigChan:
		fmt.Println("\nLeft CI shutting down.")
		exitFunc(0)
		return
	}
}

func newEngine(listenAndServeFunc server.ListenAndServeFunc, args ...string) (engine.Engine, error) {
	argParser, err := argparser.NewArgParser(args...)
	if err != nil {
		return nil, fmt.Errorf("error creating arg parser: %w", err)
	}
	return engine.NewEngine(argParser, listenAndServeFunc)
}
