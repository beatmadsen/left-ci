package main

import (
	"fmt"

	"github.com/beatmadsen/left-ci/internal/server"
)

type engine interface {
	execute() error
}

func newEngine(mode string, listenAndServeFunc server.ListenAndServeFunc) (engine, error) {
	switch mode {
	case "server":
		return &serverEngine{listenAndServeFunc: listenAndServeFunc}, nil
	default:
		return nil, fmt.Errorf("unknown mode: %s", mode)
	}
}

type serverEngine struct {
	listenAndServeFunc server.ListenAndServeFunc
}

func (e *serverEngine) execute() error {
	return e.listenAndServeFunc(":8080", nil)
}

func main() {

}
