package engine

import (
	"fmt"

	"github.com/beatmadsen/left-ci/internal/server"
)

type Engine interface {
	Execute() error
}

func NewEngine(mode string, listenAndServeFunc server.ListenAndServeFunc) (Engine, error) {
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

func (e *serverEngine) Execute() error {
	return e.listenAndServeFunc(":8080", nil)
}
