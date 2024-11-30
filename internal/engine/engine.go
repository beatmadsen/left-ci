package engine

import (
	"fmt"

	"github.com/beatmadsen/left-ci/internal/argparser"
	"github.com/beatmadsen/left-ci/internal/server"
)

type Engine interface {
	Execute() error
}

func NewEngine(argParser argparser.ArgParser, listenAndServeFunc server.ListenAndServeFunc) (Engine, error) {
	mode, err := argParser.Mode()
	if err != nil {
		return nil, fmt.Errorf("error determining engine mode: %w", err)
	}
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
