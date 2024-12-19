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
		port, err := argParser.Port()
		if err != nil {
			return nil, fmt.Errorf("error determining port: %w", err)
		}
		return &serverEngine{listenAndServeFunc: listenAndServeFunc, port: port}, nil
	default:
		return nil, fmt.Errorf("unknown mode: %s", mode)
	}
}

type serverEngine struct {
	listenAndServeFunc server.ListenAndServeFunc
	port               uint16
}

func (e *serverEngine) Execute() error {
	return e.listenAndServeFunc(fmt.Sprintf(":%d", e.port), nil)
}
