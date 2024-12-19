package argparser

import (
	"fmt"
	"strconv"
)

type ArgParser interface {
	Mode() (string, error)
	Port() (uint16, error)
}

type ap struct {
	args []string
}

func (a *ap) Mode() (string, error) {
	if len(a.args) == 0 {
		return "", fmt.Errorf("no mode provided")
	}
	return a.args[0], nil
}

func (a *ap) Port() (uint16, error) {
	if len(a.args) < 2 {
		return 8080, nil
	}
	port, err := strconv.ParseUint(a.args[1], 10, 16)
	if err != nil {
		return 0, err
	}
	return uint16(port), nil
}

func NewArgParser(args ...string) (ArgParser, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("no arguments provided")
	}

	return &ap{args: args}, nil
}
