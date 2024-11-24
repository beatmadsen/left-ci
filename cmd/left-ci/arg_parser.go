package main

import "fmt"

type argParser struct {
	args []string
}

func newArgParser(args ...string) (*argParser, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("no arguments provided")
	}

	return &argParser{args: args}, nil
}
