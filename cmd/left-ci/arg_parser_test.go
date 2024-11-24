package main

import (
	"testing"
)

func TestThatAParserMustHaveArgs(t *testing.T) {
	parser, err := newArgParser()

	if err == nil {
		t.Error("expected an error, got nil")
	}
	if parser != nil {
		t.Error("expected nil engine when error occurs")
	}
}
