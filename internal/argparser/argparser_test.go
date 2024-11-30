package argparser

import (
	"testing"
)

func TestThatAParserMustHaveArgs(t *testing.T) {
	parser, err := NewArgParser()

	if err == nil {
		t.Error("expected an error, got nil")
	}
	if parser != nil {
		t.Error("expected nil parser when error occurs")
	}
}

func TestThatModeReturnsTheFirstArgument(t *testing.T) {
	parser, err := NewArgParser("server")

	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}

	mode, err := parser.Mode()
	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}
	if mode != "server" {
		t.Errorf("expected mode to be server, got %v", mode)
	}
}

func TestThatPortReturnsTheSecondArgument(t *testing.T) {
	parser, err := NewArgParser("server", "8080")

	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}

	port, err := parser.Port()
	if err != nil {
		t.Errorf("expected no error, got %v", err)
	}
	if port != 8080 {
		t.Errorf("expected port to be 8080, got %v", port)
	}
}
