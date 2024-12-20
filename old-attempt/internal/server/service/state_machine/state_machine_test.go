package statemachine

import (
	"errors"
	"testing"
)

func TestCreatingMachineForRevision(t *testing.T) {
	machine, err := NewStateMachine("new")
	if err != nil {
		t.Fatal("Expected no error")
	}
	if machine.CurrentState() != "new" {
		t.Error("Expected current state to be 'new'")
	}
}

func TestAdvancingNewState(t *testing.T) {
	machine, err := NewStateMachine("new")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.Advance()
	if machine.CurrentState() != "environment ready" {
		t.Error("Expected current state to be 'environment ready'")
	}
}

func TestAdvancingEnvironmentReadyState(t *testing.T) {
	machine, err := NewStateMachine("environment ready")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.Advance()
	if machine.CurrentState() != "building" {
		t.Error("Expected current state to be 'building'")
	}
}

func TestAdvancingBuildingState(t *testing.T) {
	machine, err := NewStateMachine("building")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.Advance()
	if machine.CurrentState() != "testing" {
		t.Error("Expected current state to be 'testing'")
	}
}

func TestAdvancingTestingState(t *testing.T) {
	machine, err := NewStateMachine("testing")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.Advance()
	if machine.CurrentState() != "succeeded" {
		t.Error("Expected current state to be 'succeeded'")
	}
}

func TestFailingNewState(t *testing.T) {
	machine, err := NewStateMachine("new")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.fail(errors.New("test error"))
	if machine.CurrentState() != "failed" {
		t.Error("Expected current state to be 'failed'")
	}
}
func TestFailingEnvironmentReadyState(t *testing.T) {
	machine, err := NewStateMachine("environment ready")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.fail(errors.New("test error"))
	if machine.CurrentState() != "failed" {
		t.Error("Expected current state to be 'failed'")
	}
}

func TestFailingBuildingState(t *testing.T) {
	machine, err := NewStateMachine("building")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.fail(errors.New("test error"))
	if machine.CurrentState() != "failed" {
		t.Error("Expected current state to be 'failed'")
	}
}

func TestFailingTestingState(t *testing.T) {
	machine, err := NewStateMachine("testing")
	if err != nil {
		t.Fatal("Expected no error")
	}
	machine.fail(errors.New("test error"))
	if machine.CurrentState() != "failed" {
		t.Error("Expected current state to be 'failed'")
	}
}
