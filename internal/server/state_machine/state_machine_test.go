package statemachine

import "testing"

func TestCreatingMachineForRevision(t *testing.T) {
	machine, err := newStateMachine("new")
	if err != nil {
		t.Error("Expected no error")
	}
	if machine.CurrentState() != "new" {
		t.Error("Expected current state to be 'new'")
	}
}

func TestAdvancingNewState(t *testing.T) {
	machine, err := newStateMachine("new")
	if err != nil {
		t.Error("Expected no error")
	}
	machine.advance()
	if machine.CurrentState() != "building" {
		t.Error("Expected current state to be 'building'")
	}
}

func TestAdvancingBuildingState(t *testing.T) {
	machine, err := newStateMachine("building")
	if err != nil {
		t.Error("Expected no error")
	}
	machine.advance()
	if machine.CurrentState() != "testing" {
		t.Error("Expected current state to be 'testing'")
	}
}
