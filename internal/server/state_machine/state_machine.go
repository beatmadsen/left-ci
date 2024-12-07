package statemachine

import "fmt"

type stateMachine struct {
	currentState state
}

func newStateMachine(initialState string) (*stateMachine, error) {
	switch initialState {
	case "new":
		return &stateMachine{currentState: &newState{}}, nil
	case "environment ready":
		return &stateMachine{currentState: &environmentReadyState{}}, nil
	case "building":
		return &stateMachine{currentState: &buildingState{}}, nil
	case "testing":
		return &stateMachine{currentState: &testingState{}}, nil
	default:
		return nil, fmt.Errorf("invalid state: %s", initialState)
	}
}

func (s *stateMachine) CurrentState() string {
	return s.currentState.String()
}

func (s *stateMachine) advance() {
	state := s.currentState.advance()

	s.currentState = state
}

func (s *stateMachine) fail(err error) {
	s.currentState = s.currentState.fail(err)
}

type state interface {
	advance() state
	fail(error) state
	String() string
}

type failedState struct {
	err error
}

func (s *failedState) advance() state {
	return s
}

func (s *failedState) fail(err error) state {
	return s
}

func (s *failedState) String() string {
	return "failed"
}

type newState struct{}

func (s *newState) advance() state {
	return &environmentReadyState{}
}

func (s *newState) fail(err error) state {
	return &failedState{err: err}
}

func (s *newState) String() string {
	return "new"
}

type environmentReadyState struct{}

func (s *environmentReadyState) advance() state {
	return &buildingState{}
}

func (s *environmentReadyState) fail(err error) state {
	return &failedState{err: err}
}

func (s *environmentReadyState) String() string {
	return "environment ready"
}

type buildingState struct{}

func (s *buildingState) advance() state {
	return &testingState{}
}

func (s *buildingState) fail(err error) state {
	return &failedState{err: err}
}

func (s *buildingState) String() string {
	return "building"
}

type testingState struct{}

func (s *testingState) advance() state {
	return &succeededState{}
}

func (s *testingState) fail(err error) state {
	return &failedState{err: err}
}

func (s *testingState) String() string {
	return "testing"
}

type succeededState struct{}

func (s *succeededState) advance() state {
	return s
}

func (s *succeededState) fail(err error) state {
	return s
}

func (s *succeededState) String() string {
	return "succeeded"
}
