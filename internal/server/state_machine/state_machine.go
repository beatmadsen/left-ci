package statemachine

import "fmt"

type stateMachine struct {
	currentState state
}

func newStateMachine(state string) (*stateMachine, error) {
	switch state {
	case "new":
		return &stateMachine{currentState: &newState{}}, nil
	case "building":
		return &stateMachine{currentState: &buildingState{}}, nil
	case "testing":
		return &stateMachine{currentState: &testingState{}}, nil
	default:
		return nil, fmt.Errorf("invalid state: %s", state)
	}
}

func (s *stateMachine) CurrentState() string {
	return s.currentState.String()
}

func (s *stateMachine) advance() error {
	state, err := s.currentState.advance()
	if err != nil {
		return err
	}
	s.currentState = state
	return nil
}

func (s *stateMachine) fail(err error) error {
	s.currentState = s.currentState.fail(err)
	return nil
}

type state interface {
	advance() (state, error)
	fail(error) state
	String() string
}

type failedState struct {
	err error
}

func (s *failedState) advance() (state, error) {
	return nil, fmt.Errorf("Cannot advance from failed state: %s", s.err)
}

func (s *failedState) fail(err error) state {
	return s
}

func (s *failedState) String() string {
	return "failed"
}

type newState struct{}

func (s *newState) advance() (state, error) {
	return &buildingState{}, nil
}

func (s *newState) fail(err error) state {
	return &failedState{err: err}
}

func (s *newState) String() string {
	return "new"
}

type buildingState struct{}

func (s *buildingState) advance() (state, error) {
	return &testingState{}, nil
}

func (s *buildingState) fail(err error) state {
	return &failedState{err: err}
}

func (s *buildingState) String() string {
	return "building"
}

type testingState struct{}

func (s *testingState) advance() (state, error) {
	return nil, nil
}

func (s *testingState) fail(err error) state {
	return &failedState{err: err}
}

func (s *testingState) String() string {
	return "testing"
}
