package service

import (
	"fmt"

	"github.com/beatmadsen/left-ci/internal/db"
	statemachine "github.com/beatmadsen/left-ci/internal/server/service/state_machine"
)

type Service interface {
	AdvanceSlow(revision string) error
	FailSlow(revision string) error
	AdvanceFast(revision string) error
	FailFast(revision string) error
	State(revision string) (State, error)
	Close() error
}

type State struct {
	Revision string `json:"revision"`
	Fast     string `json:"fast"`
	Slow     string `json:"slow"`
}

func New(db db.Db) Service {
	return &servicePrototype{db: db, advance: advance, fail: fail}
}

type servicePrototype struct {
	db      db.Db
	advance func(state string) (string, error)
	fail    func(state string) (string, error)
}

func (s *servicePrototype) AdvanceSlow(revision string) error {
	state, err := s.db.SlowState(revision)
	if err != nil {
		return fmt.Errorf("failed to get slow state: %w", err)
	}
	newState, err := s.advance(state.State)
	if err != nil {
		return err
	}
	return s.db.UpdateSlowState(revision, newState)
}

func (s *servicePrototype) FailSlow(revision string) error {
	state, err := s.db.SlowState(revision)
	if err != nil {
		return fmt.Errorf("failed to get slow state: %w", err)
	}
	newState, err := s.fail(state.State)
	if err != nil {
		return err
	}
	return s.db.UpdateSlowState(revision, newState)
}

func (s *servicePrototype) AdvanceFast(revision string) error {
	state, err := s.db.FastState(revision)
	if err != nil {
		return fmt.Errorf("failed to get fast state: %w", err)
	}
	newState, err := s.advance(state.State)
	if err != nil {
		return err
	}
	return s.db.UpdateFastState(revision, newState)
}

func (s *servicePrototype) FailFast(revision string) error {
	state, err := s.db.FastState(revision)
	if err != nil {
		return fmt.Errorf("failed to get fast state: %w", err)
	}
	newState, err := s.fail(state.State)
	if err != nil {
		return err
	}
	return s.db.UpdateFastState(revision, newState)
}

func (s *servicePrototype) State(revision string) (State, error) {
	fastState, err := s.db.FastState(revision)
	if err != nil {
		return State{}, fmt.Errorf("failed to get fast state: %w", err)
	}
	slowState, err := s.db.SlowState(revision)
	if err != nil {
		return State{}, fmt.Errorf("failed to get slow state: %w", err)
	}
	return State{Revision: revision, Fast: fastState.State, Slow: slowState.State}, nil
}

func (s *servicePrototype) Close() error {
	return s.db.Close()
}

func advance(state string) (string, error) {
	sm, err := statemachine.NewStateMachine(state)
	if err != nil {
		return "", fmt.Errorf("failed to advance state: %w", err)
	}
	sm.Advance()
	return sm.CurrentState(), nil
}

func fail(state string) (string, error) {
	return state, nil
}
