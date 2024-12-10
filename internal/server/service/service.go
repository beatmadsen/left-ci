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
	Fast     uint8  `json:"fast"`
	Slow     uint8  `json:"slow"`
}

func New(db db.Db) Service {
	return &servicePrototype{db: db}
}

type servicePrototype struct {
	db db.Db
}

func (s *servicePrototype) AdvanceSlow(revision string) error {
	state, err := s.db.SlowState(revision)
	if err != nil {
		return fmt.Errorf("failed to get slow state: %w", err)
	}
	sm, err := statemachine.NewStateMachine(state.State)
	if err != nil {
		return fmt.Errorf("failed to advance slow state: %w", err)
	}
	sm.Advance()

	return s.db.UpdateSlowState(revision, sm.CurrentState())
}

func (s *servicePrototype) FailSlow(revision string) error {
	return nil
}

func (s *servicePrototype) AdvanceFast(revision string) error {
	return nil
}

func (s *servicePrototype) FailFast(revision string) error {
	return nil
}

func (s *servicePrototype) State(revision string) (State, error) {
	return State{}, nil
}

func (s *servicePrototype) Close() error {
	return s.db.Close()
}
