package service

import (
	"fmt"
	"testing"

	"github.com/beatmadsen/left-ci/internal/db"
)

func TestGivenFailingDbItFailsToAdvanceSlow(t *testing.T) {
	db := &dbStub{failing: true}
	service := newService(db)
	err := service.AdvanceSlow("revision-1")
	if err == nil {
		t.Error("Expected error when db fails")
	}
}

func TestGivenSuccessfulDbActionAdvanceUpdatesSlowWithCorrectRevision(t *testing.T) {
	db := &dbMock{}
	service := newService(db)
	err := service.AdvanceSlow("revision-1")
	if err != nil {
		t.Error("Expected no error when db succeeds, but got", err)
	}
	if db.updateSlowStateCalledWithState != "succeeded" {
		t.Error("Expected db to be called with state 'succeeded', but got", db.updateSlowStateCalledWithState)
	}
}

func newService(db db.Db) Service {
	return &servicePrototype{db: db}
}

type dbMock struct {
	updateSlowStateCalledWithState string
	updateFastStateCalledWithState string
}

func (d *dbMock) UpdateSlowState(revision, state string) error {
	d.updateSlowStateCalledWithState = state
	return nil
}

func (d *dbMock) UpdateFastState(revision, state string) error {
	d.updateFastStateCalledWithState = state
	return nil
}

func (d *dbMock) FastState(revision string) (*db.State, error) {
	return &db.State{State: "environment ready", Revision: revision}, nil
}

func (d *dbMock) SlowState(revision string) (*db.State, error) {
	return &db.State{State: "testing", Revision: revision}, nil
}

func (d *dbMock) CreateRevision(revision string) error {
	return nil
}

func (d *dbMock) Close() error {
	return nil
}

type dbStub struct {
	failing bool
}

func (d *dbStub) maybeFail() error {
	if d.failing {
		return fmt.Errorf("failed")
	}
	return nil
}

func (d *dbStub) Close() error {
	return d.maybeFail()
}

func (d *dbStub) CreateRevision(revision string) error {
	return d.maybeFail()
}

func (d *dbStub) FastState(revision string) (*db.State, error) {
	if d.failing {
		return nil, fmt.Errorf("failed")
	}
	return &db.State{State: "environment ready", Revision: revision}, nil
}

func (d *dbStub) SlowState(revision string) (*db.State, error) {
	if d.failing {
		return nil, fmt.Errorf("failed")
	}
	return &db.State{State: "environment ready", Revision: revision}, nil
}

func (d *dbStub) UpdateFastState(revision, state string) error {
	return d.maybeFail()
}

func (d *dbStub) UpdateSlowState(revision, state string) error {
	return d.maybeFail()
}
