package service

import (
	"fmt"
	"testing"

	"github.com/beatmadsen/left-ci/internal/db"
)

func TestAdvanceSlowWhenDbFails(t *testing.T) {

}

func newService() Service {
	return &servicePrototype{db: &dbStub{}}
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
