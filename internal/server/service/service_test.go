package service

import (
	"errors"
	"testing"

	"github.com/beatmadsen/left-ci/internal/db"
)

func TestGivenBrokenDbItErrorsWhenAdvancingSlowPipeline(t *testing.T) {
	db := &dbStub{failing: true}
	mock := &mock{}
	service := newService(db, mock)
	err := service.AdvanceSlow("revision-1")
	if err == nil {
		t.Error("Expected error when db fails")
	}
	if mock.advanceCount > 0 {
		t.Errorf("Expected advance to not be called, got %d", mock.advanceCount)
	}
}

func TestGivenWorkingDbItAdvancesSlowPipeline(t *testing.T) {
	db := &dbStub{failing: false}
	mock := &mock{}
	service := newService(db, mock)
	err := service.AdvanceSlow("revision-1")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if mock.advanceCount != 1 {
		t.Errorf("Expected advance to be called once, got %d", mock.advanceCount)
	}
}

func TestGivenBrokenDbItErrorsWhenFailingSlowPipeline(t *testing.T) {
	db := &dbStub{failing: true}
	mock := &mock{}
	service := newService(db, mock)
	err := service.FailSlow("revision-1")
	if err == nil {
		t.Error("Expected error when db fails")
	}
	if mock.advanceCount > 0 {
		t.Errorf("Expected advance to not be called, got %d", mock.advanceCount)
	}
}

func TestGivenWorkingDbItFailsSlowPipeline(t *testing.T) {
	db := &dbStub{failing: false}
	mock := &mock{}
	service := newService(db, mock)
	err := service.FailSlow("revision-1")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if mock.failCount != 1 {
		t.Errorf("Expected fail to be called once, got %d", mock.failCount)
	}
}

func TestGivenBrokenDbItErrorsWhenAdvancingFastPipeline(t *testing.T) {
	db := &dbStub{failing: true}
	mock := &mock{}
	service := newService(db, mock)
	err := service.AdvanceFast("revision-1")
	if err == nil {
		t.Error("Expected error when db fails")
	}
	if mock.advanceCount > 0 {
		t.Errorf("Expected advance to not be called, got %d", mock.advanceCount)
	}
}

func TestGivenWorkingDbItAdvancesFastPipeline(t *testing.T) {
	db := &dbStub{failing: false}
	mock := &mock{}
	service := newService(db, mock)
	err := service.AdvanceFast("revision-1")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if mock.advanceCount != 1 {
		t.Errorf("Expected advance to be called once, got %d", mock.advanceCount)
	}
}

func TestGivenBrokenDbItErrorsWhenFailingFastPipeline(t *testing.T) {
	db := &dbStub{failing: true}
	mock := &mock{}
	service := newService(db, mock)
	err := service.FailFast("revision-1")
	if err == nil {
		t.Error("Expected error when db fails")
	}
	if mock.failCount > 0 {
		t.Errorf("Expected fail to not be called, got %d", mock.failCount)
	}
}

func TestGivenWorkingDbItFailsFastPipeline(t *testing.T) {
	db := &dbStub{failing: false}
	mock := &mock{}
	service := newService(db, mock)
	err := service.FailFast("revision-1")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if mock.failCount != 1 {
		t.Errorf("Expected fail to be called once, got %d", mock.failCount)
	}
}

func TestGivenBrokenDbItErrorsWhenFetchingState(t *testing.T) {
	db := &dbStub{failing: true}
	mock := &mock{}
	service := newService(db, mock)
	_, err := service.State("revision-1")
	if err == nil {
		t.Error("Expected error when db fails")
	}
}

func TestGivenWorkingDbItFetchesState(t *testing.T) {
	db := &dbStub{failing: false}
	mock := &mock{}
	service := newService(db, mock)
	state, err := service.State("revision-1")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if state.Revision != "revision-1" {
		t.Errorf("Expected revision to be 'revision-1', got %s", state.Revision)
	}
}

func TestCloseDelegatesToDb(t *testing.T) {
	db := &dbStub{failing: false}
	mock := &mock{}
	service := newService(db, mock)
	err := service.Close()
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
}

func newService(db db.Db, mock *mock) Service {
	return &servicePrototype{db: db, advance: mock.advance, fail: mock.fail}
}

type mock struct {
	advanceCount int
	failCount    int
}

func (m *mock) advance(state string) (string, error) {
	m.advanceCount++
	return state, nil
}

func (m *mock) fail(state string) (string, error) {
	m.failCount++
	return state, nil
}

type dbStub struct {
	failing bool
}

func (d *dbStub) SlowState(revision string) (*db.State, error) {
	if d.failing {
		return nil, errors.New("failed to get slow state")
	}
	return &db.State{State: "testing", Revision: revision}, nil
}

func (d *dbStub) UpdateSlowState(revision, state string) error {
	return nil
}

func (d *dbStub) Close() error {
	return nil
}

func (d *dbStub) FastState(revision string) (*db.State, error) {
	if d.failing {
		return nil, errors.New("failed to get fast state")
	}
	return &db.State{State: "testing", Revision: revision}, nil
}

func (d *dbStub) UpdateFastState(revision, state string) error {
	return nil
}

func (d *dbStub) CreateRevision(revision string) error {
	return nil
}
