package service

import "testing"

func TestAdvanceSlowWhenDbFails(t *testing.T) {

}

type dbStub struct {
	failing bool
}
