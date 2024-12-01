package server

import "net/http"

type simpleHandler struct {
	service service
}

func (h *simpleHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusOK)
}

func newHandler() http.Handler {
	return &simpleHandler{
		service: &serviceStub{},
	}
}

type service interface {
	advanceSlow(revision string) error
	advanceFast(revision string) error
}

type serviceStub struct{}

func (s *serviceStub) advanceSlow(revision string) error {
	return nil
}

func (s *serviceStub) advanceFast(revision string) error {
	return nil
}
