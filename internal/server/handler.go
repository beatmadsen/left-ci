package server

import (
	"net/http"
	"strings"
)

type simpleHandler struct {
	service service
}

func matches(path string, prefix string, suffix string) bool {
	return strings.HasPrefix(path, prefix) && strings.HasSuffix(path, suffix)
}

func extractRevision(path string, prefix string, suffix string) string {
	return strings.TrimSuffix(strings.TrimPrefix(path, prefix), suffix)
}

func (h *simpleHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {

	path := r.URL.Path
	if matches(path, "/revision/", "/fast/advance") {
		revision := extractRevision(path, "/revision/", "/fast/advance")
		h.service.advanceFast(revision)
	}
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
