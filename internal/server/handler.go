package server

import (
	"encoding/json"
	"fmt"
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

	var myErr error

	if matches(path, "/revision/", "/fast/advance") {
		revision := extractRevision(path, "/revision/", "/fast/advance")
		myErr = h.service.advanceFast(revision)
	} else if matches(path, "/revision/", "/slow/advance") {
		revision := extractRevision(path, "/revision/", "/slow/advance")
		myErr = h.service.advanceSlow(revision)
	} else if r.Method == http.MethodGet {
		revision := extractRevision(path, "/revision/", "")
		state, err := h.service.state(revision)
		myErr = err
		if err == nil {
			json.NewEncoder(w).Encode(state)
		}
	} else {
		myErr = fmt.Errorf("unknown path: %s", path)
	}

	if myErr != nil {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte(myErr.Error()))
	} else {
		w.WriteHeader(http.StatusOK)
	}

}

func newHandler() http.Handler {
	return &simpleHandler{
		service: &servicePrototype{},
	}
}

type service interface {
	advanceSlow(revision string) error
	advanceFast(revision string) error
	state(revision string) (state, error)
}

type state struct {
	Revision string `json:"revision"`
	Fast     uint8  `json:"fast"`
	Slow     uint8  `json:"slow"`
}

type servicePrototype struct{}

func (s *servicePrototype) advanceSlow(revision string) error {
	return nil
}

func (s *servicePrototype) advanceFast(revision string) error {
	return nil
}

func (s *servicePrototype) state(revision string) (state, error) {
	return state{}, nil
}
