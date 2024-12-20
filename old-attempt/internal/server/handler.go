package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"

	"github.com/beatmadsen/left-ci/internal/db"
	svc "github.com/beatmadsen/left-ci/internal/server/service"
)

type simpleHandler struct {
	service svc.Service
}

func matches(path string, prefix string, suffix string) bool {
	return strings.HasPrefix(path, prefix) && strings.HasSuffix(path, suffix)
}

func extractRevision(path string, prefix string, suffix string) string {
	return strings.TrimSuffix(strings.TrimPrefix(path, prefix), suffix)
}

func (h *simpleHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	state, clientErr, serverErr := h.routeToService(r.URL.Path, r.Method)

	if clientErr != nil {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte(clientErr.Error()))
	} else if serverErr != nil {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte(serverErr.Error()))
	} else {
		w.WriteHeader(http.StatusOK)
		if state != nil {
			json.NewEncoder(w).Encode(state)
		}
	}
}

// returns state, client error, server error
func (h *simpleHandler) routeToService(path string, method string) (*svc.State, error, error) {

	if matches(path, "/revision/", "/fast/advance") {
		revision := extractRevision(path, "/revision/", "/fast/advance")
		err := h.service.AdvanceFast(revision)
		return nil, nil, err
	} else if matches(path, "/revision/", "/slow/advance") {
		revision := extractRevision(path, "/revision/", "/slow/advance")
		err := h.service.AdvanceSlow(revision)
		return nil, nil, err
	} else if method == http.MethodGet {
		revision := extractRevision(path, "/revision/", "")
		state, err := h.service.State(revision)
		return &state, nil, err
	} else {
		return nil, fmt.Errorf("unknown path: %s", path), nil
	}
}

func newHandler(dbDirPath string) http.Handler {
	db := db.New(dbDirPath, "handler")
	return &simpleHandler{
		service: svc.New(db),
	}
}
