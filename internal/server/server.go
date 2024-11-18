package server

import "net/http"

type fakeHandler struct{}

func (h *fakeHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusOK)
}

func NewServer() http.Handler {
	return &fakeHandler{}
}
