package server

import (
	"fmt"
	"log"
	"net/http"
)

type simpleHandler struct{}

func (h *simpleHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusOK)
}

func NewHandler() http.Handler {
	return &simpleHandler{}
}

func RunServer(port uint8) {
	log.Printf("Server starting on port %d", port)
	if err := http.ListenAndServe(fmt.Sprintf(":%d", port), NewHandler()); err != nil {
		log.Fatal(err)
	}
}
