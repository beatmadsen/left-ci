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

func newHandler() http.Handler {
	return &simpleHandler{}
}

type Server interface {
	Run()
}

type ListenAndServeFunc func(addr string, handler http.Handler) error

type myServer struct {
	handler            http.Handler
	listenAndServeFunc ListenAndServeFunc
	port               uint16
}

func NewServer(port uint16) Server {
	return &myServer{handler: newHandler(), listenAndServeFunc: http.ListenAndServe, port: port}
}

func (s *myServer) Run() {
	log.Printf("Server starting on port %d", s.port)
	if err := s.listenAndServeFunc(fmt.Sprintf(":%d", s.port), s.handler); err != nil {
		log.Fatal(err)
	}
}
