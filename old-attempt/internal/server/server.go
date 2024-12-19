package server

import (
	"fmt"
	"log"
	"net/http"
	"os"
)

type Server interface {
	Run()
}

type ListenAndServeFunc func(addr string, handler http.Handler) error

type myServer struct {
	handler            http.Handler
	listenAndServeFunc ListenAndServeFunc
	port               uint16
}

var dbDirPath string

func createDatabaseDirectory() {
	// get tmpdir
	dir, err := os.MkdirTemp("", "left-ci")
	if err != nil {
		panic(err)
	}
	dbDirPath = dir
}

func NewServer(port uint16) Server {
	createDatabaseDirectory()
	return &myServer{handler: newHandler(dbDirPath), listenAndServeFunc: http.ListenAndServe, port: port}
}

func (s *myServer) Run() {
	log.Printf("Server starting on port %d", s.port)
	if err := s.listenAndServeFunc(fmt.Sprintf(":%d", s.port), s.handler); err != nil {
		log.Fatal(err)
	}
}
