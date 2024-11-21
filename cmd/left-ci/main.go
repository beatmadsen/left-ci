package main

import "github.com/beatmadsen/left-ci/internal/server"

type engine interface {
	execute()
}

func newEngine(mode string, listenAndServeFunc server.ListenAndServeFunc) engine {
	panic("Unknown mode")
}

func main() {

}
