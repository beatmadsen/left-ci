package server

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
