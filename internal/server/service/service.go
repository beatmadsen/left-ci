package service

type Service interface {
	AdvanceSlow(revision string) error
	AdvanceFast(revision string) error
	State(revision string) (State, error)
}

type State struct {
	Revision string `json:"revision"`
	Fast     uint8  `json:"fast"`
	Slow     uint8  `json:"slow"`
}

func New() Service {
	return &servicePrototype{}
}

type servicePrototype struct{}

func (s *servicePrototype) AdvanceSlow(revision string) error {
	return nil
}

func (s *servicePrototype) AdvanceFast(revision string) error {
	return nil
}

func (s *servicePrototype) State(revision string) (State, error) {
	return State{}, nil
}
