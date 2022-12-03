package gol

import (
	"strings"
	"testing"
)

func TestNDimensionalGameOfLife(t *testing.T) {
	const input = `...###.#
#.#.##..
.##.##..
..##...#
.###.##.
.#..##..
.....###
.####..#`

	const (
		dimensions = 4
		iterations = 6
	)

	space, err := ParseGameState(strings.NewReader(input), dimensions)
	if err != nil {
		t.Fatal(err)
	}

	for i := 0; i < iterations; i++ {
		space = space.NextState()
	}

	if want, got := 1980, space.KeyCount(); want != got {
		t.Errorf("want: %d, got: %d", want, got)
	}
}
