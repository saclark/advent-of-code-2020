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
		dimensions          = 4
		maxNeighborDistance = 1
		iterations          = 6
	)

	initActives, err := ParseGameState(strings.NewReader(input), dimensions)
	if err != nil {
		t.Fatal(err)
	}

	game := NewGameOfLife(initActives, dimensions, maxNeighborDistance)
	for i := 0; i < iterations; i++ {
		game.NextState()
	}

	if want, got := 1980, game.ActiveCoordinateCount(); want != got {
		t.Errorf("want: %d, got: %d", want, got)
	}
}
