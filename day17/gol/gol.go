package gol

import (
	"bufio"
	"fmt"
	"io"
)

const N int = 4

type Point [N]int

func ParseGameState(r io.Reader) (map[Point]struct{}, error) {
	space := map[Point]struct{}{}
	scanner := bufio.NewScanner(r)
	y := 0
	for scanner.Scan() {
		for x, char := range scanner.Text() {
			if char == '#' {
				coord := Point{}
				coord[0] = x
				coord[1] = y
				for i := 2; i < N; i++ {
					coord[i] = 0
				}
				space[coord] = struct{}{}
			}
		}
		y++
	}

	if err := scanner.Err(); err != nil {
		return map[Point]struct{}{}, fmt.Errorf("scanning input file: %w", err)
	}

	return space, nil
}

type GameOfLife struct {
	shifts  [][]int
	actives map[Point]struct{}
}

func NewGameOfLife(actives map[Point]struct{}, maxNeighborDistance int) *GameOfLife {
	return &GameOfLife{
		shifts:  coordinateShifts(maxNeighborDistance),
		actives: actives,
	}
}

func coordinateShifts(maxNeighborDistance int) [][]int {
	if maxNeighborDistance <= 0 {
		return [][]int{}
	}
	return permuteShifts(N, -maxNeighborDistance, maxNeighborDistance)
}

func permuteShifts(iter, minShift, maxShift int) [][]int {
	if iter == 0 {
		return [][]int{{}}
	}
	permutations := [][]int{}
	for _, perm := range permuteShifts(iter-1, minShift, maxShift) {
		for s := minShift; s <= maxShift; s++ {
			if iter == N {
				allZero := s == 0
				for _, p := range perm {
					if p != 0 {
						allZero = false
					}
				}
				if allZero {
					continue
				}
			}
			permutations = append(permutations, append([]int{s}, perm...))
		}
	}
	return permutations
}

func (g *GameOfLife) ActiveCoordinateCount() int {
	return len(g.actives)
}

func (g *GameOfLife) NextState() {
	newActives := map[Point]struct{}{}
	for coord := range g.areaCoordinates() {
		if g.shouldActivate(coord) {
			newActives[coord] = struct{}{}
		}
	}
	g.actives = newActives
}

func (g *GameOfLife) areaCoordinates() map[Point]struct{} {
	coords := map[Point]struct{}{}
	for coord := range g.actives {
		coords[coord] = struct{}{}
		g.eachNeighbor(coord, func(neighbor Point) bool {
			coords[neighbor] = struct{}{}
			return true
		})
	}
	return coords
}

func (g *GameOfLife) shouldActivate(coord Point) bool {
	var activeNeighborCount int
	g.eachNeighbor(coord, func(neighbor Point) bool {
		if g.isActive(neighbor) {
			activeNeighborCount++
		}
		return activeNeighborCount <= 3
	})
	isActive := g.isActive(coord)
	if (isActive && activeNeighborCount == 2 || activeNeighborCount == 3) || (!isActive && activeNeighborCount == 3) {
		return true
	}
	return false
}

func (g *GameOfLife) isActive(coord Point) bool {
	_, ok := g.actives[coord]
	return ok
}

func (g *GameOfLife) eachNeighbor(coord Point, f func(Point) bool) {
	for _, shifts := range g.shifts {
		neighbor := Point{}
		for j, shift := range shifts {
			neighbor[j] = coord[j] + shift
		}
		if !f(neighbor) {
			return
		}
	}
}
