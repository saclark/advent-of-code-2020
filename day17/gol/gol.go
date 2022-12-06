package gol

import (
	"bufio"
	"fmt"
	"io"
)

func ParseGameState(r io.Reader, dimensions int) (Trie[int], error) {
	if dimensions < 2 {
		panic("dimensions must be >= 2")
	}
	space := Trie[int]{}
	scanner := bufio.NewScanner(r)
	y := 0
	for scanner.Scan() {
		for x, char := range scanner.Text() {
			if char == '#' {
				coord := make([]int, dimensions)
				coord[0] = x
				coord[1] = y
				for i := 2; i < dimensions; i++ {
					coord[i] = 0
				}
				space.Insert(coord)
			}
		}
		y++
	}

	if err := scanner.Err(); err != nil {
		return Trie[int]{}, fmt.Errorf("scanning input file: %w", err)
	}

	return space, nil
}

type GameOfLife struct {
	dimensions int
	shifts     [][]int
	actives    Trie[int]
}

func NewGameOfLife(actives Trie[int], dimensions, maxNeighborDistance int) *GameOfLife {
	if dimensions < 2 {
		panic("dimensions must be >= 2")
	}
	return &GameOfLife{
		dimensions: dimensions,
		shifts:     coordinateShifts(dimensions, maxNeighborDistance),
		actives:    actives,
	}
}

func coordinateShifts(dimensions, maxNeighborDistance int) [][]int {
	if maxNeighborDistance <= 0 {
		return [][]int{}
	}
	return permuteShifts(dimensions, -maxNeighborDistance, maxNeighborDistance, dimensions)
}

func permuteShifts(dimensions, minShift, maxShift, iter int) [][]int {
	if iter == 0 {
		return [][]int{{}}
	}
	permutations := [][]int{}
	for _, perm := range permuteShifts(dimensions, minShift, maxShift, iter-1) {
		for s := minShift; s <= maxShift; s++ {
			if iter == dimensions {
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
	return g.actives.KeyCount()
}

func (g *GameOfLife) NextState() {
	space := Trie[int]{}
	g.actives.EachKeyAtDepth(g.dimensions, func(coord []int) bool {
		space.Insert(coord)
		g.eachNeighbor(coord, func(neighbor []int) bool {
			space.Insert(neighbor)
			return true
		})
		return true
	})
	newActives := Trie[int]{}
	space.EachKeyAtDepth(g.dimensions, func(coord []int) bool {
		if g.shouldActivate(coord) {
			newActives.Insert(coord)
		}
		return true
	})
	g.actives = newActives
}

func (g *GameOfLife) shouldActivate(coord []int) bool {
	var activeNeighborCount int
	g.eachNeighbor(coord, func(neighbor []int) bool {
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

func (g *GameOfLife) isActive(coord []int) bool {
	suffixes, ok := g.actives.Find(coord)
	return ok && suffixes.IsEmpty()
}

// Assumes len(coord) == len(g.shifts[i]) == g.dimensions
func (g *GameOfLife) eachNeighbor(coord []int, f func([]int) bool) {
	// Could also store this on g.neighbor and allocate this once up front.
	neighbor := make([]int, len(coord))
	for _, shifts := range g.shifts {
		for j, shift := range shifts {
			neighbor[j] = coord[j] + shift
		}
		if !f(neighbor) {
			return
		}
	}
}

type Trie[T comparable] map[T]Trie[T]

func (t Trie[T]) IsEmpty() bool {
	return len(t) == 0
}

func (t Trie[T]) KeyCount() int {
	var count int
	for _, v := range t {
		subCount := v.KeyCount()
		if subCount == 0 {
			count++
		} else {
			count += subCount
		}
	}
	return count
}

func (t Trie[T]) EachKeyAtDepth(depth int, f func([]T) bool) {
	key := make([]T, depth)
	t.walkKeys(0, key, f)
}

func (t Trie[T]) walkKeys(level int, key []T, f func([]T) bool) bool {
	if level >= cap(key) {
		return f(key)
	}
	for k, subT := range t {
		key[level] = k
		if !subT.walkKeys(level+1, key, f) {
			return false
		}
	}
	return true
}

func (t Trie[T]) Find(key []T) (suffixes Trie[T], found bool) {
	for _, k := range key {
		subT, ok := t[k]
		if !ok {
			return t, false
		}
		t = subT
	}
	return t, true
}

func (t Trie[T]) Insert(key []T) {
	for _, k := range key {
		subT, ok := t[k]
		if !ok {
			subT = Trie[T]{}
			t[k] = subT
		}
		t = subT
	}
}
