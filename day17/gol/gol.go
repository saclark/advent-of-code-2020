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
	newActives := Trie[int]{}
	for _, coord := range g.areaCoordinates() {
		if g.shouldActivate(coord) {
			newActives.Insert(coord)
		}
	}
	g.actives = newActives
}

func (g *GameOfLife) areaCoordinates() [][]int {
	coords := Trie[int]{}
	for _, coord := range g.actives.Keys() {
		coords.Insert(coord)
		g.eachNeighbor(coord, func(neighbor []int) bool {
			coords.Insert(neighbor)
			return true
		})
	}
	return coords.Keys()
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

func (g *GameOfLife) eachNeighbor(coord []int, f func([]int) bool) {
	for _, shifts := range g.shifts {
		neighbor := make([]int, len(coord))
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

func (t Trie[T]) Keys() [][]T {
	var keys [][]T
	for k, subT := range t {
		subKeys := subT.Keys()
		if len(subKeys) == 0 {
			keys = append(keys, []T{k})
			continue
		}
		for _, subKey := range subKeys {
			subKey = append(subKey[:1], subKey[0:]...)
			subKey[0] = k
			keys = append(keys, subKey)
		}
	}
	return keys
}

func (t Trie[T]) Find(key []T) (suffixes Trie[T], found bool) {
	for _, k := range key {
		subT, exists := t[k]
		if !exists {
			return t, false
		}
		t = subT
	}
	return t, true
}

func (t Trie[T]) Insert(key []T) {
	for _, k := range key {
		subT, exists := t[k]
		if !exists {
			subT = Trie[T]{}
			t[k] = subT
		}
		t = subT
	}
}
