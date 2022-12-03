package gol

import (
	"bufio"
	"fmt"
	"io"
)

func ParseGameState(r io.Reader, dimensions int) (IntTrie, error) {
	if dimensions < 2 {
		panic("dimensions must be >= 2")
	}
	space := IntTrie{}
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
		return IntTrie{}, fmt.Errorf("scanning input file: %w", err)
	}

	return space, nil
}

type GameOfLife struct {
	dimensions int
	shifts     [][]int
	actives    IntTrie
}

func NewGameOfLife(actives IntTrie, dimensions, maxNeighborDistance int) *GameOfLife {
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
	dimensionShifts := make([]int, (maxNeighborDistance*2)+1)
	for i, s := 0, -maxNeighborDistance; s <= maxNeighborDistance; i, s = i+1, s+1 {
		dimensionShifts[i] = s
	}
	return permuteShifts(dimensions, dimensionShifts)
}

func permuteShifts(size int, elems []int) [][]int {
	if size == 0 {
		return [][]int{{}}
	}
	permutations := [][]int{}
	for _, perm := range permuteShifts(size-1, elems) {
		for _, elem := range elems {
			updatedPerm := make([]int, len(perm)+1)
			updatedPerm[0] = elem
			for i, p := range perm {
				updatedPerm[i+1] = p
			}
			permutations = append(permutations, updatedPerm)
		}
	}
	return permutations
}

func (g *GameOfLife) NextState() {
	newActives := IntTrie{}
	for _, coord := range g.areaCoordinates() {
		if g.shouldActivate(coord) {
			newActives.Insert(coord)
		}
	}
	g.actives = newActives
}

func (g *GameOfLife) ActiveCoordinateCount() int {
	return g.actives.KeyCount()
}

func (g *GameOfLife) shouldActivate(coord []int) bool {
	var activeNeighborCount int
	for _, neighbor := range g.getNeighbors(coord) {
		if g.isActive(neighbor) {
			activeNeighborCount++
		}
	}
	isActive := g.isActive(coord)
	if (isActive && activeNeighborCount == 2 || activeNeighborCount == 3) || (!isActive && activeNeighborCount == 3) {
		return true
	}
	return false
}

func (g *GameOfLife) isActive(coord []int) bool {
	found, suffixes := g.actives.Find(coord)
	return found && suffixes.IsEmpty()
}

func (g *GameOfLife) areaCoordinates() [][]int {
	coords := IntTrie{}
	for _, coord := range g.actives.Keys() {
		coords.Insert(coord)
		for _, neighbor := range g.getNeighbors(coord) {
			coords.Insert(neighbor)
		}
	}
	return coords.Keys()
}

func (g *GameOfLife) getNeighbors(coord []int) [][]int {
	var neighbors [][]int
	for _, shifts := range g.shifts {
		if isZeroShift(shifts) {
			continue
		}
		neighbors = append(neighbors, shiftCoordinate(coord, shifts))
	}
	return neighbors
}

func isZeroShift(shifts []int) bool {
	for _, shift := range shifts {
		if shift != 0 {
			return false
		}
	}
	return true
}

func shiftCoordinate(coord, shifts []int) []int {
	if len(coord) != len(shifts) {
		panic("length of offsets must equal length of coord")
	}
	shifted := make([]int, len(coord))
	for i, shift := range shifts {
		shifted[i] = coord[i] + shift
	}
	return shifted
}

type IntTrie map[int]IntTrie

func (t IntTrie) IsEmpty() bool {
	return len(t) == 0
}

func (t IntTrie) KeyCount() int {
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

func (t IntTrie) Keys() [][]int {
	var keys [][]int
	for k, subT := range t {
		subKeys := subT.Keys()
		if len(subKeys) == 0 {
			keys = append(keys, []int{k})
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

func (t IntTrie) Find(key []int) (found bool, suffixes IntTrie) {
	for _, k := range key {
		subT, exists := t[k]
		if !exists {
			return false, t
		}
		t = subT
	}
	return true, t
}

func (t IntTrie) Insert(key []int) {
	for _, k := range key {
		subT, exists := t[k]
		if !exists {
			subT = IntTrie{}
			t[k] = subT
		}
		t = subT
	}
}
