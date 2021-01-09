package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	dimensions := 4
	iterations := 6
	space, err := parseInput("input.txt", dimensions)
	if err != nil {
		log.Fatal(err)
	}
	for i := 0; i < iterations; i++ {
		space = space.NextState()
	}
	fmt.Println(space.KeyCount())
}

func parseInput(inputFile string, dimensions int) (ActiveSpace, error) {
	if dimensions < 2 {
		panic("dimensions must be >= 2")
	}

	file, err := os.Open(inputFile)
	if err != nil {
		return ActiveSpace{}, fmt.Errorf("opening input file: %w", err)
	}
	defer file.Close()

	space := ActiveSpace{}
	scanner := bufio.NewScanner(file)
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
		return ActiveSpace{}, fmt.Errorf("scanning input file: %w", err)
	}

	return space, nil
}

type ActiveSpace = IntTrie

func (s ActiveSpace) NextState() ActiveSpace {
	newSpace := ActiveSpace{}
	for _, coord := range s.areaCoordinates() {
		if s.shouldActivate(coord) {
			newSpace.Insert(coord)
		}
	}
	return newSpace
}

func (s ActiveSpace) shouldActivate(coord []int) bool {
	var activeNeighborCount int
	for _, neighbor := range getNeighbors(coord) {
		if s.isActive(neighbor) {
			activeNeighborCount++
		}
	}
	isActive := s.isActive(coord)
	if (isActive && activeNeighborCount == 2 || activeNeighborCount == 3) || (!isActive && activeNeighborCount == 3) {
		return true
	}
	return false
}

func (s ActiveSpace) isActive(coord []int) bool {
	found, suffixes := s.Find(coord)
	return found && suffixes.IsEmpty()
}

func (s ActiveSpace) areaCoordinates() [][]int {
	coords := IntTrie{}
	for _, coord := range s.Keys() {
		coords.Insert(coord)
		for _, neighbor := range getNeighbors(coord) {
			coords.Insert(neighbor)
		}
	}
	return coords.Keys()
}

func getNeighbors(coord []int) [][]int {
	isZeroShift := func(shifts []int) bool {
		for _, shift := range shifts {
			if shift != 0 {
				return false
			}
		}
		return true
	}

	var neighbors [][]int
	for _, shifts := range permute(len(coord), []int{-1, 0, 1}) {
		if isZeroShift(shifts) {
			continue
		}
		neighbors = append(neighbors, shiftCoordinate(coord, shifts))
	}
	return neighbors
}

func permute(size int, elems []int) [][]int {
	if size == 0 {
		return [][]int{{}}
	}
	permutations := [][]int{}
	for _, perm := range permute(size-1, elems) {
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
