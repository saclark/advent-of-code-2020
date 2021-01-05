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
	space.areaCoordinates()
	for i := 0; i < iterations; i++ {
		space = space.NextState()
	}
	fmt.Println(space.ActivesCount())
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

	space := newActiveSpace()
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
				space.activate(coord)
			}
		}
		y++
	}

	if err := scanner.Err(); err != nil {
		return ActiveSpace{}, fmt.Errorf("scanning input file: %w", err)
	}

	return space, nil
}

type ActiveSpace struct {
	actives IntTree
}

func newActiveSpace() ActiveSpace {
	return ActiveSpace{IntTree{}}
}

func (s ActiveSpace) NextState() ActiveSpace {
	newSpace := newActiveSpace()
	for _, coord := range s.areaCoordinates() {
		if s.shouldActivate(coord) {
			newSpace.activate(coord)
		}
	}
	return newSpace
}

func (s ActiveSpace) ActivesCount() int {
	return s.actives.LeafCount()
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

func (s ActiveSpace) activate(coord []int) {
	s.actives.AddBranch(coord)
}

func (s ActiveSpace) isActive(coord []int) bool {
	hasBranch, isLeaf := s.actives.HasBranch(coord)
	return hasBranch && isLeaf
}

func (s ActiveSpace) areaCoordinates() [][]int {
	coords := IntTree{}
	for _, coord := range s.actives.Branches() {
		coords.AddBranch(coord)
		for _, neighbor := range getNeighbors(coord) {
			coords.AddBranch(neighbor)
		}
	}
	return coords.Branches()
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
		panic("lenght of offsets must equal length of coord")
	}
	shifted := make([]int, len(coord))
	for i, shift := range shifts {
		shifted[i] = coord[i] + shift
	}
	return shifted
}

type IntTree map[int]IntTree

func (t IntTree) LeafCount() int {
	var count int
	for _, v := range t {
		subCount := v.LeafCount()
		if subCount == 0 {
			count++
		} else {
			count += subCount
		}
	}
	return count
}

func (t IntTree) Branches() [][]int {
	var branches [][]int
	for k, subTree := range t {
		subBranches := subTree.Branches()
		if len(subBranches) == 0 {
			branches = append(branches, []int{k})
		} else {
			for _, subBranch := range subBranches {
				subBranch = append(subBranch[:1], subBranch[0:]...)
				subBranch[0] = k
				branches = append(branches, subBranch)
			}
		}
	}
	return branches

}

func (t IntTree) HasBranch(branch []int) (hasBranch bool, isLeaf bool) {
	subTree := t
	for _, k := range branch {
		v, exists := subTree[k]
		if !exists {
			return false, false
		}
		subTree = v
	}
	if len(subTree) != 0 {
		return true, false
	}
	return true, true
}

func (t IntTree) AddBranch(branch []int) {
	subTree := t
	for len(branch) > 0 {
		v, exists := subTree[branch[0]]
		if !exists {
			v = IntTree{}
			subTree[branch[0]] = v
		}
		subTree = v
		branch = branch[1:]
	}
}
