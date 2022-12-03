package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"runtime/pprof"

	"gol"
)

func main() {
	var (
		cpuprofile          string
		memprofile          string
		dimensions          int
		maxNeighborDistance int
		iterations          int
	)
	flag.StringVar(&cpuprofile, "cpuprofile", "", "write cpu profile to file")
	flag.StringVar(&memprofile, "memprofile", "", "write memory profile to this file")
	flag.IntVar(&dimensions, "n", 4, "dimensions")
	flag.IntVar(&maxNeighborDistance, "d", 1, "maxNeighborDistance")
	flag.IntVar(&iterations, "i", 6, "iterations")
	flag.Parse()

	filename := flag.Arg(0)
	if filename == "" {
		log.Fatal("you must specify an input file")
	}

	f, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	space, err := gol.ParseGameState(f, dimensions)
	if err != nil {
		log.Fatal(err)
	}

	if cpuprofile != "" {
		f, err := os.Create(cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	for i := 0; i < iterations; i++ {
		space = space.NextState()
	}

	fmt.Println(space.KeyCount())

	if memprofile != "" {
		f, err := os.Create(memprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.WriteHeapProfile(f)
		f.Close()
		return
	}
}
