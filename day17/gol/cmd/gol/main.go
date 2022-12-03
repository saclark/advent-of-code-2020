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
		maxNeighborDistance int
		iterations          int
	)
	flag.StringVar(&cpuprofile, "cpuprofile", "", "write cpu profile to file")
	flag.StringVar(&memprofile, "memprofile", "", "write memory profile to this file")
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

	initActives, err := gol.ParseGameState(f)
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

	game := gol.NewGameOfLife(initActives, maxNeighborDistance)
	for i := 0; i < iterations; i++ {
		game.NextState()
	}

	fmt.Println(game.ActiveCoordinateCount())

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
