package main

import (
	"fmt"
	"time"
)

func main() {
	i := 0
	fmt.Println("Hi")

	go func() {
		for {
			time.Sleep(800000000)
			fmt.Println("Other goroutine incrementing i")
			i += 1
		}
	}()

	for {
		time.Sleep(100000000)
		fmt.Printf("%d\n", i)
	}
}
