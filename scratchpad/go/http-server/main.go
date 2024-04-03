package main

import (
	"fmt"
	"net/http"
)

func main() {
	http.HandleFunc("/", handleRoot)

	fmt.Println("Listening on port 8000...")
	err := http.ListenAndServe(":8000", nil)
	if err != nil {
		panic(err)
	}
}

func handleRoot(w http.ResponseWriter, r *http.Request) {
	fmt.Println("handleRoot called")
	w.Write([]byte("Hello there!"))
}
