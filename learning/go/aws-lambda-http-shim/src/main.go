package main

import (
	"fmt"
	"net/http"

	"github.com/aws/aws-lambda-go/lambda"
	"github.com/gorilla/mux"
	"github.com/iamatypeofwalrus/shim"
)

func main() {
	// Create a router as normal. Any router that satisfies the http.Handler interface
	// is accepted!
	router := mux.NewRouter()

	router.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprint(w, "hello, world")
	})

	router.
		HandleFunc("/hiya", func(w http.ResponseWriter, req *http.Request) {
			fmt.Fprint(w, "hello, world")
		}).
		Methods("GET")

	router.
		HandleFunc("/hiya", func(w http.ResponseWriter, req *http.Request) {
			fmt.Fprint(w, "Nope, no POSTs here")
		}).
		Methods("POST")

	s := shim.New(router)

	// Pass your router to shim and let Lambda handle the rest
	lambda.Start(s.Handle)
}
