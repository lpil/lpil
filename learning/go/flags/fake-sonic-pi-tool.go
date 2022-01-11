// sonic-pi-tool 0.1.0
// Louis Pilfold <louis@lpil.uk>

// USAGE:
//     sonic-pi-tool [SUBCOMMAND]

// FLAGS:
//     -h, --help       Prints help information
//     -V, --version    Prints version information

// SUBCOMMANDS:
//     check           Check if the Sonic Pi server is listening on port 4557
//     eval            Takes a string of Sonic Pi code and sends it to the server
//     eval-file       Reads Sonic Pi code from a file and sends it to the server
//     eval-stdin      Reads Sonic Pi code from stdin and sends it to the server
//     help            Prints this message or the help of the given subcommand(s)
//     logs            Print log events emitted by the Sonic Pi server
//     record          Record the audio output of a Sonic Pi session
//     start-server    Find and start the Sonic Pi server
//     stop            Stops all currently playing music on the server
package main

import (
	"fmt"
	"os"

	"github.com/jessevdk/go-flags"
)

type Options struct {
	// Example of a callback, called each time the option is found.
	Version func() `long:"version" short:"v" description:"Prints version information"`
}

type Check struct{}

var check Check

func (_ *Check) Execute() error {
	fmt.Println("Check!")
	os.Exit(0)
	return nil
}

type Eval struct{}

var eval Eval

func (_ *Eval) Execute() error {
	fmt.Println("Eval!")
	os.Exit(0)
	return nil
}

var options Options

var parser = flags.NewParser(&options, flags.Default)

func main() {
	options.Version = func() {
		fmt.Println("0.1.0")
		os.Exit(0)
	}

	parser.AddCommand(
		"check",
		"Check if server running",
		"Check if the Sonic Pi server is listening on port 4557",
		&check,
	)

	parser.AddCommand(
		"eval",
		"Takes a string of Sonic Pi code and sends it to the server",
		"Takes a string of Sonic Pi code and sends it to the server",
		&eval,
	)

	_, err := parser.Parse()
	if err != nil {
		panic(err)
		flagsErr, ok := err.(*flags.Error)
		if ok && flagsErr.Type == flags.ErrHelp {
			os.Exit(0)
		} else {
			os.Exit(1)
		}
	}

	parser.WriteHelp(os.Stdout)
}
