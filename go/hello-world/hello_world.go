package hello

import (
	"fmt"
)

const testVersion = 2

func HelloWorld(name string) string {
	if name == "" {
		return "Hello, World!"
	} else {
		return fmt.Sprintf("Hello, %s!", name)
	}
}
