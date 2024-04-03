// This doesn't work. Need to store the state in a Mutex.

package testwrapper

import (
	"fmt"
	"os"
	"runtime"
	"testing"
)

var Suite = TestSuite{
	results: make(map[string]TestCaseResult),
}

type TestSuite struct {
	results map[string]TestCaseResult
}

func (s *TestSuite) Test(testingT *testing.T, name string) TestCase {
	if _, present := Suite.results[name]; present {
		testingT.Fatalf(fmt.Sprintf("There is already a TestCase named `%s`", name))
	}

	Suite.results[name] = TestCaseResult{true, make([]string, 0)}

	return TestCase{name, testingT}
}

type TestCase struct {
	name     string
	testingT *testing.T
}

func (t *TestCase) Errorf(format string, args ...interface{}) {
	err := fmt.Sprintf(format, args...)
	Suite.results[t.name].appendError(err)
	t.testingT.Error(err)
}

type TestCaseResult struct {
	ok     bool
	errors []string
}

func (r TestCaseResult) appendError(err string) {
	r.errors = append(r.errors, err)
}

func MakeTestCase(testingT *testing.T) TestCase {
	_, file, line, _ := runtime.Caller(0)
	fmt.Printf("%s:%d %s", file, line, testingT.Name())
	return TestCase{testingT: testingT}
}

func TestMain(m *testing.M) {
	exit := m.Run()
	fmt.Printf("%#v", Suite)
	os.Exit(exit)
}

func TestUniverse(tt *testing.T) {
	t := Suite.Test(tt, "Universe still works")

	result := 1 + 2
	if result != 2 {
		t.Errorf("1 + 1 == %d", result)
	}

	fmt.Printf("%T", Suite)
}

// func TestUniverse2(tt *testing.T) {
// 	t := Suite.Test(tt, "Universe still works")
// 	result := 1 + 1
// 	if result != 2 {
// 		t.Errorf("1 + 1 == %d", result)
// 	}
// }
