package check

type CheckResult interface {
	String() string
	Detail() string
	IsOk() bool
}

// Pass
// Everything is OK!
//
type CheckPass struct{}

func (_ CheckPass) IsOk() bool {
	return true
}

func (_ CheckPass) String() string {
	return "pass"
}

func (_ CheckPass) Detail() string {
	return ""
}

// Fail
// The target failed to pass the check, there is a problem.
//
type CheckFail struct{ detail string }

func (_ CheckFail) IsOk() bool {
	return false
}

func (_ CheckFail) String() string {
	return "fail"
}

func (r CheckFail) Detail() string {
	return r.detail
}

// Error
// There was an internal problem while performing the check. There is likely a
// bug within this program.
// We represent errors using this variant rather than panicking because it may
// take a long time to run all the checks and thus we don't want a singular
// error to stop us from getting the results from the other unaffected checks.
//
type CheckError struct{ detail string }

func (_ CheckError) IsOk() bool {
	return false
}

func (_ CheckError) String() string {
	return "error"
}

func (r CheckError) Detail() string {
	return r.detail
}
