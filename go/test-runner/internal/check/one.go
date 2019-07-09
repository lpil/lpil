package check

type One struct{}

func (_ One) Name() string {
	return "Number One"
}

func (_ One) Description() string {
	return "The first one, etc etc"
}

func (_ One) Exec() CheckResult {
	return pass()
}
