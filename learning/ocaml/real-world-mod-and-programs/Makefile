.PHONY: run
run: _build/default/program.exe
	_build/default/program.exe

_build/default/program.exe: $(wildcard *.ml) $(wildcard *.mli)
	jbuilder build program.exe
