mtscheme.exe: interpreter.fs parser.fs helper.fs tester.fs runner.fs
	fsc.exe -g -r nunit.framework.dll interpreter.fs parser.fs helper.fs tester.fs runner.fs -o mtscheme.exe

