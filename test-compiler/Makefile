# F# project doesn't exactly need a makefile, but writing one that translates make-isms into dotnet calls makes it easier to integrate this to other parts of the build infrastructure

DEPS=$(wildcard *.fs) test-compiler.fsproj 

test-compiler/test-compiler: $(DEPS)
	dotnet build ./test-compiler.fsproj -c release -o ./test-compiler 

# wonder if we need a recipe for installing dependencies or if dotnet automagically installs them on clean environments?
# If we'll find out that the dependencies need explicit installing, the recipe should be easy to write off from package-add-demo.sh

run: $(DEPS)
	dotnet run --input ../exporter-test-output.json --output lol.xml
