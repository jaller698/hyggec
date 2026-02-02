# hyggec - the Didactic Compiler for Hygge

This is the source code `hyggec`, the didactic compiler for the Hygge
programming language.

Hygge and `hyggec` have been designed and developed as learning tools for the
course *02247 Compiler Construction* at DTU Compute - Technical University of
Denmark.

`hyggec` is released under the terms of the [MIT license](LICENSE.md).

## Software Requirements

  * .NET 10.0 (for compiling and running `hyggec`): <https://dotnet.microsoft.com/en-us/download>
 
  * Java Runtime Environment, preferably version 21 or later
    (for running the included copy of [RARS, the RISC-V Assembler and Runtime Simulator](https://github.com/TheThirdOne/rars))
    - On Ubuntu and Debian GNU/Linux: `sudo apt install openjdk-21-jre`
    - On MacOS: `brew install openjdk@21`
    - On Windows: <https://www.oracle.com/java/technologies/javase/jdk21-archive-downloads.html>

**NOTE:** you will need to have both the `dotnet` and `java` programs in your
[executable PATH](https://janelbrandon.medium.com/understanding-the-path-variable-6eae0936e976)
(their installation scripts should take care of it).

## Quick Start

After installing the required software above, open a terminal in the root
directory of the `hyggec` source tree, and try:

```
./hyggec test
```

This command automatically builds `hyggec` and runs its test suite. If you don't
see any error, then `hyggec` was built correctly and passed all its tests.  You
should now be able to use and modify it.

To see the usage options, you can execute:

```
./hyggec help
```

You will see a list of various commands.  To get usage options for a specific
command (for example, `compile`):

```
./hyggec compile --help
```

Here's something you can try:

```
./hyggec interpret --typecheck --verbose examples/hygge0-spec-example.hyg
```

## Building `hyggec` from the Command Line

Every time you invoke the script `./hyggec`, the compiler will be rebuilt if its
source code was modified since the last execution.

You can also (re)build the `hyggec` executable by running:

```
dotnet build
```

To clean up the results of a build, you can run:

```
dotnet clean
```

## Recommended Visual Studio Code Extension

The following VS Code extension is highly recommended for working on `hyggec`
(and F# projects in general):

  * [Ionide for F#](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp)

Some recommendations for configuring the Ionide extension:

  * Disable the setting _"F Sharp › Inlay Hints: Parameter Names"_
  * Disable the setting _"F Sharp › Inlay Hints: Type Annotations"_

The reason for these recommendations is that the F# programming language is
indentation-sensitive (similarly to Python) -- but the settings above (which are
enabled by default) produce inlay hints that change the visual alignment of the
code.  As a consequence, these inlay hints make it harder to spot and fix errors
caused by misindented code.

Even if you disable the settings above, you can get the same information by
hovering the mouse pointer on functions, variables, or operators: Ionide will
show the hints in a pop-up window.
