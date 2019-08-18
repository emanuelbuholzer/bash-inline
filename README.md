# bash-inline

## Introduction

`bash-inline` is a CLI tool to format Bash scripts for inlining.

## Usage

In order to format a Bash script to be readable you can run `bash-inline` without any further paramters.
By default `bash-inline` reads your Bash script from standard input to standard output.

```
# Given a Bash script like:
# 
# | #!/usr/bin/env bash
# | echo "Is true just 1?"
# | 
# | # We'll see
# | if [[ true -eq 1 ]]; then
# |   echo "True is really just 1"
# | fi
# 
# Running bash-inline on it will remove comments, format (e.g. add missing semicolons) and check your script:
# 
# | echo "Is true just 1?";
# | if [[ true -eq 1 ]]; then
# |   echo "True is really just 1";
# | fi;
#
# This can be also used for making a one-liner more readable.
cat example.sh | bash-inline
```

The same script can also be one-lined when passing the `-1` flag.

```bash
# Running bash-inline with the -1 flag will one-line it:
#
# | echo "Is true just 1?"; if [[ true -eq 1 ]]; then echo "True is really just 1"; fi;
#
cat example.sh | bash-inline -1
```

You can also read and output the Bash script from a file, and pass some arguments for formatting it given some line length and ribbon ratio constraints. 
For more information on how to use `bash-inline` run `bash-inline --help`

## Building

This project is built with [GHC](http://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install), you can get the latest version by installing the [Haskell platform](http://hackage.haskell.org/platform/).

Update dependencies and configure the project with tests enabled:
```bash
cabal update 
cabal configure --enable-tests
```

Build the project and run tests:
```bash
cabal build
cabal test
```

If you want to install the latest version of `bash-inline`:
```bash
# Be sure that the $CABALDIR/bin directory is in your path
cabal install
```

## Special Thanks

To everyone who contributed to [langugae-bash](http://hackage.haskell.org/package/language-bash) and [pretty](http://hackage.haskell.org/package/pretty) which provides the core functionality to this tool.

