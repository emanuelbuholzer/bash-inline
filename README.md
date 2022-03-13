# bash-inline

## Introduction

`bash-inline` is a CLI tool to format Bash scripts for inlining.

## Usage

In order to format a Bash script to be readable you can run `bash-inline` without any further paramters.
By default `bash-inline` reads your Bash script from standard input to standard output.

Given a Bash script like this:
```bash
#!/usr/bin/env bash
echo "Is true just 1?"
# We'll see
if [[ true -eq 1 ]]; then
  echo "True is really just 1"
fi
```

Running bash-inline on it will remove comments, format (e.g. add missing semicolons) and check your script, such that it can be inlined:
```bash 
echo "Is true just 1?";
if [[ true -eq 1 ]]; then
  echo "True is really just 1";
fi;
```

You can also use this tool to expand a one-liner and make it more readable. The general usage is:
```bash
cat script.sh | bash-inline
```

The same script can also be one-lined when passing the `-1` flag:
```bash
echo "Is true just 1?"; if [[ true -eq 1 ]]; then echo "True is really just 1"; fi;
```

The general usage for creating a one-liner is:
```bash
cat script.sh | bash-inline -1
```

You can also read and output the Bash script from a file, and pass some arguments for formatting it given some line length and ribbon ratio constraints. 
For more information on how to use `bash-inline` run `bash-inline --help`.

## Building

This project is built with [GHC](http://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install), you can get the latest version by using [GHCup](https://www.haskell.org/ghcup/).

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

## Special thanks

To everyone who contributed to [language-bash](http://hackage.haskell.org/package/language-bash) and [pretty](http://hackage.haskell.org/package/pretty) which provide the core functionality to this tool.
