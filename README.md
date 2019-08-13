# bash-inline

`bash-inline` enables you to inline your Bash scripts for using it in contexts such as `bash -c '$SCRIPT'`. 
By default `bash-inline` reads your Bash script from standard input and output it to standard output.
It's possible to output your script as a one-liner or keeping it as a multiline script (default).

## Usage

The most common usage scenarios are shown below:
```bash
# Inline from standard input
cat script.sh | bash-inline

# Inline from file
bash-inline -f script.sh

# Convert to a one-liner
cat script.sh | bash-inline -o 

# For more information and help
bash-inline --help
```
