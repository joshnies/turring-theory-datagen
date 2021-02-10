# Theory Dataset Generator

Dataset generator for Turring Theory's translation neural network.

## Run

```sh
python main.py \
  # Output file path
  -o output/dataset.csv \
  # Number of variables
  --vars 10000 \
  # Number of array variables
  --arr-vars 10000 \
  # Number of functions
  --functions 200000 \
  # Number of entity chains
  --entity-chains 500000 \
  # Number of classes
  --classes 1000 \
  # Number of class construction statements
  --class-constructs 10000 \
  # Number of conditionals
  --conditionals 1000 \
  # Number of switch structures
  --switches 1000 \
  # Number of switch cases
  --switch-cases 1000 \
  # Number of loop structures (excluding "for" loops)
  --loops 1000 \
  # Number of "for" loop structures
  --for-loop-inputs 1000 \
  # Number of arithmetic expressions
  --arithmetic 1000 \
  # Number of return statements
  --returns 1000
```
