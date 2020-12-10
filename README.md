# Theory Data Generator

Data generator for the Turring Theory translation neural network.

## Run
```sh
python main.py \
  # Output file path
  -o output/dataset.csv \
  # Number of variable definitions that use a generic type
  --generic-var-defs 50 \
  # Number of functions
  --functions 500 \
  # Number of entity chains
  --entity-chains 1000 \
  # Number of classes
  --classes 50 \
  # Number of class construction statements
  --class-constructs 100
```
