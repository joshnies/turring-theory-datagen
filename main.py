from var_defs import gen_var_defs
from var_assigns import gen_var_assigns
from funcs import gen_funcs
from classes import gen_classes
from output import to_csv

FUNC_COUNT = 200000
CLASS_COUNT = 1000
OUTPUT_FILE_PATH = 'theory_dataset_cpp17_nodejs14.csv'

# Generate data
var_defs = gen_var_defs()
var_assigns = gen_var_assigns()
funcs = gen_funcs(FUNC_COUNT)
classes = gen_classes(CLASS_COUNT)

# Output data to CSV file
to_csv([var_defs, var_assigns, funcs, classes], OUTPUT_FILE_PATH)
