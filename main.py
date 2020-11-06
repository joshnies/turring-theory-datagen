import os

from var_defs import gen_var_defs
from var_assigns import gen_var_assigns
from funcs import gen_funcs
from func_calls import gen_func_calls
from classes import gen_classes
from class_constructs import gen_class_constructs
from imports import gen_path_imports
from output import to_csv

# Cloud training
# GENERIC_VAR_DEFS_COUNT = 1000
# FUNC_COUNT = 200000
# FUNC_CALL_COUNT = 500000
# CLASS_COUNT = 1000
# CLASS_CONSTRUCTS_COUNT = 10000

# Local training
GENERIC_VAR_DEFS_COUNT = 200
FUNC_COUNT = 10000
FUNC_CALL_COUNT = 20000
CLASS_COUNT = 200
CLASS_CONSTRUCTS_COUNT = 500

OUTPUT_FILE_PATH = os.path.join('output', 'theory_dataset_cpp17_nodejs14.csv')

print('Generating...')

# Generate data
var_defs = gen_var_defs(generic_count=GENERIC_VAR_DEFS_COUNT)
var_assigns = gen_var_assigns()
funcs = gen_funcs(FUNC_COUNT)
func_calls = gen_func_calls(FUNC_CALL_COUNT)
classes = gen_classes(CLASS_COUNT)
class_constructs = gen_class_constructs(CLASS_CONSTRUCTS_COUNT)
path_imports = gen_path_imports()

# Output data to CSV file
to_csv([var_defs, var_assigns, funcs, func_calls, classes, class_constructs, path_imports], OUTPUT_FILE_PATH)

print('Output to {}'.format(OUTPUT_FILE_PATH))
