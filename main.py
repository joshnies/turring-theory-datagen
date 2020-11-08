import os

from conditional_structures import gen_conditional_structs
from for_loop_inputs import gen_for_loop_inputs
from loop_structures import gen_loop_structs
from switch_structures import gen_switch_data
from var_defs import gen_var_defs
from var_assigns import gen_var_assigns
from funcs import gen_funcs
from func_calls import gen_func_calls
from classes import gen_classes
from class_constructs import gen_class_constructs
from imports import gen_path_imports
from output import to_csv

# Full dataset
GENERIC_VAR_DEFS_COUNT = 1000
FUNC_COUNT = 200000
FUNC_CALL_COUNT = 500000
CLASS_COUNT = 1000
CLASS_CONSTRUCTS_COUNT = 10000

# Slim dataset
# GENERIC_VAR_DEFS_COUNT = 200
# FUNC_COUNT = 10000
# FUNC_CALL_COUNT = 20000
# CLASS_COUNT = 200
# CLASS_CONSTRUCTS_COUNT = 500

OUTPUT_FILE_PATH = os.path.join('output', 'theory_data_cpp17_nodejs14.csv')

print('Generating...')

# Generate data
var_defs = gen_var_defs(generic_count=GENERIC_VAR_DEFS_COUNT)
var_assigns = gen_var_assigns()
funcs = gen_funcs(FUNC_COUNT)
func_calls = gen_func_calls(FUNC_CALL_COUNT)
conditional_structs = gen_conditional_structs()
loop_structs = gen_loop_structs()
for_loop_inputs = gen_for_loop_inputs()
switch_data = gen_switch_data()
classes = gen_classes(CLASS_COUNT)
class_constructs = gen_class_constructs(CLASS_CONSTRUCTS_COUNT)
path_imports = gen_path_imports()

# Output data to CSV file
to_csv(
    [var_defs, var_assigns, funcs, func_calls, conditional_structs, loop_structs, for_loop_inputs, switch_data, classes,
     class_constructs, path_imports], OUTPUT_FILE_PATH)

print('Output to {}'.format(OUTPUT_FILE_PATH))
