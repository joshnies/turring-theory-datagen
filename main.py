import argparse

from var_defs import gen_var_defs
from var_assigns import gen_var_assigns
from funcs import gen_funcs
from func_calls import gen_func_calls
from conditional_structures import gen_conditional_structs
from loop_structures import gen_loop_structs
from for_loop_inputs import gen_for_loop_inputs
from switch_structures import gen_switch_data
from jump_statements import gen_jump_statements
from classes import gen_classes
from class_constructs import gen_class_constructs
from try_catch_blocks import gen_try_catch_blocks
from imports import gen_path_imports
from comments import gen_comments
from cout import gen_couts
from output import to_csv

# Parse args
parser = argparse.ArgumentParser(description='Generate Theory dataset.')
parser.add_argument('-o', '--out', help='Output file path', required=True)
parser.add_argument('--generic-var-defs', help='Number of generic variable definitions', type=int, required=True)
parser.add_argument('--functions', help='Number of functions', type=int, required=True)
parser.add_argument('--function-calls', help='Number of function calls', type=int, required=True)
parser.add_argument('--classes', help='Number of classes', type=int, required=True)
parser.add_argument('--class-constructs', help='Number of class construction statements', type=int, required=True)
args = parser.parse_args()

# Generate data
var_defs = gen_var_defs(generic_count=args.generic_var_defs)
var_assigns = gen_var_assigns()
funcs = gen_funcs(args.functions)
func_calls = gen_func_calls(args.function_calls)
conditional_structs = gen_conditional_structs()
loop_structs = gen_loop_structs()
for_loop_inputs = gen_for_loop_inputs()
switch_data = gen_switch_data()
jump_stmts = gen_jump_statements()
classes = gen_classes(args.classes)
class_constructs = gen_class_constructs(args.class_constructs)
try_catch_blocks = gen_try_catch_blocks()
path_imports = gen_path_imports()
comments = gen_comments()
couts = gen_couts()

# Output data to CSV file
to_csv(
    [var_defs, var_assigns, funcs, func_calls, conditional_structs, loop_structs, for_loop_inputs, switch_data,
     jump_stmts, classes, class_constructs, try_catch_blocks, path_imports, comments, couts], args.out)

print('Output to {}'.format(args.out))
