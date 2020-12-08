import argparse

from theory_data_gen.common import deduplicate
from theory_data_gen.cpp17_nodejs14.imports import gen_path_imports
from theory_data_gen.cpp17_nodejs14.var_defs import gen_var_defs
from theory_data_gen.cpp17_nodejs14.var_assigns import gen_var_assigns
from theory_data_gen.cpp17_nodejs14.funcs import gen_funcs
from theory_data_gen.cpp17_nodejs14.func_calls import gen_func_calls
from theory_data_gen.cpp17_nodejs14.conditional_structures import gen_conditional_structs
from theory_data_gen.cpp17_nodejs14.loop_structures import gen_loop_structs
from theory_data_gen.cpp17_nodejs14.for_loop_inputs import gen_for_loop_inputs
from theory_data_gen.cpp17_nodejs14.switch_structures import gen_switch_data
from theory_data_gen.cpp17_nodejs14.jump_statements import gen_jump_statements
from theory_data_gen.cpp17_nodejs14.classes import gen_classes
from theory_data_gen.cpp17_nodejs14.class_constructs import gen_class_constructs
from theory_data_gen.cpp17_nodejs14.try_catch_blocks import gen_try_catch_blocks
from theory_data_gen.cpp17_nodejs14.comments import gen_comments
from theory_data_gen.cpp17_nodejs14.bool_expressions import gen_bool_expressions
from theory_data_gen.cpp17_nodejs14.cout import gen_couts
from theory_data_gen.output import to_csv

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
data = list()
data.extend(gen_path_imports())
data.extend(gen_var_defs(generic_count=args.generic_var_defs))
data.extend(gen_var_assigns())
data.extend(gen_funcs(args.functions))
data.extend(gen_func_calls(args.function_calls))
data.extend(gen_conditional_structs())
data.extend(gen_loop_structs())
data.extend(gen_for_loop_inputs())
data.extend(gen_switch_data())
data.extend(gen_jump_statements())
data.extend(gen_classes(args.classes))
data.extend(gen_class_constructs(args.class_constructs))
data.extend(gen_try_catch_blocks())
data.extend(gen_comments())
data.extend(gen_bool_expressions())
data.extend(gen_couts())

# Deduplicate data
data = deduplicate(data)

# Output data to CSV file
to_csv(data, args.out)

print('Output to {}'.format(args.out))
