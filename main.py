import argparse

from theory_data_gen.common import deduplicate
from theory_data_gen.cpp_17_to_nodejs_14.imports import gen_imports
from theory_data_gen.cpp_17_to_nodejs_14.vars import gen_vars
from theory_data_gen.cpp_17_to_nodejs_14.funcs import gen_funcs
from theory_data_gen.cpp_17_to_nodejs_14.entity_chains import gen_entity_chains
from theory_data_gen.cpp_17_to_nodejs_14.classes import gen_classes
from theory_data_gen.cpp_17_to_nodejs_14.class_constructs import gen_class_constructs
from theory_data_gen.cpp_17_to_nodejs_14.conditional_structures import gen_conditional_structs
from theory_data_gen.cpp_17_to_nodejs_14.loop_structures import gen_loops
from theory_data_gen.cpp_17_to_nodejs_14.for_loop_inputs import gen_for_loop_inputs
from theory_data_gen.cpp_17_to_nodejs_14.switch_structures import gen_switch_data
from theory_data_gen.cpp_17_to_nodejs_14.jump_statements import gen_jump_statements
from cpp_17_to_nodejs_14.return_statements import gen_returns
from theory_data_gen.cpp_17_to_nodejs_14.try_catch_blocks import gen_try_catch_blocks
from theory_data_gen.cpp_17_to_nodejs_14.comments import gen_comments
from theory_data_gen.cpp_17_to_nodejs_14.cout import gen_couts
from cpp_17_to_nodejs_14.arithmetic import gen_rogue_arithmetic
from theory_data_gen.cpp_17_to_nodejs_14.rogue_entities import gen_rogue_entities
from theory_data_gen.output import to_csv

# Parse args
parser = argparse.ArgumentParser(description='Generate Theory dataset.')
parser.add_argument('-o', '--out', help='Output file path', required=True)
parser.add_argument('--generic-var-defs', help='Number of variable definitions that use a generic type', type=int,
                    required=True)
parser.add_argument('--vars', help='Number of variables', type=int, required=True)
parser.add_argument('--functions', help='Number of functions', type=int, required=True)
parser.add_argument('--entity-chains', help='Number of entity chains', type=int, required=True)
parser.add_argument('--classes', help='Number of classes', type=int, required=True)
parser.add_argument('--class-constructs', help='Number of class construction statements', type=int, required=True)
parser.add_argument('--conditionals', help='Number of conditional structures', type=int, required=True)
parser.add_argument('--loops', help='Number of loops', type=int, required=True)
parser.add_argument('--for-loop-inputs', help='Number of rogue "for" loop inputs', type=int, required=True)
parser.add_argument('--arithmetic', help='Number of arithmetic expressions', type=int, required=True)
parser.add_argument('--returns', help='Number of "return" statements',
                    type=int, required=True)
args = parser.parse_args()

# Generate data
data = list()
data.extend(gen_imports())
data.extend(gen_vars(args.vars))
data.extend(gen_funcs(args.functions))
data.extend(gen_entity_chains(args.entity_chains))
data.extend(gen_classes(args.classes))
data.extend(gen_class_constructs(args.class_constructs))
data.extend(gen_conditional_structs(args.conditionals))
data.extend(gen_loops(args.loops))
data.extend(gen_for_loop_inputs(args.for_loop_inputs))
data.extend(gen_switch_data())
data.extend(gen_jump_statements())
data.extend(gen_returns(args.returns))
data.extend(gen_try_catch_blocks())
data.extend(gen_comments())
data.extend(gen_couts())
data.extend(gen_rogue_arithmetic(args.arithmetic))
data.extend(gen_rogue_entities())

# Deduplicate data
data = deduplicate(data)

# Output data to CSV file
to_csv(data, args.out)

print('Output to {}'.format(args.out))
