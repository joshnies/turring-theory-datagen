from theory_data_gen.generator import Generator
from .arithmetic import gen_rogue_arithmetic
from .class_constructs import gen_class_constructs
from .classes import gen_classes
from .comments import gen_comments
from .conditional_structures import gen_conditional_structs
from .cout import gen_couts
from .entity_chains import gen_entity_chains
from .foreach_loop_inputs import gen_foreach_loop_inputs
from .funcs import gen_funcs
from .loop_structures import gen_loops
from .return_statements import gen_returns
from .catch_blocks import gen_catch_blocks
from .var_defs import gen_var_defs
from .vars import gen_vars


class Java14ToPython3Generator(Generator):
    """Data generator for Java 14 to Python 3."""

    @staticmethod
    def generate(args, write):
        print('\nGenerating dataset for Java 14 --> Python 3')

        gen_vars(write, standard_count=args.vars, array_count=args.arr_vars)
        gen_var_defs(write, array_count=args.arr_var_defs)
        gen_funcs(write, args.functions)
        gen_entity_chains(write, args.entity_chains)
        gen_classes(write, args.classes)
        gen_class_constructs(write, args.class_constructs)
        gen_conditional_structs(write, args.conditionals)
        gen_loops(write, args.loops)
        gen_foreach_loop_inputs(write, args.for_loop_inputs)
        gen_returns(write, args.returns)
        gen_catch_blocks(write)
        gen_comments(write)
        gen_couts(write, args.cout)
        gen_rogue_arithmetic(write, args.arithmetic)
