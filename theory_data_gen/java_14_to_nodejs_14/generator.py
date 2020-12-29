from theory_data_gen.generator import Generator
from .arithmetic import gen_rogue_arithmetic
from .class_constructs import gen_class_constructs
from .classes import gen_classes
from .comments import gen_comments
from .conditional_structures import gen_conditional_structs
from .cout import gen_couts
from .entity_chains import gen_entity_chains
from .for_loop_inputs import gen_for_loop_inputs
from .funcs import gen_funcs
from .loop_structures import gen_loops
from .return_statements import gen_returns
from .switch_structures import gen_switch_data
from .catch_blocks import gen_catch_blocks
from .var_defs import gen_var_defs
from .vars import gen_vars


class Java14ToNodeJS14Generator(Generator):
    """Data generator for Java 14 to Node.js 14."""

    @staticmethod
    def generate(args):
        print('\nGenerating dataset for Java 14 --> Node.js 14')

        data = list()
        # data.extend(gen_vars(standard_count=args.vars, array_count=args.arr_vars))
        # data.extend(gen_var_defs(array_count=args.arr_var_defs))
        data.extend(gen_funcs(args.functions))
        # data.extend(gen_entity_chains(args.entity_chains))
        # data.extend(gen_classes(args.classes))
        # data.extend(gen_class_constructs(args.class_constructs))
        # data.extend(gen_conditional_structs(args.conditionals))
        # data.extend(gen_loops(args.loops))
        # data.extend(gen_for_loop_inputs(args.for_loop_inputs))
        # data.extend(gen_switch_data(switch_count=args.switches, case_count=args.switch_cases))
        # data.extend(gen_returns(args.returns))
        # data.extend(gen_catch_blocks())
        # data.extend(gen_comments())
        # data.extend(gen_couts(args.cout))
        # data.extend(gen_rogue_arithmetic(args.arithmetic))
        return data
