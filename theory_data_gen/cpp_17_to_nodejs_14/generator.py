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
from .imports import gen_imports
from .jump_statements import gen_jump_statements
from .loop_structures import gen_loops
from .return_statements import gen_returns
from .rogue_entities import gen_rogue_entities
from .switch_structures import gen_switch_data
from .try_catch_blocks import gen_try_catch_blocks
from .vars import gen_vars


class Cpp17ToNodeJS14Generator(Generator):
    """Data generator for C++17 to Node.js 14."""

    @staticmethod
    def generate(args):
        print('Generating dataset for C++17 --> Node.js 14')

        data = list()
        # data.extend(gen_imports())
        data.extend(gen_vars(args.vars))
        # data.extend(gen_funcs(args.functions))
        # data.extend(gen_entity_chains(args.entity_chains))
        # data.extend(gen_classes(args.classes))
        # data.extend(gen_class_constructs(args.class_constructs))
        # data.extend(gen_conditional_structs(args.conditionals))
        # data.extend(gen_loops(args.loops))
        # data.extend(gen_for_loop_inputs(args.for_loop_inputs))
        # data.extend(gen_switch_data())
        # data.extend(gen_jump_statements())
        # data.extend(gen_returns(args.returns))
        # data.extend(gen_try_catch_blocks())
        # data.extend(gen_comments())
        # data.extend(gen_couts())
        # data.extend(gen_rogue_arithmetic(args.arithmetic))
        # data.extend(gen_rogue_entities())
        return data
