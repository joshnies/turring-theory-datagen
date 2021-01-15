from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.generator import Generator


class JderegJavaUtilGenerator(Generator):
    """
    Case generator for "jdereg/java-util":
    https://github.com/jdereg/java-util
    """

    @staticmethod
    def generate(args, write):
        print('\nGenerating case data for "jdereg/java-util"')

        items = [
            gen_item(
                f'public class {gen_mask_token(0)}<{gen_mask_token(1)}, {gen_mask_token(2)}> implements {gen_mask_token(3)}<{gen_mask_token(1)}, {gen_mask_token(2)}> {{'
                f'class {gen_mask_token(0)} implements {gen_mask_token(3)} {{'
            ),
            gen_item(
                f'private final {gen_mask_token(0)}<{gen_mask_token(1)}, {gen_mask_token(2)}> {gen_mask_token(3)};'
                f'private {gen_mask_token(3)};'
            ),
            gen_item(
                f'if ({gen_mask_token(0)} == null)',
                f'if ({gen_mask_token(0)} == null)'
            ),
            gen_item(
                f'throw new IllegalArgumentException({gen_mask_token(0)});'
                f'throw new Error({gen_mask_token(0)});'
            ),
            gen_item(
                f'{gen_mask_token(0)} = {gen_mask_token(1)};',
                f'{gen_mask_token(0)} = {gen_mask_token(1)};'
            ),
            gen_item(
                f'{gen_mask_token(0)} = new HashSet<>();',
                f'{gen_mask_token(0)} = new Set();'
            ),
            gen_item(
                f'{gen_mask_token(0)} {gen_mask_token(1)} = {gen_mask_token(2)}.{gen_mask_token(3)}({gen_mask_token(4)});',
                f'let {gen_mask_token(1)} = {gen_mask_token(2)}.{gen_mask_token(3)}({gen_mask_token(4)});'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}(({gen_mask_token(2)}) {gen_mask_token(3)});',
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(3)});'
            )
        ]

        for i in items:
            write(i)
