from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.generator import Generator


class FGREGGTaxExtensionToCSharp9Generator(Generator):
    """
    Case generator for "fgregg/tax_extension":
    https://github.com/fgregg/tax_extension

    Source: COBOL
    Target: C# 9
    """

    @staticmethod
    def generate(args, write):
        print('\nGenerating case data for "fgregg/tax_extension" --> C# 9')

        items = [
            gen_item(
                f"DISPLAY '{gen_mask_token(0)}' '{gen_mask_token(1)}' {gen_mask_token(2)} ({gen_mask_token(3)})",
                f'Console.WriteLine("{gen_mask_token(0)}" + "{gen_mask_token(1)}" + {gen_mask_token(2)}.GetSubvalue(start: {gen_mask_token(3)}));'
            ),
            gen_item(
                f'{gen_mask_token(0)} {gen_mask_token(1)} PIC XXBXXBX({gen_mask_token(2)})',
                f"{gen_mask_token(1)} = new COBOLVar(new string(' ', 7), size: 7);"
            ),
            gen_item(
                f'{gen_mask_token(0)} {gen_mask_token(1)} PIC XXBXXXBX({gen_mask_token(2)})',
                f"{gen_mask_token(1)} = new COBOLVar(new string(' ', 8), size: 8);"
            ),
        ]

        for i in items:
            write(i)
