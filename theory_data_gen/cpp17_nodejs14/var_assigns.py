from theory_data_gen.mask_tokens import AI_VAR_NAME, AI_VAL


def gen_var_assigns():
    """Generate a variable assignment pair."""

    assign = f'{AI_VAR_NAME} = {AI_VAL};'
    return [{'source': assign, 'target': assign}]
