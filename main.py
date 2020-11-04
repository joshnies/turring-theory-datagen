from output import to_csv
from vars import gen_var_defs
from funcs import gen_funcs

FUNC_COUNT = 200000
OUTPUT_FILE_PATH = 'theory_dataset_cpp17_nodejs14.csv'

# Generate data
var_defs = gen_var_defs()
funcs = gen_funcs(FUNC_COUNT)

# Output data to CSV file
to_csv([var_defs, funcs], OUTPUT_FILE_PATH)
