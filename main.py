import argparse

from cpp_17_to_nodejs_14.generator import Cpp17ToNodeJS14Generator
from java_14_to_nodejs_14.generator import Java14ToNodeJS14Generator
from theory_data_gen.lvp import LVP
from theory_data_gen.common import deduplicate
from theory_data_gen.output import to_csv

# Parse args
parser = argparse.ArgumentParser(description='Generate Theory dataset.')
parser.add_argument('-l', '--lvp', help='Language-version pair', required=True)
parser.add_argument('-o', '--out', help='Output file path', required=True)
parser.add_argument('--vars', help='Number of variables', type=int, required=True)
parser.add_argument('--arr-vars', help='Number of array variables', type=int, required=True)
parser.add_argument('--arr-var-defs', help='Number of array variable definitions (with no default value)', type=int,
                    required=True)
parser.add_argument('--functions', help='Number of functions', type=int, required=True)
parser.add_argument('--entity-chains', help='Number of entity chains', type=int, required=True)
parser.add_argument('--classes', help='Number of classes', type=int, required=True)
parser.add_argument('--class-constructs', help='Number of class construction statements', type=int, required=True)
parser.add_argument('--conditionals', help='Number of conditional structures', type=int, required=True)
parser.add_argument('--switches', help='Number of switch structures', type=int, required=True)
parser.add_argument('--switch-cases', help='Number of switch cases', type=int, required=True)
parser.add_argument('--loops', help='Number of loops (excluding "for" loops)', type=int, required=True)
parser.add_argument('--for-loop-inputs', help='Number of rogue "for" loop inputs', type=int, required=True)
parser.add_argument('--arithmetic', help='Number of arithmetic expressions', type=int, required=True)
parser.add_argument('--returns', help='Number of return statements', type=int, required=True)
parser.add_argument('--cout', help='Number of console output statements', type=int)
args = parser.parse_args()

# Get LVP from args
lvp = None

try:
    lvp = LVP[args.lvp.upper()]
except Exception:
    print(f'Unknown language-version pair "{args.lvp}".')
    raise Exception()

# Generate data
if lvp == LVP.CPP_17_TO_NODEJS_14:
    data = Cpp17ToNodeJS14Generator.generate(args)
elif lvp == LVP.JAVA_14_TO_NODEJS_14:
    data = Java14ToNodeJS14Generator.generate(args)
else:
    raise Exception(f'Unimplemented language-version pair "{lvp.value}".')

# Deduplicate data
data = deduplicate(data)

# Output data to CSV file
to_csv(data, args.out)

print('Output to {}'.format(args.out))
