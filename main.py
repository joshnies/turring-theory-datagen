import argparse
import csv

from theory_data_gen.lvp import LVP
from theory_data_gen.output import create_output_file, deduplicate_lines, CSV_COLUMNS
from theory_data_gen.base.cpp_17_to_nodejs_14.generator import Cpp17ToNodeJS14Generator
from theory_data_gen.base.java_14_to_nodejs_14.generator import Java14ToNodeJS14Generator
from theory_data_gen.base.java_14_to_python_3.generator import Java14ToPython3Generator
from theory_data_gen.cases.jdereg.java_util.nodejs_14.generator import JderegJavaUtilGenerator

# Parse args
parser = argparse.ArgumentParser(description='Generate Theory dataset.')
parser.add_argument('-l', '--lvp', help='Language-version pair', required=True)
parser.add_argument('-c', '--case', help='Case module used for additional data generation')
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

case_name = args.case.lower() if args.case is not None else None

# Create output file
og_file_name = f'{args.out}_DUP'
write_func = create_output_file(og_file_name)

# Generate base LVP data
if lvp == LVP.CPP_17_TO_NODEJS_14:
    Cpp17ToNodeJS14Generator.generate(args, write_func)
elif lvp == LVP.JAVA_14_TO_NODEJS_14:
    Java14ToNodeJS14Generator.generate(args, write_func)
elif lvp == LVP.JAVA_14_TO_PYTHON_3:
    Java14ToPython3Generator.generate(args, write_func)
else:
    raise Exception(f'Unimplemented language-version pair "{lvp.value}".')

# Remove duplicated lines
# NOTE: Deduplication is performed before case data generation due to a bug with file writing.
deduplicate_lines(og_file_name, args.out)

# Generate case data
case_writer = csv.DictWriter(open(args.out, 'a', newline=''), fieldnames=CSV_COLUMNS)

if case_name == 'jdereg/java-util':
    JderegJavaUtilGenerator.generate(args, case_writer.writerow)
elif case_name is not None:
    raise Exception(f'Unimplemented case "{case_name}".')

print('Output to {}'.format(args.out))
