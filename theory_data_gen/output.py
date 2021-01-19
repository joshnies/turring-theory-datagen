import csv
import os

CSV_COLUMNS = ['source', 'target']


def create_output_file(file_path):
    """
    Create output CSV file.

    :param file_path: File path.

    :returns: Tuple of:
        - File setup for appending writes.
        - Function that, when called, writes the given data to the output file.
    """

    try:
        with open(file_path, 'w', newline='') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=CSV_COLUMNS)
            writer.writeheader()
    except IOError:
        print('I/O Error while creating output file.')

    file_for_append = open(file_path, 'a', newline='')
    writer = csv.DictWriter(file_for_append, fieldnames=CSV_COLUMNS)
    return file_for_append, writer.writerow


def deduplicate_lines(input_file_path: str, output_file_path: str):
    """
    Remove duplicate lines from file.

    :param input_file_path: Input file path.
    :param output_file_path: Output file path.
    """

    print('Removing duplicates...')

    lines = set()

    with open(output_file_path, 'w', newline='') as output_file:
        for l in open(input_file_path, 'r'):
            if l not in lines:
                output_file.write(l)
                lines.add(l)

    # Delete input file
    os.remove(input_file_path)
