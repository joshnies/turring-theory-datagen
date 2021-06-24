import csv
import os
import random

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


def split_dataset(file_path: str, split: int):
    """
    Split dataset between training and validation.

    :param file_path: Dataset file path.
    :param split: Split percentage.
    """

    lines = open(file_path).readlines()
    header = lines[0]
    lines = lines[1:]
    random.shuffle(lines)
    line_count = len(lines)
    split_idx = int(line_count * (1 - split))

    train_lines = [header]
    train_lines.extend(lines[:split_idx - 1])
    valid_lines = [header]
    valid_lines.extend(lines[split_idx:])

    open(f'{file_path[:-4]}_train.csv', 'w').writelines(train_lines)
    open(f'{file_path[:-4]}_valid.csv', 'w').writelines(valid_lines)


def output_json_from_csv(file_path: str, delete_csv: bool = False):
    """
    Create JSON file from CSV file.

    :param file_path: CSV file name.
    :param delete_csv: Whether to delete the CSV file afterwards.
    """

    # Open new JSON file for writing
    with open(f'{file_path[:-4]}.json', 'a') as json_file:
        # Add open bracket
        json_file.write('{\n\t')

        # Open CSV file for reading
        with open(file_path, newline='') as csv_file:
            csv_reader = csv.reader(csv_file)
            next(csv_reader)  # Skip header

            # Write rows to JSON file
            for row in csv_reader:
                json_file.write(f'"{row[0]}": "{row[1]}",\n\t')

            # Add closing bracket
            json_file.write('}')

    # Delete CSV file
    if delete_csv:
        os.remove(file_path)
