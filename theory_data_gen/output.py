import csv

from tqdm import tqdm

CSV_COLUMNS = ['source', 'target']


def create_output_file(file_path):
    """Create output CSV file."""

    try:
        with open(file_path, 'w', newline='') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=CSV_COLUMNS)
            writer.writeheader()
    except IOError:
        print('I/O Error while creating output file.')
