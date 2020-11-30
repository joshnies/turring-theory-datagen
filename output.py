import csv

from tqdm import tqdm

CSV_COLUMNS = ['source', 'target']


def to_csv(data, file_path):
    """Output CSV file."""

    try:
        with open(file_path, 'w', newline='') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=CSV_COLUMNS)
            writer.writeheader()

            for d in tqdm(data, desc='Writing to CSV file'):
                writer.writerow(d)
    except IOError:
        print('I/O Error while writing CSV file.')
