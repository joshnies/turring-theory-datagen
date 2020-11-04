import csv

CSV_COLUMNS = ['source', 'target']


def to_csv(arrs, file_path):
    """Output CSV file."""

    # Merge data
    data = []

    for a in arrs:
        data.extend(a)

    try:
        with open(file_path, 'w', newline='') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=CSV_COLUMNS)
            writer.writeheader()

            for d in data:
                writer.writerow(d)
    except IOError:
        print('I/O Error while writing CSV file.')
