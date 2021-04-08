class Generator:
    """Base class for data generator."""

    @staticmethod
    def generate_datasets(args, write):
        """
        Generate random data for this LVP's datasets.

        :param args: Command line arguments.
        :param write: Function called to write to the output file.
        """
        pass

    @staticmethod
    def generate_map_data(args, write):
        """
        Generate finite amount of data for this LVP that will be added to the data map file.

        :param args: Command line arguments.
        :param write: Function called to write to the output file.
        """
        pass
