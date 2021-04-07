class Generator:
    """Base class for data generator."""

    @staticmethod
    def generate_random(args, write):
        """
        Generate random data for this LVP.

        :param args: Command line arguments.
        :param write: Function called to write to the output file.
        """
        pass

    @staticmethod
    def generate_required(args, write):
        """
        Generate required data for this LVP.

        :param args: Command line arguments.
        :param write: Function called to write to the output file.
        """
        pass
