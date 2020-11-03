def join(iterator, separator):
    """Join items in an iterator with a given separator."""

    it = map(str, iterator)
    separator = str(separator)
    string = next(it, '')
    for s in it:
        string += separator + s
    return string
