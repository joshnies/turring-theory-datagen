import random

from theory_data_gen.common import gen_item
from theory_data_gen.common.java import JAVA_ACCESS_MODIFIERS
from theory_data_gen.constants import MASK_TOKEN

# Java 14 --> Python 3 type map
TYPE_MAP = {
    MASK_TOKEN: 'Any',
    'var': 'Any',
    'void': 'None',
    'boolean': 'Bool',
    'byte': 'bytes',
    'short': 'int',
    'int': 'int',
    'float': 'float',
    'double': 'float',
    'long': 'float',
    'char': 'chr',
    'Void': 'None',
    'Boolean': 'bool',
    'Byte': 'bytes',
    'Short': 'int',
    'Integer': 'int',
    'Float': 'float',
    'Double': 'float',
    'Long': 'float',
    'Character': 'chr',
    'String': 'str',
    'File': 'Any',
    'Exception': 'Exception',
    'AbsentInformationException': 'Exception',
    'ActivationException': 'Exception',
    'AgentInitializationException': 'Exception',
    'AgentLoadException': 'Exception',
    'AlreadyBoundException': 'Exception',
    'AttachNotSupportedException': 'Exception',
    'AWTException': 'Exception',
    'BackingStoreException': 'Exception',
    'BadAttributeValueExpException': 'Exception',
    'BadBinaryOpValueExpException': 'Exception',
    'BadLocationException': 'Exception',
    'BadStringOperationException': 'Exception',
    'BrokenBarrierException': 'Exception',
    'CardException': 'Exception',
    'CertificateException': 'Exception',
    'ClassNotLoadedException': 'Exception',
    'CloneNotSupportedException': 'Exception',
    'DataFormatException': 'Exception',
    'DatatypeConfigurationException': 'Exception',
    'DestroyFailedException': 'Exception',
    'ExecutionControl.ExecutionControlException': 'Exception',
    'ExecutionException': 'Exception',
    'ExpandVetoException': 'Exception',
    'FontFormatException': 'Exception',
    'GeneralSecurityException': 'Exception',
    'GSSException': 'Exception',
    'IllegalClassFormatException': 'Exception',
    'IllegalConnectorArgumentsException': 'Exception',
    'IncompatibleThreadStateException': 'Exception',
    'InterruptedException': 'Exception',
    'IntrospectionException': 'Exception',
    'InvalidApplicationException': 'Exception',
    'InvalidMidiDataException': 'Exception',
    'InvalidPreferencesFormatException': 'Exception',
    'InvalidTargetObjectTypeException': 'Exception',
    'InvalidTypeException': 'Exception',
    'InvocationException': 'Exception',
    'IOException': 'IOError',
    'JMException': 'Exception',
    'JShellException': 'Exception',
    'KeySelectorException': 'Exception',
    'LambdaConversionException': 'Exception',
    'LineUnavailableException': 'Exception',
    'MarshalException': 'Exception',
    'MidiUnavailableException': 'Exception',
    'MimeTypeParseException': 'Exception',
    'NamingException': 'NameError',
    'NoninvertibleTransformException': 'Exception',
    'NotBoundException': 'Exception',
    'ParseException': 'Exception',
    'ParserConfigurationException': 'Exception',
    'PrinterException': 'Exception',
    'PrintException': 'Exception',
    'PrivilegedActionException': 'Exception',
    'PropertyVetoException': 'Exception',
    'ReflectiveOperationException': 'Exception',
    'RefreshFailedException': 'Exception',
    'RuntimeException': 'RuntimeError',
    'SAXException': 'Exception',
    'ScriptException': 'Exception',
    'ServerNotActiveException': 'Exception',
    'SQLException': 'Exception',
    'StringConcatException': 'Exception',
    'TimeoutException': 'TimeoutError',
    'TooManyListenersException': 'Exception',
    'TransformerException': 'Exception',
    'TransformException': 'Exception',
    'UnmodifiableClassException': 'Exception',
    'UnsupportedAudioFileException': 'Exception',
    'UnsupportedCallbackException': 'Exception',
    'UnsupportedFlavorException': 'Exception',
    'UnsupportedLookAndFeelException': 'Exception',
    'URIReferenceException': 'Exception',
    'URISyntaxException': 'Exception',
    'VMStartException': 'Exception',
    'XAException': 'Exception',
    'XMLParseException': 'Exception',
    'XMLSignatureException': 'Exception',
    'XMLStreamException': 'Exception',
    'XPathException': 'Exception'
}

# Java 14 --> Python 3 generic type map
GENERIC_TYPE_MAP = {
    'Array': 'list',
    'ArrayList': 'list',
    'List': 'list',
    'LinkedList': 'list',
    'Set': 'set',
    'HashSet': 'set',
    'LinkedHashSet': 'set',
    'Map': 'dict',
    'HashMap': 'dict',
    'ConcurrentHashMap': 'dict',
    'IdentityHashMap': 'dict',
    'LinkedHashMap': 'dict',
    'WeakHashMap': 'dict'
}


def gen_java_generic_type():
    """Generates a random Java type with generic arguments."""

    types = list(GENERIC_TYPE_MAP.keys())
    types.append(MASK_TOKEN)

    base_type = random.choice(types)
    args = []

    # Generate generic args
    for i in range(1, 5):
        arg_types = list(TYPE_MAP.keys())
        arg_types.append(MASK_TOKEN)
        args.append(random.choice(arg_types))

    args = ', '.join(args)

    return f'{base_type}<{args}>'


def gen_modifier_permutations(item, include_abstract=True, include_static=True, is_method=True):
    """
    Generate permutations of an item with prefixed modifiers (such as access modifiers).

    :param item: Item.
    :param include_abstract: Whether to generate permutations with "abstract" modifiers.
    :param include_static: Whether to generate permutations with "static" modifiers.
    :param is_method: Whether the item is a method.

    :returns: List of item permutations with modifiers.
    """

    src = item['source']
    tar = item['target']
    tar_static = '@staticmethod ' if is_method else ''
    new_items = list()

    mods = JAVA_ACCESS_MODIFIERS.copy()
    mods.append('')

    for m in mods:
        new_items.append((f'{m} {src}', tar))

        if include_abstract:
            new_items.append((f'{m} abstract {src}', tar))

            if include_static:
                new_items.append((f'{m} static abstract {src}', f'{tar_static}{tar}'))
                new_items.append((f'{m} abstract static {src}', f'{tar_static}{tar}'))

        if include_static:
            new_items.append((f'{m} static {src}', f'{tar_static}{tar}'))

    new_items = list(map(lambda i: gen_item(i[0].strip(), i[1].strip()), new_items))

    return new_items
