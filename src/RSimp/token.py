from enum import Enum

class TokenType(Enum):
    # Arithmetic Operators
    PLUS          = '+'
    MINUS         = '-'
    MUL           = '*'
    FLOAT_DIV     = '/'
    POWER         = '^'
    MODULO        = '%'

    # Relational Operators
    GREATER_THAN  = '>'
    LESS_THAN     = '<'
    EQUALITY      = '=='
    LESS_OR_EQUAL = '<='
    GREAT_OR_EQUAL= '>='
    NOT_EQUAL     = '!='

    # Logical Operators
    AND           = '&'
    OR            = '|'
    NOT           = '!'
    LOGIC_AND     = '&&'
    LOGIC_OR      = '||'

    # Assignment Operators
    ASSIGN        = '='

    # Syntax
    VECTOR        = 'c'
    LPAREN        = '('
    RPAREN        = ')'
    SEMI          = ';'
    DOT           = '.'
    COLON         = ':'
    COMMA         = ','
    LCURLY        = '{'
    RCURLY        = '}'
    STRDOUBLE     = '"'
    STRSINGLE     = "'"      # marks the end of the block

    # Control Structures
    IF            = 'IF'
    ELSE          = 'ELSE'
    WHILE         = 'WHILE'
    FOR           = 'FOR'

    # block of reserved words
    NUMERIC       = 'NUMERIC'
    BOOLTRUE      = 'TRUE'
    BOOLFALSE     = 'FALSE'
    FUNCTION      = 'function'

    # misc
    ID            = 'ID'
    IN            = 'in'
    EOF           = 'EOF'


class Token:
    def __init__(self, type, value, lineno=None, column=None):
        self.type = type
        self.value = value
        self.lineno = lineno
        self.column = column

    def __str__(self):
        """String representation of the class instance.
        Example:
            >>> Token(TokenType.INTEGER, 7, lineno=5, column=10)
            Token(TokenType.INTEGER, 7, position=5:10)
        """
        return 'Token({type}, {value}, position={lineno}:{column})'.format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self):
        return self.__str__()


def _build_reserved_keywords():
    """Build a dictionary of reserved keywords.
    The function relies on the fact that in the TokenType
    enumeration the beginning of the block of reserved keywords is
    marked with IF and the end of the block is marked with
    the EOF keyword.
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.IF)
    end_index = tt_list.index(TokenType.EOF)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index + 1]
    }
    return reserved_keywords


RESERVED_KEYWORDS = _build_reserved_keywords()

MAX_STRING_LENGTH = 65535