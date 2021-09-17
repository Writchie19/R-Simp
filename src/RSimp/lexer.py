from src.RSimp.parser import Vector
from src.RSimp.errors import LexerError
from src.RSimp.token import MAX_STRING_LENGTH, TokenType, Token, RESERVED_KEYWORDS

class Lexer:
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == '\n':
            self.lineno += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.column += 1

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def peekSkipWhitespace(self):
        peek_pos = self.pos + 1
        while peek_pos <= len(self.text) - 1:
            if not self.text[peek_pos].isspace():
                return self.text[peek_pos]
            peek_pos += 1
            
    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '#' and self.current_char != '\n':
            self.advance()
        self.advance()  # the closing curly brace

    def create_number(self):
        """Return a (multidigit) integer or float consumed from the input."""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
        self.skip_whitespace()
        if (self.current_char == ':'):
            token.type = TokenType.VECTOR
        else:
            token.type = TokenType.NUMERIC
        token.value = float(result)

        return token

    def create_string(self, token_type):
        # Create a new token with current line and column number
        token = Token(type=token_type, value=None, lineno=self.lineno, column=self.column)
        result = ''
        count = 0
        while self.current_char is not None and self.current_char != token_type.value and count <= MAX_STRING_LENGTH:
            result += self.current_char
            count += 1
            self.advance()

        if count >= MAX_STRING_LENGTH:
            while self.current_char is not None and self.current_char != token_type:
                self.advance
            result += self.current_char
        else:
            self.advance()

        token.value = result
        return token

    def _id(self):
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ''
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(value.upper())
        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()

        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '#':
                self.advance()
                self.skip_comment()
                continue

            # Check for vector initialization c(...)
            if self.current_char == 'c':
                # Create a new token with current line and column number
                token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

                if self.peekSkipWhitespace() != '(':
                    return self._id()
                
                token.type = TokenType.CVECTOR
                token.value = TokenType.CVECTOR.value
                self.advance()
                return token

            if self.current_char.isalpha():
                return self._id()

            if self.current_char.isdigit():
                return self.create_number()

            if self.current_char == '"':
                self.advance()
                return self.create_string(TokenType.STRDOUBLE)

            if self.current_char == "'":
                self.advance()
                return self.create_string(TokenType.STRSINGLE)

            if self.current_char == '<' and self.peek() == '-':
                token = Token(
                    type=TokenType.ASSIGN,
                    value=TokenType.ASSIGN.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char == '<' and self.peek() == '=':
                token = Token(
                    type=TokenType.LESS_OR_EQUAL,
                    value=TokenType.LESS_OR_EQUAL.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char == '>' and self.peek() == '=':
                token = Token(
                    type=TokenType.GREAT_OR_EQUAL,
                    value=TokenType.GREAT_OR_EQUAL.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char == '=' and self.peek() == '=':
                token = Token(
                    type=TokenType.EQUALITY,
                    value=TokenType.EQUALITY.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char == '!' and self.peek() == '=':
                token = Token(
                    type=TokenType.NOT_EQUAL,
                    value=TokenType.NOT_EQUAL.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            #TODO this does not work for !ID fix it
            if self.current_char == '!' and self.peek().isalpha():
                self.advance()
                token = self._id()
                if token.type == TokenType.BOOLTRUE:
                    token.type = TokenType.BOOLFALSE
                    token.value = TokenType.BOOLFALSE.value
                elif token.type == TokenType.BOOLFALSE:
                    token.type = TokenType.BOOLTRUE
                    token.value = TokenType.BOOLTRUE.value                    
                else :
                    self.error()
                return token

            if self.current_char == '&' and self.peek() == '&':
                token = Token(
                    type=TokenType.LOGIC_AND,
                    value=TokenType.LOGIC_AND.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char == '|' and self.peek() == '|':
                token = Token(
                    type=TokenType.LOGIC_OR,
                    value=TokenType.LOGIC_OR.value,
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)