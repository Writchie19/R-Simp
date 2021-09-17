from os import error, initgroups
from src.RSimp import token
from src.RSimp.errors import ErrorCode, ParserError
from src.RSimp.token import Token, TokenType 

class AST:
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Bool(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Str(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Break(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Continue(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class CVector(AST):
    def __init__(self, token, value):
        self.token = token
        self.value = value

class Vector(AST):
    def __init__(self, token, start, end):
        self.token = token
        self.start = start
        self.end = end

class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Compound(AST):
    """Represents a '{ ... }' block"""
    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token, index):
        self.token = token
        self.value = token.value
        self.index = index


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, block):
        self.block = block


class Block(AST):
    def __init__(self, compound_statement):
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Param(AST):
    def __init__(self, var_node):
        self.var_node = var_node

class IfStatement(AST):
    """Represents an if statement: if (...) {...} """
    def __init__(self, token):
        self.token = token
        self.bool_nodes = []
        self.compound_nodes = []

class ForLoop(AST):
    """Represents a for loop: for (...) {...}"""
    def __init__(self, token, var_node,vector_node, compound_statement):
        self.token = token
        self.var_node = var_node
        self.vector_node = vector_node
        self.compound_statement = compound_statement

class WhileLoop(AST):
    def __init__(self, token, expression_node, compound_statement):
        self.token = token
        self.expr_node = expression_node
        self.compound_statement = compound_statement

class ProcedureDecl(AST):
    def __init__(self, proc_name, formal_params, compound_statement):
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.compound_statement = compound_statement


class ProcedureCall(AST):
    def __init__(self, proc_name, actual_params, token):
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol = None


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()
        self.previous_token = None

    def get_next_token(self):
        return self.lexer.get_next_token()

    def error(self, error_code, token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.previous_token = self.current_token
            self.current_token = self.get_next_token()
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

    def block(self):
        """block :  compound_statement"""
        compound_statement_node = self.compound_statement()
        node = Block(compound_statement_node)
        return node

    def first_pass(self):
        block_node = self.initial_block()
        program_node = Program(block_node)
        self.eat(TokenType.EOF)
        return program_node

    def initial_block(self):
        init_compound_statement_node = self.initial_compound_statement()
        node = Block(init_compound_statement_node)
        return node

    def initial_compound_statement(self):
        """compound_statement: { statement_list }"""
        nodes = self.statement_list()

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def formal_parameters(self):
        """ formal_parameters : ID (COMMA ID)* """
        param_nodes = []

        param_tokens = [self.current_token]
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            if self.current_token.type == TokenType.VECTOR and self.current_token.value == 'c':
                self.current_token.type = TokenType.ID
            param_tokens.append(self.current_token)
            self.eat(TokenType.ID)

        for param_token in param_tokens:
            param_node = Param(Var(param_token, None))
            param_nodes.append(param_node)

        return param_nodes

    def formal_parameter_list(self):
        """ formal_parameter_list : formal_parameters
                                  | formal_parameters SEMI formal_parameter_list
        """
        # procedure Foo();
        if not self.current_token.type == TokenType.ID:
            return []

        param_nodes = self.formal_parameters()

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            param_nodes.extend(self.formal_parameters())

        return param_nodes

    # def variable_declaration(self):
    #     """variable_declaration : ID (COMMA ID)* COLON type_spec"""
    #     var_nodes = [Var(self.current_token)]  # first ID
    #     self.eat(TokenType.ID)

    #     while self.current_token.type == TokenType.COMMA:
    #         self.eat(TokenType.COMMA)
    #         var_nodes.append(Var(self.current_token))
    #         self.eat(TokenType.ID)

    #     self.eat(TokenType.COLON)

    #     type_node = self.type_spec()
    #     var_declarations = [
    #         VarDecl(var_node, type_node)
    #         for var_node in var_nodes
    #     ]
    #     return var_declarations

    def function_declaration(self, id):
        """procedure_declaration :
            FUNCTION LPAREN formal_parameter_list RPAREN LCURLY compound_statement RCURLY
        """
        if id is not None:
            self.eat(TokenType.FUNCTION)
            proc_name = id+'_1'
            formal_params = []

            if self.current_token.type == TokenType.LPAREN:
                self.eat(TokenType.LPAREN)
                formal_params = self.formal_parameters()
                self.eat(TokenType.RPAREN)

            node = self.compound_statement()
            proc_decl = ProcedureDecl(proc_name, formal_params, node)
            return proc_decl

    def program(self):
        block_node = self.block()
        program_node = Program(block_node)
        self.eat(TokenType.EOF)
        return program_node

    # def program(self):
    #     """program : PROGRAM variable SEMI block DOT"""
    #     self.eat(TokenType.PROGRAM)
    #     var_node = self.variable()
    #     prog_name = var_node.value
    #     self.eat(TokenType.SEMI)
    #     block_node = self.block()
    #     program_node = Program(prog_name, block_node)
    #     self.eat(TokenType.DOT)
    #     return program_node

    # def type_spec(self):
    #     """type_spec : INTEGER
    #                  | REAL
    #     """
    #     token = self.current_token
    #     if self.current_token.type == TokenType.INTEGER:
    #         self.eat(TokenType.INTEGER)
    #     else:
    #         self.eat(TokenType.REAL)
    #     node = Type(token)
    #     return node

    def compound_statement(self):
        """
        compound_statement: { statement_list }
        """
        self.eat(TokenType.LCURLY)
        nodes = self.statement_list()
        self.eat(TokenType.RCURLY)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def if_statement(self):
        """if_statement :
            IF LPAREN expr RPAREN LCURLY compound RCURLY
        """
        root = IfStatement(self.current_token)
        if self.current_token.type == TokenType.IF:
            self.eat(TokenType.IF)
            self.eat(TokenType.LPAREN)
            root.bool_nodes.append(self.expr())
            self.eat(TokenType.RPAREN)
            root.compound_nodes.append(self.compound_statement())

            while self.current_token.type == TokenType.ELSE:
                self.eat(TokenType.ELSE)
                if self.current_token.type == TokenType.IF:
                    self.eat(TokenType.IF)
                    self.eat(TokenType.LPAREN)
                    root.bool_nodes.append(self.expr())
                    self.eat(TokenType.RPAREN)
                    root.compound_nodes.append(self.compound_statement())
                else:
                    root.bool_nodes.append(Bool(TokenType.BOOLTRUE))
                    root.compound_nodes.append(self.compound_statement())
        
        return root

    def for_loop(self):
        """ for_loop:
            FOR LPAREN ID IN (VECTOR | CVECTOR | ID) RPAREN LCURLY compound_statement RCURLY
        """
        token = self.current_token
        self.eat(TokenType.FOR)
        self.eat(TokenType.LPAREN)

        var_node = self.variable()
        vector_node = None

        self.eat(TokenType.IN)
        if self.current_token.type is TokenType.VECTOR:
            vector_node = self.vector()
        elif self.current_token.type is TokenType.CVECTOR:
            vector_node = self.c_vector()
        elif self.current_token.type is TokenType.ID:
            vector_node = self.variable()

        self.eat(TokenType.RPAREN)
        compound_node = self.compound_statement()
        return ForLoop(token,var_node, vector_node, compound_node)

    def while_loop(self):
        """
            WHOLE LPAREN expr RPAREN LCURLY compound_statement RCURLY
        """
        token = self.current_token
        self.eat(TokenType.WHILE)
        self.eat(TokenType.LPAREN)
        
        node = self.expr()
        self.eat(TokenType.RPAREN)
        compound_node = self.compound_statement()
        return WhileLoop(token, node, compound_node)

    def statement_list(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type == TokenType.SEMI or self.current_token.type == TokenType.RCURLY or self.previous_token.type == TokenType.RCURLY:
            if self.current_token.type == TokenType.SEMI: 
                self.eat(TokenType.SEMI)
                if self.current_token.type == TokenType.RCURLY: break
            elif self.current_token.type == TokenType.RCURLY:
                if self.previous_token.type == TokenType.RCURLY:
                    break
                elif self.previous_token.type != TokenType.SEMI:
                    self.error(
                        error_code=ErrorCode.UNEXPECTED_TOKEN,
                        token=self.previous_token.type,
                    )

            results.append(self.statement())

        return results

    def statement(self):
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | empty
        """
        if self.current_token.type == TokenType.LCURLY:
            node = self.compound_statement()
        elif (self.current_token.type is TokenType.ID and
              self.lexer.current_char == '('
        ):
            node = self.function_call_statement()
        elif self.current_token.type is TokenType.ID:
            node = self.assignment_statement()
        elif self.current_token.type is TokenType.IF:
            node = self.if_statement()
        elif self.current_token.type is TokenType.FOR:
            node = self.for_loop()
        elif self.current_token.type is TokenType.WHILE:
            node = self.while_loop()
        elif self.current_token.type is TokenType.BREAK:
            node = Break(self.current_token)
            self.eat(TokenType.BREAK)
        elif self.current_token.type is TokenType.CONTINUE:
            node = Continue(self.current_token)
            self.eat(TokenType.CONTINUE)
        else:
            node = self.empty()
        return node

    def function_call_statement(self):
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value + '_1'
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params = []
        if self.current_token.type != TokenType.RPAREN:
            node = self.expr()
            actual_params.append(node)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            node = self.expr()
            actual_params.append(node)

        self.eat(TokenType.RPAREN)

        node = ProcedureCall(
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        right = None
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr(left.value)

        node = Assign(left, token, right)
        return node

    def variable(self):
        """
        variable : ID
        """
        node = Var(self.current_token, None)
        self.eat(TokenType.ID)
        
        if self.current_token.type is TokenType.LBRACKET:
            self.eat(TokenType.LBRACKET)
            node.index = self.expr()
            self.eat(TokenType.RBRACKET)

        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def c_vector(self):
        """
        vector : (STR, STR ,STR ...) | (NUM, NUM, NUM ...) | (BOOL, BOOL, BOOL ...)
        """
        params = []
        token = self.current_token
        self.eat(TokenType.CVECTOR)
        self.eat(TokenType.LPAREN)

        # Needs to come after eating vector and lparen to save the input type for the vector
        node = self.expr()
        params.append(node)

        while self.current_token.type is TokenType.COMMA:
            self.eat(TokenType.COMMA)
            params.append(self.expr())
    
        self.eat(TokenType.RPAREN)
        return CVector(token, params)

    def vector(self):
        """
        vector : NUM:NUM
        """
        token = self.current_token
        self.eat(TokenType.VECTOR)
        start_token = token
        start_token.type = TokenType.NUMERIC
        start_node = Num(start_token)
        self.eat(TokenType.COLON)
        end_node = Num(self.current_token)
        self.eat(TokenType.NUMERIC)
        return Vector(token, start_node, end_node)

    def expr(self, id = None):
        """
        expr : term ((PLUS | MINUS | LESS_THAN | LESS_OR_EQUAL | GREAT_OR_EQUAL | GREATER_THAN | NOT_EQUAL | EQUALITY | LOGIC_OR | LOGIC_AND | AND | OR) term)*
        """
        node = self.term(id)

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS, TokenType.LESS_THAN, TokenType.LESS_OR_EQUAL, TokenType.GREAT_OR_EQUAL, TokenType.GREATER_THAN, TokenType.NOT_EQUAL, TokenType.EQUALITY, TokenType.LOGIC_OR, TokenType.LOGIC_AND, TokenType.AND, TokenType.OR):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)
            elif token.type == TokenType.LESS_THAN:
                self.eat(TokenType.LESS_THAN)
            elif token.type == TokenType.LESS_OR_EQUAL:
                self.eat(TokenType.LESS_OR_EQUAL)
            elif token.type == TokenType.GREAT_OR_EQUAL:
                self.eat(TokenType.GREAT_OR_EQUAL)
            elif token.type == TokenType.GREATER_THAN:
                self.eat(TokenType.GREATER_THAN)
            elif token.type == TokenType.NOT_EQUAL:
                self.eat(TokenType.NOT_EQUAL)
            elif token.type == TokenType.EQUALITY:
                self.eat(TokenType.EQUALITY)
            elif token.type == TokenType.LOGIC_OR:
                self.eat(TokenType.LOGIC_OR)
            elif token.type == TokenType.LOGIC_AND:
                self.eat(TokenType.LOGIC_AND)
            elif token.type == TokenType.AND:
                self.eat(TokenType.AND)
            elif token.type == TokenType.OR:
                self.eat(TokenType.OR)

            node = BinOp(left=node, op=token, right=self.term(id))

        return node        

    def term(self, id = None):
        """term : factor ((MUL | FLOAT_DIV | MODULO | POWER) factor)*"""
        node = self.factor(id)

        while self.current_token.type in (
                TokenType.MUL,
                TokenType.FLOAT_DIV,
                TokenType.MODULO,
                TokenType.POWER
        ):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)
            elif token.type == TokenType.MODULO:
                self.eat(TokenType.MODULO)
            elif token.type == TokenType.POWER:
                self.eat(TokenType.POWER)

            node = BinOp(left=node, op=token, right=self.factor(id))

        return node

    def factor(self, id = None):
        """factor : PLUS factor
                  | MINUS factor
                  | INTEGER_CONST
                  | BOOL
                  | LPAREN expr RPAREN
                  | VECTOR
                  | variable
        """
        token = self.current_token
        node = None
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
        elif token.type == TokenType.NUMERIC:
            self.eat(TokenType.NUMERIC)
            node =  Num(token)               
        elif token.type == TokenType.BOOLTRUE:
            self.eat(TokenType.BOOLTRUE)
            node =  Bool(token)
        elif token.type == TokenType.BOOLFALSE:
            self.eat(TokenType.BOOLFALSE)
            node =  Bool(token)
        elif token.type == TokenType.STRSINGLE:
            self.eat(TokenType.STRSINGLE)
            node =  Str(token)
        elif token.type == TokenType.STRDOUBLE:
            self.eat(TokenType.STRDOUBLE)
            node = Str(token)
        elif token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
        elif token.type == TokenType.CVECTOR:
            node = self.c_vector()
        elif token.type == TokenType.VECTOR:
            node = self.vector()
        elif token.type == TokenType.FUNCTION:
            node = self.function_declaration(id)
        else:
            node = self.variable()

        return node

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT
        block : declarations compound_statement
        declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration*
        variable_declaration : ID (COMMA ID)* COLON type_spec
        procedure_declaration :
             PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list
        formal_parameters : ID (COMMA ID)* COLON type_spec
        type_spec : INTEGER | REAL
        compound_statement : BEGIN statement_list END
        statement_list : statement
                       | statement SEMI statement_list
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | empty
        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN
        assignment_statement : variable ASSIGN expr
        empty :
        expr : term ((PLUS | MINUS) term)*
        term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
        factor : PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | REAL_CONST
               | LPAREN expr RPAREN
               | variable
        variable: ID
        """
        node = self.first_pass()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node


###############################################################################
#                                                                             #
#  AST visitors (walkers)                                                     #
#                                                                             #
###############################################################################

class NodeVisitor:
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))
