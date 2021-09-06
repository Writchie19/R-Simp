from enum import Enum
from src.RSimp.token import Token, TokenType
from src.RSimp.parser import NodeVisitor, Num
from src.RSimp import cmd_line_globals

class ARType(Enum):
    PROGRAM = 'PROGRAM'
    FUNCTION = 'FUNCTION'


class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def peek(self):
        return self._records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n\n'
        return s

    def __repr__(self):
        return self.__str__()


class ActivationRecord:
    def __init__(self, type, nesting_level):
        self.type = type
        self.nesting_level = nesting_level
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key)
    
    def remove(self, key):
        return self.members.pop(key, None)

    def __str__(self):
        lines = [
            '{level}: {type}'.format(
                level=self.nesting_level,
                type=self.type.value,
            )
        ]
        for name, val in self.members.items():
            lines.append(f'   {name:<20}: {val}')

        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.call_stack = CallStack()
        self.can_break_or_continue = False
        self.is_break = False

    def log(self, msg):
        if cmd_line_globals._SHOULD_LOG_STACK:
            print(msg)

    def visit_Program(self, node):
        ar = ActivationRecord(
            type=ARType.PROGRAM,
            nesting_level=1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Block(self, node):
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        # Do nothing
        pass

    def visit_Type(self, node):
        # Do nothing
        pass

    def visit_BinOp(self, node):
        if node.op.type == TokenType.PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == TokenType.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == TokenType.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))
        elif node.op.type == TokenType.MODULO:
            return float(self.visit(node.left)) % float(self.visit(node.right))
        elif node.op.type == TokenType.POWER:
            return float(self.visit(node.left)) ** float(self.visit(node.right))
        elif node.op.type == TokenType.LESS_THAN:
            return float(self.visit(node.left)) < float(self.visit(node.right))
        elif node.op.type == TokenType.LESS_OR_EQUAL:
            return float(self.visit(node.left)) <= float(self.visit(node.right))
        elif node.op.type == TokenType.GREAT_OR_EQUAL:
            return float(self.visit(node.left)) >= float(self.visit(node.right))
        elif node.op.type == TokenType.GREATER_THAN:
            return float(self.visit(node.left)) > float(self.visit(node.right))
        elif node.op.type == TokenType.NOT_EQUAL:
            return self.visit(node.left) != self.visit(node.right)
        elif node.op.type == TokenType.EQUALITY:
            return self.visit(node.left) == self.visit(node.right)
        elif node.op.type == TokenType.AND:
            return int(self.visit(node.left)) & int(self.visit(node.right))
        elif node.op.type == TokenType.OR:
            return int(self.visit(node.left)) | int(self.visit(node.right))
        elif node.op.type == TokenType.LOGIC_OR:
            return self.visit(node.left) or self.visit(node.right)
        elif node.op.type == TokenType.LOGIC_AND:
            return self.visit(node.left) and self.visit(node.right)

    def visit_Num(self, node):
        return node.value

    def visit_Bool(self, node):
        if (node.value == TokenType.BOOLTRUE.value):
            return True
        elif (node.value == TokenType.BOOLFALSE.value):
            return False
        else:
            return None

    def visit_Str(self, node):
        return node.value

    def visit_Break(self, node):
        pass

    def visit_Continue(self, node):
        pass

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == TokenType.PLUS:
            return +self.visit(node.expr)
        elif op == TokenType.MINUS:
            return -self.visit(node.expr)

    def visit_Compound(self, node):
        for child in node.children:
            if self.can_break_or_continue and child.token.type is TokenType.BREAK:
                self.is_break = True
                return
            elif self.can_break_or_continue and child.token.type is TokenType.CONTINUE:
                return
            self.visit(child)

    def visit_Assign(self, node):
        var_name = node.left.value
        var_value = self.visit(node.right)
        index = None
        if node.left.index != None:
            index = self.visit(node.left.index)

        ar = self.call_stack.peek()
        if index != None:
            init_type = type(ar[var_name][0])
            primary_type = self.find_primetype(init_type, var_value)
            if not isinstance(init_type, primary_type):
                ar[var_name] = list(map(primary_type, ar[var_name]))

            ar[var_name][self.convert_to_index(index)] = primary_type(var_value)
        else:
            ar[var_name] = var_value

    def visit_CVector(self, node):
        vector_value = []
        primary_type = None # used to ensure all of the vector items are the appropriate type

        # c(token, token, token ...)
        for token in node.value:
            token_value = self.visit(token)
            if isinstance(token_value, list):
                for item in token_value:
                    vector_value.append(item)
                    primary_type = self.find_primetype(primary_type, item)
            else:
                vector_value.append(token_value)

            primary_type = self.find_primetype(primary_type, token_value)

        vector_value = list(map(primary_type, vector_value))

        return vector_value

    def visit_Vector(self, node):
        start_value = int(self.visit(node.start))
        end_value = int(self.visit(node.end))
        return [*range(start_value,end_value+1,1)]

    def visit_Var(self, node):
        var_name = node.value
        index = None

        ar = self.call_stack.peek()
        var_value = ar.get(var_name)

        if node.index != None:
            index = self.visit(node.index)
        
        if index != None:
            if isinstance(index, list):
                value = []
                negatives = []
                booleans = []
                for idx in index:
                    if idx < 0:
                        negatives.append(idx)
                    elif isinstance(idx, bool):
                        booleans.append(idx)
                    else:
                        value.append(var_value[self.convert_to_index(idx)])
                
                if len(negatives) != 0:
                    value = var_value
                    for negative in negatives:
                        value.pop(self.convert_to_index(negative))

                if len(booleans) != 0:
                    value = []
                    for idx, boolean in enumerate(booleans):
                        if boolean:
                            value.append(var_value[idx])
                    
                return value

            return var_value[self.convert_to_index(index)]

        return var_value

    def visit_NoOp(self, node):
        pass

    def visit_ProcedureDecl(self, node):
        pass

    def visit_ProcedureCall(self, node):
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        ar = ActivationRecord(
            type=ARType.FUNCTION,
            nesting_level=proc_symbol.scope_level + 1,
        )

        formal_params = proc_symbol.formal_params
        actual_params = node.actual_params

        for param_symbol, argument_node in zip(formal_params, actual_params):
            ar[param_symbol.name] = self.visit(argument_node)

        self.call_stack.push(ar)

        self.log(f'ENTER: PROCEDURE {proc_name}')
        self.log(str(self.call_stack))

        # evaluate procedure body
        self.visit(proc_symbol.block_ast)

        self.log(f'LEAVE: PROCEDURE {proc_name}')
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_IfStatement(self, node):
        for bool_node,compound_node in zip(node.bool_nodes, node.compound_nodes):
            bool_result = self.visit(bool_node)
            if (bool_result != None and bool_result == True):
                self.visit(compound_node)
                return
    
    def visit_ForLoop(self, node):
        self.can_break_or_continue = True
        vector = self.visit(node.vector_node)
        self.visit(node.var_node)
        var_name = node.var_node.value
        ar = self.call_stack.peek()

        for value in vector:
            ar[var_name] = value
            self.visit(node.compound_statement)
            if self.is_break:
                self.is_break = False
                break

        # Remove the var so we can keep limited scope
        ar.remove(var_name)
        self.can_break_or_continue = False

    def visit_WhileLoop(self, node):
        self.can_break_or_continue = True
        bool_result = self.visit(node.expr_node)
        while bool_result:
            self.visit(node.compound_statement)
            if self.is_break:
                self.is_break = False
                break
            bool_result = self.visit(node.expr_node)
        self.can_break_or_continue = False

    def find_primetype(self, init_type, new_type):
        if init_type is not str and isinstance(new_type, str):
            return str
        elif init_type not in (str, float) and isinstance(new_type, float):
            return float
        elif init_type not in (str, float, int, bool) and isinstance(new_type, bool):
            return bool
        return init_type

    def convert_to_index(self, index):
        if index == 0 or type(index) not in (bool, int, float):
            return None
        
        # Subtract 1 from the index because python uses zero based indexing whereas R is 1 based indexing
        return int(abs(index))-1

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)