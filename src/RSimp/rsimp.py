

import argparse
import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))
from src.RSimp.errors import LexerError, ParserError, SemanticError
from src.RSimp.lexer import Lexer
from src.RSimp.parser import Parser
from src.RSimp.semantic_analyzer import SemanticAnalyzer
from src.RSimp.interpreter import Interpreter
from src.RSimp import cmd_line_globals

class RSimp():
    def run(self, file, scope, stack):
        cmd_line_globals._SHOULD_LOG_SCOPE, cmd_line_globals._SHOULD_LOG_STACK = scope, stack

        text = open(file, 'r').read()

        lexer = Lexer(text)

        try:
            parser = Parser(lexer)
            tree = parser.parse()
        except (LexerError, ParserError) as e:
            print(e.message)
            sys.exit(1)
        
        semantic_analyzer = SemanticAnalyzer()
        try:
            semantic_analyzer.visit(tree)
        except SemanticError as e:
            print(e.message)
            sys.exit(1)
        
        interpreter = Interpreter(tree)
        interpreter.interpret()

    def run_with_cmd_args(self):
        parser = argparse.ArgumentParser(
            description='SPI - Simple Pascal Interpreter'
        )
        parser.add_argument('inputfile', help='Pascal source file')
        parser.add_argument(
            '--scope',
            help='Print scope information',
            action='store_true',
        )
        parser.add_argument(
            '--stack',
            help='Print call stack',
            action='store_true',
        )
        args = parser.parse_args()
        self.run(args.inputfile, args.scope, args.stack)
