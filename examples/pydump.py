import ast
import sys

print ast.dump(ast.parse(sys.stdin.read()))
