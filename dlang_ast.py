'''
Definitions for AST nodes

Each node records the lineno and index of its first terminal
Most nodes are just wrappers around a little bit of data, but some do more complex actions

StatementBlock refers to a block of statements (e.g., {int x; int y; return x + y;}
BlockStatement refers to any statement that has a block associated with it (e.g., if's, loops)
'''


class TerminalWrapper:
    def __init__(self, terminal, lineno, index):
        self.terminal = terminal
        self.lineno = lineno
        self.index = index
        
### AST Nodes ###

## Base Nodes ##

class Node:
    def __init__(self, lineno=0, index=0):
        self.lineno = lineno
        self.index = index

        
class Expression(Node): # every expression at least has a type. some may not have a value (like calls)
    value = None

    def __init__(self, lineno, index):
        self.type = None
        super().__init__(lineno, index)


class Statement(Node):
    pass


class Declaration(Node): # every declaration has an id and a type
    identifier = None

    def __init__(self, lineno, index):
        self.type = None
        super().__init__(lineno, index)


class StatementBlock(Node):
    statements = None
    declarations = None

    symbol_count = 0
    block_count = 0

    def __init__(self, declarations, statements, lineno, index):
        self.statements = statements
        self.declarations = declarations

        self.breaks = [] # if this continued to code generation, I figured this would be useful for loops
        self.returns = []

        self.symbol_count = len(declarations) # number of new symbols introduced

        for stmt in statements:
            if type(stmt) == BreakStatement: # add breaks and returns to appropriate lists
                self.breaks.append(stmt)
            elif type(stmt) == ReturnStatement:
                self.returns.append(stmt)
            elif issubclass(type(stmt), BlockStatement): # count number of sub-blocks
                self.block_count += 1

                # since a return breaks out of any scope, any returns in the sub-block are also returns of the enclosing block
                self.returns += stmt.block.returns

        super().__init__(lineno, index)

    def __repr__(self):
        return 'BLOCK (declarations: %d, statements: %d, breaks: %d, returns: %d)' % (len(self.declarations), len(self.statements), len(self.breaks), len(self.returns))


## Expression Derived Nodes ##
class Literal(Expression):
    def __repr__(self):
        return 'LITERAL (%s, %s)' % (self.type, str(self.value))


class Empty(Expression):
    def __init__(self):
        self.type = 'nothing'

    def __repr__(self):
        return 'Empty'


class VariableReference(Expression):
    name = None

    def __repr__(self):
        return 'VAR-REF %s' % self.name


class Operator(Expression):
    operation = None


class UnaryOperator(Operator):
    rhs = None

    def __repr__(self):
        return '(%s %s(%s))' % ('LOGICALEXPR' if self.type == 'bool' else 'MATHEXPR', self.operation, repr(self.rhs))


class BinaryOperator(UnaryOperator):
    lhs = None

    def __repr__(self):
        if self.type == 'bool':
            return '(LOGICALEXPR A %s B: A=%s B=%s)' % (self.operation, repr(self.lhs), repr(self.rhs))
        else:
            return '(MATHEXPR %s %s and %s)' % (self.operation, repr(self.lhs), repr(self.rhs))


## Statement Derived Nodes ##
class BreakStatement(Statement):
    def __repr__(self):
        return 'BREAK'


class BlockStatement(Statement):
    block = None


class CallStatement(Statement, Expression):
    identifier = None
    args = None

    def __repr__(self):
        return 'CALL %s, args: %s' % (self.identifier, str(self.args))


class ExpressionStatement(Statement):
    expression = None

  
# Block Statement Derived Nodes #
class IfStatement(BlockStatement):
    condition = None
    alternate = None

    def __repr__(self):
        return 'BRANCH condition=%s%s' % (repr(self.condition), ', alternative-present' if self.alternate else '')


class ElseStatement(BlockStatement):
    def __repr__(self):
        return 'ELSE'


class WhileStatement(IfStatement):
    def __repr__(self):
        return 'LOOP_WHILE condition=%s' % repr(self.condition)


class ForStatement(IfStatement):
    initial = None
    increment = None

    def __repr__(self):
        opt = ''.join(['(', ('initial=%s' % repr(self.initial)) if self.initial else '', ('increment=%s' % repr(self.increment)) if self.increment else '', ')'])
        return 'LOOP_FOR condition=%s%s' % (repr(self.condition), opt if (self.initial or self.increment) else '')


# Expression Statement Derived Nodes #
class AssignmentStatement(ExpressionStatement):
    target = None

    def __repr__(self):
        return 'ASSIGN %s => VAR-REF %s' % (repr(self.expression), self.target)


class ReturnStatement(ExpressionStatement):
    def __repr__(self):
        return 'RETURN %s' % (repr(self.expression) if self.expression.type != 'nothing' else '')


## Declaration Derived Nodes ##
class VariableDeclaration(Declaration):
    def __repr__(self):
        return 'VAR %s: %s' % (self.identifier, self.type)


class FunctionDeclaration(Declaration):
    parameters = None
    block = None

    def __repr__(self):
        return 'FUNC %s: %s => %s' % (self.identifier, str(self.parameters), self.type)
