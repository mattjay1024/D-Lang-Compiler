from sly import Parser
import dlang_lexer as lexer
from dlang_symbols import SymbolType
from dlang_ast import *

'''
Parser produces a list of declarations that compose the program
Each declaration is an AST of program constructs
Also constructs the symbol table and handles some semantic analysis
'''

class DLangParser(Parser):
    #debugfile = 'parser.out'
    ops = { # convert literals into a name
        '+': 'ADD',
        '-': 'SUBTRACT',
        '*': 'MULTIPLY',
        '/': 'DIVIDE',
        '%': 'MODULO',
        '==': 'COMPARE',
        '!=': 'CMP_NOT_EQ',
        '<': 'CMP_LESS',
        '<=': 'CMP_LESS_EQ',
        '>': 'CMP_GREATER',
        '>=': 'CMP_GREATER_EQ',
        '&&': 'LOGICAL_AND',
        '||': 'LOGICAL_OR'
    }
    unary_ops = {
        '!': 'NOT',
        '-': 'UNARY_MINUS'
    }
    tokens = lexer.DLangLexer.tokens
    precedence = (# this is pretty much c's precedence. figured it out after getting half the shift/reduce conflicts to go away, and just borrowed from there. Threw me for a loop that it's up (low prec) -> down (high prec)
        ('right', ASSIGN),
        ('left', OR),
        ('left', AND),
        ('nonassoc', EQ, NE),
        ('nonassoc', '<', LE, '>', GE),
        ('left', '+', '-'),
        ('left', '*', '/', '%'),
        ('right', UNARY)
    )

    numeric_types = {'bool': 0, 'int': 1, 'double': 2}

    semantic_errors = []
    
    def __init__(self, symtable, source_indexer):
        self.symtable = symtable
        self.error_count = 0
        self.indexer = source_indexer

    # make a block, single statement if necessary
    def make_block(self, block):
        if type(block) != StatementBlock:
            return self.make_single_stmt_block(block)
        return block
    
    # turn a single statement into a block
    def make_single_stmt_block(self, stmt):
        return StatementBlock([], [stmt], stmt.lineno, stmt.index)
    
    # turns a ParamList into a signature tuple
    def params_to_typelist(self, params):
        return tuple([param.type for param in params])

    # turns a ParamList into a list of names
    def params_to_namelist(self, params):
        return [param.identifier for param in params]

    # turns an ArgList into a signature tuple
    def args_to_typelist(self, args):
        return tuple([arg.type for arg in args])

    # checks if two types are compatible and returns the wider type if they are
    def check_type_compat(self, a, b, strict=False):
        if a == b or (a == None or b == None):
            return (True, a if a is not None else (b if b is not None else None)) # return whatever type exists, otherwise give up and return None
        elif a in self.numeric_types and b in self.numeric_types:
            t = None
            if self.numeric_types[a] > self.numeric_types[b]:
                t = a
            elif not strict: # if strict is true, then only return True and a type when a.type > b.type, not vice versa. For assignments
                t = b
            else:
                return (False, t)
            return (True, t)
        else:
            return (False, None)

    # compares two equal size typelists
    # assume any None values *would* have been the correct type, since we want to know if there are any other semantic errors here
    def compare_typelist(self, a, b):
        return False if any([((t1 != t2) and (t1 != None and t2 != None)) for t1, t2 in zip(a, b)]) else True

    def add_semantic_error(self, line, index, what):
        self.semantic_errors.append((line, index, what))

    def print_semantic_errors(self):
        for lineno, index, what in self.semantic_errors:
            err_line, mark = self.indexer.get_line(lineno, index)
            print('At line %d, col %d: %s' % (lineno, self.indexer.get_col(lineno, index) + 1, what))
            print('| %s\n| %s' % (err_line, mark))


    # Productions #
    # Program -> Decl+ # anywhere + appears in a rule, a list construct is used (i.e., Decls -> Decl Decls | Decl, where the latter rule creates a list and the former appends to the created list) 
    @_('Decls')
    def Program(self, p):
        p.Decls.reverse() # a list created like this must be reversed since it's constructed in reverse
        self.symtable.global_scope() # finish symtable creation by putting all remaining scopes and symbols into global

        if len(self.semantic_errors) > 0:
            print('There were %d errors encountered while parsing.' % len(self.semantic_errors))
            self.print_semantic_errors()
        else:
            print('Parsing completed successfully! No semantic errors') # If we get here with no issues, bottom-up parsing is successful!
        return p.Decls

    @_('Decl Decls')
    def Decls(self, p): 
        p.Decls.append(p.Decl) # productions within the list
        return p.Decls

    @_('Decl')
    def Decls(self, p): # list base level
        return [p.Decl]

    # Decl -> VariableDecl | FunctionDecl
    @_('VariableDecl', 'FunctionDecl')
    def Decl(self, p):
        return p[0]

    # VariableDecl -> Variable;
    @_('Variable ";"')
    def VariableDecl(self, p):
        return p.Variable

    # Variable -> Type ident
    @_('Type Identifier')
    def Variable(self, p):
        var = VariableDeclaration(p.Type.lineno, p.Type.index)
        var.type = p.Type.terminal
        var.identifier = p.Identifier.terminal
        self.symtable.insert(var.identifier, SymbolType.DATA, var.type)

        print('Found', var)
        
        return var

    # Type -> int | double | bool | string    
    @_('INTK', 'DOUBLEK', 'BOOLK', 'STRINGK')
    def Type(self, p):
        return TerminalWrapper(p[0], p.lineno, p.index)
    
    # FunctionDecl -> Type ident ( Formals ) StmtBlock | nothing ident ( Formals ) StmtBlock
    @_('Type Identifier "(" Formals ")" StmtBlock', 'Nothing Identifier "(" Formals ")" StmtBlock')
    def FunctionDecl(self, p):
        f = FunctionDeclaration(p[0].lineno, p[0].index)
        f.type = p[0].terminal
        f.identifier = p.Identifier.terminal
        f.parameters = p.Formals
        f.block = p.StmtBlock

        # check for redeclaration of parameters #
        signature = self.params_to_typelist(f.parameters)
        namelist = self.params_to_namelist(f.parameters)

        redeclarations = self.symtable.check_redeclarations(namelist)
        if len(redeclarations) > 0:
            error_vars = [name for name, is_redeclared in zip(namelist, redeclarations) if is_redeclared]
            for var in error_vars:
                for decl in f.block.declarations:
                    if var == decl.identifier:
                        self.add_semantic_error(decl.lineno, decl.index, 'variable redeclaration')
                
        # check for return type #
        if len(f.block.returns) == 0: # if a function block has no return, assume implicit 'return nothing;' or 'return;'
            if f.type != 'nothing':
                self.add_semantic_error(f.lineno, f.index, 'function %s has type %s but returns nothing' % (f.identifier, f.type))
                
        for ret in f.block.returns:
            if ret.expression.type != f.type:
                self.add_semantic_error(ret.lineno, ret.index, 'function %s has type %s but returns %s' % (f.identifier, f.type, ret.expression.type))
                
        f.block.symbol_count += len(f.parameters) # since the parameters are in the scope of the function, increase the sym_count of the func block by # of parameters

        self.symtable.new_scope('%s_func_%s' % (f.type, f.identifier), f.block.symbol_count, f.block.block_count) # new scope, should be added to global table

        try:
            self.symtable.insert(f.identifier, SymbolType.FUNCTION, f.type, signature, namelist) # add function to symbol stack AFTER new scope creation, so it doesn't get added to its own scope
        except KeyError as err:
            print('WARNING:', err)

        print('Found', f)

        return f

    @_('NOTHING')
    def Nothing(self, p):
        return TerminalWrapper(p.NOTHING, p.lineno, p.index)

    # Formals -> Variable+ , | epsilon
    @_('ParamList')
    def Formals(self, p):
        p.ParamList.reverse()
        return p.ParamList

    @_('epsilon')
    def Formals(self, p):
        return []

    @_('Variable "," ParamList')
    def ParamList(self, p):
        p.ParamList.append(p.Variable)
        return p.ParamList
    
    @_('Variable')
    def ParamList(self, p):
        return [p.Variable]

    # StmtBlock -> { VariableDecl* Stmt* }
    @_('"{" OptVariableDecls OptStmts "}"')
    def StmtBlock(self, p):
        return StatementBlock(p.OptVariableDecls, p.OptStmts, p.lineno, p.index)

    @_('VariableDecl VariableDecls')
    def VariableDecls(self, p):
        p.VariableDecls.append(p.VariableDecl)
        return p.VariableDecls

    @_('VariableDecl')
    def VariableDecls(self, p):
        return [p.VariableDecl]

    @_('VariableDecls')
    def OptVariableDecls(self, p):
        p.VariableDecls.reverse()
        return p.VariableDecls

    @_('epsilon')
    def OptVariableDecls(self, p): # to accomplish * pattern, an empty production is used that returns an empty list
        return []

    @_('Stmt Stmts')
    def Stmts(self, p):
        p.Stmts.append(p.Stmt)
        return p.Stmts

    @_('Stmt')
    def Stmts(self, p):
        return [p.Stmt]

    @_('Stmts')
    def OptStmts(self, p):
        p.Stmts.reverse()
        return p.Stmts

    @_('epsilon')
    def OptStmts(self, p):
        return []

    # Stmt -> <Expr> ; | IfStmt | WhileStmt | ForStmt | BreakStmt | ReturnStmt | OutputStmt | StmtBlock
    @_('OptExpr ";"', 'IfStmt', 'WhileStmt', 'ForStmt', 'BreakStmt', 'ReturnStmt', 'OutputStmt', 'StmtBlock')
    def Stmt(self, p):
        print('Found', p[0])
        return p[0]

    # IfStmt -> if ( Expr ) Stmt <else Stmt>
    @_('IF "(" Expr ")" Stmt OptElse')
    def IfStmt(self, p):
        stmt = IfStatement(p.lineno, p.index)
        stmt.condition = p.Expr
        stmt.block = self.make_block(p.Stmt)

        if type(p.OptElse) != Empty:
            stmt.alternate = p.OptElse

        self.symtable.new_scope('if', stmt.block.symbol_count, stmt.block.block_count) # create new scope for the if statement
        return stmt

    @_('ELSE Stmt')
    def OptElse(self, p):
        stmt = ElseStatement(p.lineno, p.index)
        stmt.block = self.make_block(p.Stmt)
        self.symtable.new_scope('else', stmt.block.symbol_count, stmt.block.block_count) # new scope for else
        return stmt
    
    @_('epsilon')
    def OptElse(self, p):
        return p.epsilon

    # WhileStmt -> while ( Expr ) Stmt
    @_('WHILE "(" Expr ")" Stmt')
    def WhileStmt(self, p):
        stmt = WhileStatment(p.lineno, p.index)
        stmt.condition = p.Expr
        stmt.block = self.make_block(p.Stmt)
        self.symtable.new_scope('while', stmt.block.symbol_count, stmt.block.block_count) # new scope for while
        return stmt

    # ForStmt -> for ( <Expr> ; Expr ; <Expr> ) Stmt
    @_('FOR "(" OptExpr ";" Expr ";" OptExpr ")" Stmt')
    def ForStmt(self, p):
        stmt = ForStatement(p.lineno, p.index)
        stmt.condition = p.Expr
        stmt.block = self.make_block(p.Stmt)
        
        if type(p.OptExpr0) != Empty:
            stmt.initial = p.OptExpr0

        if type(p.OptExpr1) != Empty:
            stmt.increment = p.OptExpr1
            
        self.symtable.new_scope('for', stmt.block.symbol_count, stmt.block.block_count) # and new scope for the for statement
        return stmt

    # BreakStmt -> break ;
    @_('BREAK ";"')
    def BreakStmt(self, p):
        return BreakStatement(p.lineno, p.index)

    # ReturnStmt -> return <Expr> ;
    @_('RETURN OptExpr ";"')
    def ReturnStmt(self, p):
        ret = ReturnStatement(p.lineno, p.index)
        ret.expression = p.OptExpr
        return ret

    # OutputStmt -> Output ( Expr+ , ) ;
    @_('OUTPUT "(" ArgList ")" ";"')
    def OutputStmt(self, p):
        f = CallStatement(p.lineno, p.index)
        f.identifier = p.OUTPUT
        f.args = p.ArgList
        f.type = 'nothing'

        print('Found', f)
        
        return f

    # Expr -> ident = Expr | ident | Constant | Call | ( Expr ) | Expr + Expr | Expr - Expr | Expr * Expr | Expr / Expr | Expr % Expr | - Expr | Expr < Expr | Expr <= Expr
    # | Expr > Expr | Expr >= Expr | Expr == Expr | Expr != Expr | Expr && Expr | Expr || Expr | ! Expr | InputInt() | InputLine()
    @_('Identifier "=" Expr %prec ASSIGN')
    def Expr(self, p):
        assign = AssignmentStatement(p.Identifier.lineno, p.Identifier.index)
        assign.expression = p.Expr
        assign.target = p.Identifier.terminal

        try:
            entry = self.symtable.lookup(assign.target)

            if entry.sym_type == SymbolType.DATA:
                type_check, t = self.check_type_compat(entry.val_type, assign.expression.type, True)
                if not type_check:
                    self.add_semantic_error(assign.expression.lineno, assign.expression.index, 'expression type (%s) does not match variable type (%s)' % (assign.expression.type, entry.val_type))
                entry.add_value(t)
            else:
                self.add_semantic_error(assign.lineno, assign.index, 'invalid assignment of expression to function')
        except KeyError:
            self.add_semantic_error(assign.lineno, assign.index, 'undeclared variable %s' % assign.target)
        
        return assign
    
    @_('Identifier')
    def Expr(self, p):
        reference = VariableReference(p.Identifier.lineno, p.Identifier.index)
        reference.name = p.Identifier.terminal

        try:
            symbol = self.symtable.lookup(reference.name)
            reference.type = symbol.val_type
            reference.value = symbol.value
        except KeyError:
            self.add_semantic_error(reference.lineno, reference.index, 'undeclared variable %s' % reference.identifier)
        
        return reference
    
    @_('Constant')
    def Expr(self, p):
        return p.Constant
    
    @_('Call')
    def Expr(self, p):
        return p.Call
    
    @_('"(" Expr ")"')
    def Expr(self, p):
        return p.Expr
    
    @_('Expr "+" Expr',
        'Expr "-" Expr',
        'Expr "*" Expr',
        'Expr "/" Expr',
        'Expr "%" Expr')
    def Expr(self, p):
        op = BinaryOperator(p.Expr0.lineno, p.Expr0.index)
        op.operation = self.ops[p[1]]
        op.rhs = p.Expr0
        op.lhs = p.Expr1

        type_check, t = self.check_type_compat(op.lhs.type, op.rhs.type)
        if not type_check:
            self.add_semantic_error(op.lineno, op.index, 'incompatible operand types: %s, %s' % (op.lhs.type, op.rhs.type))
        op.type = t
        
        return op

    @_('Expr "<" Expr',
        'Expr LE Expr',
        'Expr ">" Expr',
        'Expr GE Expr',
        'Expr EQ Expr',
        'Expr NE Expr',
        'Expr AND Expr',
        'Expr OR Expr')
    def Expr(self, p):
        op = BinaryOperator(p.Expr0.lineno, p.Expr0.index)
        op.operation = self.ops[p[1]]
        op.rhs = p.Expr0
        op.lhs = p.Expr1

        type_check, t = self.check_type_compat(op.lhs.type, op.rhs.type)
        if not type_check:
            self.add_semantic_error(op.lineno, op.index, 'incompatible operand types: %s, %s' % (op.lhs.type, op.rhs.type))
            op.type = t
        else:
            op.type = 'bool'        

        return op
    
    @_('"-" Expr %prec UNARY', '"!" Expr %prec UNARY')
    def Expr(self, p):
        op = UnaryOperator(p.lineno, p.index)
        op.operation = self.unary_ops[p[0]]
        op.rhs = p.Expr
        op.type = op.rhs.type

        if not op.type in self.numeric_types:
            self.add_semantic_error(op.lineno, op.index, 'incompatible operand type: %s' % op.type)

        return op
    
    @_('INPUTINT "(" ")"')
    def Expr(self, p):
        f = CallStatement(p.lineno, p.index)
        f.identifier = p.INPUTINT
        f.args = []
        f.type = 'int'
        
        return f

    @_('INPUTINT "(" error ")"')
    def Expr(self, p):
        self.add_semantic_error(p.lineno, p.index, 'InputInt takes no arguments, but was given some')
    
    @_('INPUTLINE "(" ")"')
    def Expr(self, p):
        f = CallStatement(p.lineno, p.index)
        f.identifier = p.INPUTLINE
        f.args = []
        f.type = 'string'

        return f

    @_('INPUTLINE "(" error ")"')
    def Expr(self, p):
        self.add_semantic_error(p.lineno, p.index, 'InputLine takes no arguments, but was given some')

    @_('Expr')
    def OptExpr(self, p):
        return p.Expr

    @_('epsilon')
    def OptExpr(self, p):
        return p.epsilon
        
    # Call -> ident ( Actuals )
    @_('Identifier "(" Actuals ")"')
    def Call(self, p):
        ## TODO: verify function has been declared before it was used ##
        f = CallStatement(p.Identifier.lineno, p.Identifier.index)
        f.identifier = p.Identifier.terminal

        try:
            entry = self.symtable.lookup(f.identifier)
            
        
            f.args = p.Actuals
            f.type = entry.val_type

            f_args = self.args_to_typelist(f.args)
            entry_args = entry.signature[1]

            if len(f_args) != len(entry_args):
                self.add_semantic_error(f.lineno, f.index, 'function %s expects %d arguments, got %d instead' % (f.identifier, len(entry_args), len(f_args)))
            elif not self.compare_typelist(f_args, entry_args):
                self.add_semantic_error(f.lineno, f.index, 'function %s expects arguments of type (%s), got (%s) instead' % (f.identifier, ', '.join(entry_args), ', '.join(f_args)))
        except KeyError:
            self.add_semantic_error(f.lineno, f.index, 'undeclared function %s' % f.identifier)
            
        return f

    # Actuals -> Expr+ , | epsilon
    @_('ArgList')
    def Actuals(self, p):
        p.ArgList.reverse()
        return p.ArgList

    @_('epsilon')
    def Actuals(self, p):
        return []

    @_('Expr "," ArgList')
    def ArgList(self, p):
        p.ArgList.append(p.Expr)
        return p.ArgList

    @_('Expr')
    def ArgList(self, p):
        return [p.Expr]

    # Contant -> intConstant | doubleConstant | boolConstant | stringConsant | null
    @_('IntConstant', 'DoubleConstant', 'BoolConstant', 'StringConstant', 'NullConstant')
    def Constant(self, p):
        constant = Literal(p[0].lineno, p[0].index)
        constant.type = p[0].terminal[0]
        constant.value = p[0].terminal[1]
        return constant

    @_('INT')
    def IntConstant(self, p):
        return TerminalWrapper(('int', int(p.INT)), p.lineno, p.index)

    @_('DOUBLE')
    def DoubleConstant(self, p):
        return TerminalWrapper(('double', float(p.DOUBLE)), p.lineno, p.index)

    @_('BOOL')
    def BoolConstant(self, p):
        return TerminalWrapper(('bool', bool(p.BOOL)), p.lineno, p.index)

    @_('STRING')
    def StringConstant(self, p):
        return TerminalWrapper(('string', str(p.STRING)), p.lineno, p.index)

    @_('NULL')
    def NullConstant(self, p):
        return TerminalWrapper(('null', p.NULL), p.lineno, p.index)

    @_('IDENTIFIER')
    def Identifier(self, p):
        return TerminalWrapper(p.IDENTIFIER, p.lineno, p.index)
    
    # epsilon rule
    @_('')
    def epsilon(self, p):
        return Empty()

    def error(self, p):
        self.error_count += 1
        if not p:
            print('ERROR: Syntax error at EOF')
            return

        err_line, mark = self.indexer.get_line(p.lineno, p.index)
        print('At line %d, col %d: syntax error on %s' % (p.lineno, self.indexer.get_col(p.lineno, p.index), p.type))
        print('| %s\n| %s' % (err_line, mark))
