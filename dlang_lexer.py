from sly import Lexer
from dlang_symbols import SymbolType

'''
Lexer produces tokens for the parser
'''

class DLangLexer(Lexer):
    # Define names of tokens
    tokens = {LE, GE, EQ, NE, AND, OR, INT, DOUBLE, STRING, IDENTIFIER, NOTHING, INTK, DOUBLEK, BOOL, BOOLK, STRINGK, NULL, FOR, WHILE, IF, ELSE, RETURN, BREAK, OUTPUT, INPUTINT, INPUTLINE}
    
    # Single-character literals can be recognized without token names
    # If you use separate tokens for each literal, that is fine too
    literals = {'+', '-', '*', '/', '%', '<', '>', '=','!', ';', ',', '.', '[', ']','(',')','{','}'}
    
    # Specify things to ignore
    ignore = ' \t\r' # space, tab, and carriage return
    #ignore_empty_lines = r'(?<=\n)[ \t\r]+(?=\n)' # ignore empty, nonsense lines
    ignore_comment1= r'\/\*[^"]*\*\/' # c-style multi-line comment (note: test with input from file)
    ignore_comment = r'\/\/.*' # single line comment

    # Specify REs for each token
    STRING = r'"(.*?)"'
    DOUBLE = r'[0-9]+\.[0-9]*([E][+-]?\d+)?'
    INT = r'[0-9]+'
    EQ = r'=='
    NE = r'!='
    LE = r'<='
    GE = r'>='
    AND = r'&&' 
    OR =  r'\|\|'
    IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]{0,39}'

    # IDENTIFIER lexemes overlap with keywords.
    # To avoid confusion, we do token remaping.
    # Alternatively, you can specify each keywork before IDENTIFIER
    IDENTIFIER['nothing'] = NOTHING
    IDENTIFIER['int'] = INTK
    IDENTIFIER['double'] = DOUBLEK
    IDENTIFIER['string'] = STRINGK
    #IDENTIFIER['interface'] = INTERFACE # missing from original code
    IDENTIFIER['bool'] = BOOLK
    IDENTIFIER['True'] = BOOL
    IDENTIFIER['False'] = BOOL
    IDENTIFIER['null'] = NULL
    IDENTIFIER['for'] = FOR
    IDENTIFIER['while'] = WHILE
    IDENTIFIER['if'] = IF
    IDENTIFIER['else'] = ELSE
    IDENTIFIER['return'] = RETURN
    IDENTIFIER['break'] = BREAK # also missing from original code
    # IDENTIFIER['ArrayInstance'] = ARRAYINSTANCE
    IDENTIFIER['Output'] = OUTPUT
    IDENTIFIER['InputInt'] = INPUTINT
    IDENTIFIER['InputLine'] = INPUTLINE

    def __init__(self, sym_table, source_indexer):
        super().__init__()
        self.indexer = source_indexer
        self.symbols = sym_table
        
        self.symbols.insert('InputInt', sym_type=SymbolType.BUILTIN, val_type='int')
        self.symbols.insert('InputLine', sym_type=SymbolType.BUILTIN, val_type='string')
        self.symbols.insert('Output', sym_type=SymbolType.BUILTIN, val_type='nothing')

    def IDENTIFIER(self, t):
        #self.symtable.insert(t.value)
        return t
    
    @_(r'\n+')
    def ignore_newline(self, t): # count lineno and columns
        #print('self.lineno %d, indexer.lineno %d' % (self.lineno, self.indexer.lineno))
        #print('t.value.count(\'\\n\') = %d vs. len(t.value) = %d' % (t.value.count('\n'), len(t.value)))
        #print('\tadvance %d lines' % len(t.value))
        self.indexer.advance_blank_lines(len(t.value))
        self.indexer.mark_line_start(t.index + len(t.value))

        self.lineno += t.value.count('\n')

        #print('line %d (%d) starts at %d' % (self.lineno, self.indexer.lineno, t.index + len(t.value)))

    def error(self, t):
        err_line, mark = self.indexer.get_line(self.lineno, t.index)
        print('At line %d, col %d: unknown symbol' % (self.lineno, self.indexer.get_col(self.lineno, t.index)))
        print('| %s\n| %s' % (err_line, mark))
        self.index+=1
