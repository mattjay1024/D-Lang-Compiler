import enum
from collections import OrderedDict

'''
Contains all classes related to the Symbol Table

Symbol table is able to store type, value, parameters, whether the symbol is a function or data, etc.
'''

# helper class for Enum
class AutoName(enum.Enum):
    def _generate_next_value_(name, start, count, last_values):
        return name.lower()


# SymbolTypes for table
class SymbolType(AutoName):
    DATA = enum.auto()
    FUNCTION = enum.auto()
    BUILTIN = enum.auto()
    UNKNOWN = enum.auto()

    @classmethod
    def _missing_(cls, value):
        return cls.UNKNOWN


# class for single entry in symbol table
class SymbolTableEntry:
    __slots__ = ('sym_type', 'val_type', 'value', 'signature', 'parameters') # can't remember why I did this but it seemed neat at the time
    
    def __init__(self, sym_type, val_type, signature, parameters):
        self.sym_type = sym_type
        self.val_type = val_type
        self.value = None
        self.parameters = parameters
        self.set_signature(signature)

    def add_sym_type(self, t):
        self.sym_type = t

    def add_val_type(self, val_type):
        self.val_type = val_type

    def add_value(self, value):
        self.value = value

    def add_parameters(self, params):
        self.parameters = params
    
    def set_signature(self, *args):
        self.signature = tuple([self.val_type]) + args

    def __repr__(self):
        if self.sym_type == SymbolType.FUNCTION:
            return '%s %s => %s' % (self.sym_type.name, repr(self.signature[1:]), self.val_type)

        return '%s %s' % (self.sym_type.name, self.val_type)


# wrapper class around symbol table dict
class SymbolTable:
    '''
        symbol stack:
            1) as the parser builds the tree, it will add symbols to the symbol stack when it finds them
            2) when a new scope is declared, a number of symbols equal to the # of symbols within the scope are popped off the symbol_stack
                - these are then added to the new scope as members
            3) any symbols remaining in the stack at the end of parsing are assumed to be global symbols

        scope stack:
            1) as the parser builds the tree, it counts the number of child scopes (block_count) in a given block
            2) when a new scope is declared, a number of scopes equal to the # of scopes within the scope's block are popped off the scope_stack
                - these are then added to the $scopes table in the new scope
            4) the new scope is put on the stack
            5) any scopes remaining in the stack at the end of parsing are assumed to be under the global scope

        in this way, symbols and scopes may be added to the their parent scope before the program knows anything about the parent scope (until new_scope() is called)

        in hindsight, I realize I could have just made an embedded parser action, but this works so whatever
    '''
    def __init__(self):
        self.table = dict()
        self._SCOPES_name = '$scopes'
        self.table['global'] = {self._SCOPES_name: {}}
        self.table['scope_stack'] = OrderedDict()
        self.table['symbol_stack'] = [] # trade the lookup time of a dict for the ability to have duplicate elements

    # insert a key into the scope stack, if it does not already exist
    def insert(self, key, sym_type=SymbolType.UNKNOWN, val_type='', signature=tuple(), parameters=[]):
        self.table['symbol_stack'].append((key, SymbolTableEntry(sym_type, val_type, signature, parameters)))
        return self.table['symbol_stack'][-1] # return the symbol for chaining. never used this so I could get rid of it I suppose

    # return the SymbolTableEntry associated with that key
    def lookup(self, key):
        if key not in self:
            raise KeyError('symbol %s does not exist' % key) # if this happens, the variable was used before declaration
        else:
            for sym in reversed(self.table['symbol_stack']): # will return the latest definition of the symbol
                if sym[0] == key:
                    return sym[1]
            return self.table['global'][key] # if it isn't in the symbol stack, it must be in global

    def remove(self, n):
        for i in range(n):
            self.table['symbol_stack'].pop()

    def check_redeclarations(self, keys):
        return tuple([self.table['symbol_stack'].count(k) > 1 for k in keys])
    
    # adds a new scope to the scope stack, adding as many symbols and scopes to the new one as necessary
    def new_scope(self, scope_name, sym_count, block_count):
        if scope_name == 'global': # don't allow for the global scope to be added
            raise ValueError('global is a reserved scope name')

        if scope_name in self.table:
            raise ValueError('%s is a known scope. This may be a redefine' % scope_name)

        new_scope = {self._SCOPES_name: {}}
        
        for i in range(sym_count): # pop sym_count symbols from the symbol_stack into the new scope
            symbol, entry = self.table['symbol_stack'].pop()

            if not symbol in new_scope:
                new_scope[symbol] = entry
            else:
                raise KeyError('Symbol redeclaration') # TODO: change to custom error

        for i in range(1, block_count + 1): # pop block_count scopes from the scope stack into the new scope
            name, scope = self.table['scope_stack'].popitem(True)
            new_name = name + '_%d' % i
            new_scope['$scopes'][new_name] = scope

        self.table['scope_stack'][scope_name] = new_scope

    # consumes all the scopes and symbols left, adding them to the global scope table
    def global_scope(self): # called at the end of parsing
        for name, scope in self.table['scope_stack'].items():
            self.table['global'][self._SCOPES_name][name] = scope
        self.table['scope_stack'].clear()

        for name, symbol in self.table['symbol_stack']:
            self.table['global'][name] = symbol
        self.table['symbol_stack'].clear()

        del self.table['scope_stack'] # these are no longer needed, so remove them
        del self.table['symbol_stack']

    def __contains__(self, item):
        return (any(sym[0] == item for sym in self.table['symbol_stack']) or item in self.table['global'])
