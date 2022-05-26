# -------------------------------------------------------------------------
# dlang-parser.py: DLang Syntax Analyzer
# Run with source file (E:\\Libraries\\Documents\\test.txt)
# -------------------------------------------------------------------------
import sys
import dlang_indexer as indexer
import dlang_symbols as symbols
import dlang_lexer as lexer
import dlang_parser as parser

if __name__ == '__main__':
    import pprint

    printer = pprint.PrettyPrinter(compact=True)

    # Expects DLang source from file
    if len(sys.argv) == 2:
        with open(sys.argv[1]) as source:
            dlang_code = source.read()
            
            symbols = symbols.SymbolTable()
            indexer = indexer.SourceIndexer(dlang_code)
            
            lexer = lexer.DLangLexer(symbols, indexer)
            parser = parser.DLangParser(symbols, indexer)
            
            try:
                tokens = lexer.tokenize(dlang_code)
                parser.parse(tokens)
                print('\nSymbol Table')
                print('------------')
                printer.pprint(symbols.table)
            except EOFError: exit(1)
    else:
        print("[DLang]: Source file missing")
