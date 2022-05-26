'''
SourceIndexer maintains a record of all the line starts
Can return a line of source code given the line number, and can optionally mark the line with a ^
'''

class SourceIndexer:
    def __init__(self, source):
        self.source = source
        self.line_start_indices = {}

        self.lineno = 1

    def mark_line_start(self, index):
        text_start = index
        for c in self.source[index:]:
            if c == ' ' or c == '\t':
                text_start += 1
            else:
                break
        self.line_start_indices[self.lineno] = (index, text_start)

    def advance_blank_chars(self, n):
        self.line_start_indices[self.lineno] += n

    def advance_blank_lines(self, n):
        self.lineno += n

    def get_line(self, lineno, mark_index=None):
        line = self._get_line_at_index(self.line_start_indices[lineno][0])
        if mark_index is not None:
            col = self.get_text_col(lineno, mark_index)
            return (line, self._mark_line(line, col))
        return line

    def get_col(self, lineno, index):
        return (index - self.line_start_indices[lineno][0])

    def get_text_col(self, lineno, index):
        return (index - self.line_start_indices[lineno][1])

    def _mark_line(self, line, col):        
        pre = (' ' * col)
        post = (' ' * (len(line) - col))
        return (pre + '^' + post)

    def _get_line_at_index(self, index):
        line = ''
        for c in self.source[index:]:
            if c == '\n':
                break
            line += c
        return line.lstrip(' \t')
