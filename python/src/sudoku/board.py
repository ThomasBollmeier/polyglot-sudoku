class Board(object):
    
    def __init__(self, layout, parent=None):
        self._layout = layout
        self._parent = parent
        self._cells = {}
        if not self._parent:
            size = self._layout.get_size()
            values = list(range(1, size+1))
            for row in range(0, size):
                for col in range(0, size):
                    self._cells[(row, col)] = values[:]
                    
    def get_layout(self):
        return self._layout
                    
    def get_free_cell(self):
        min_pos = None
        min_vals = []
        cell_positions = []
        size = self._layout.get_size()
        for r in range(0, size):
            for c in range(0, size):
                cell_positions.append((r, c))
                
        for cell_pos in cell_positions:
            values = self.get_candidates(cell_pos)
            nvals = len(values)
            if nvals <= 1:
                continue
            if min_pos:
                if nvals < len(min_vals):
                    min_pos = cell_pos
                    min_vals = values
            else:
                min_pos = cell_pos
                min_vals = values
        return (min_pos, min_vals)
                
    def is_valid(self):
        return self._rows_valid() and self._columns_valid() and self._areas_valid()
    
    def _rows_valid(self):
        for r in range(0, self._layout.get_size()):
            if not self._group_valid(self._layout.get_row(r)):
                return False
        return True

    def _columns_valid(self):
        for c in range(0, self._layout.get_size()):
            if not self._group_valid(self._layout.get_column(c)):
                return False
        return True
    
    def _areas_valid(self):
        for x in range(0, self._layout.get_xmax_area()):
            for y in range(0, self._layout.get_ymax_area()):
                if not self._group_valid(self._layout.get_area(x, y)):
                    return False
        return True
    
    def _group_valid(self, cells):
        values = []
        for cell in cells:
            values += self.get_candidates(cell)
        distinct_vals = set(values)
        return len(distinct_vals) == self._layout.get_size()
                    
    def get_candidates(self, cell_pos):
        if cell_pos in self._cells:
            return self._cells[cell_pos][:]
        elif self._parent:
            return self._parent.get_candidates(cell_pos)
        else:
            row, col = cell_pos
            raise RuntimeError("No candidates at position ({}, {})".format(row, col))
        
    def set_value(self, cell_pos, value):
        
        todo = [(cell_pos, value)]
        board = self
        while todo:
            cell_pos, value = todo.pop()
            cells = {}
            cells[cell_pos] = [value]
            siblings = board._layout.get_siblings(cell_pos)
            for sibling in siblings:
                values = board.get_candidates(sibling)
                if value in values:
                    values.remove(value)
                    if len(values) != 1:
                        cells[sibling] = values
                    else: # single value => trigger removal 
                        todo.append((sibling, values[0]))
            board = Board(board._layout, board)
            board._cells = cells
        return board
    
if __name__ == "__main__":

    from layout import Layout    
    
    b = Board(Layout())
    b2 = b.set_value((0, 0), 1)
    b3 = b2.set_value((0, 1), 2)
    
    print(b3.is_valid())
    
    print(b.get_candidates((0, 0)))
    print(b.get_candidates((1, 0)))
    print(b.get_candidates((8, 8)))
    
    print(b2.get_candidates((0, 0)))
    print(b2.get_candidates((1, 0)))
    print(b2.get_candidates((8, 8)))
    
                
    