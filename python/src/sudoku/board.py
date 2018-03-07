class Board(object):
    
    def __init__(self, layout, parent=None):
        
        self._layout = layout
        self._parent = parent
        self._cells = {}
        if not self._parent:
            size = self._layout.get_size()
            values = list(range(1, size+1))
            for cell in self._layout.get_all_cells():
                self._cells[cell] = values[:]
                    
    def solve(self):
    
        cell_pos, values = self.get_free_cell()
        if not cell_pos:
            return None
        
        todo = [(cell_pos, values, self)]
        
        while todo:
            cell_pos, values, board = todo.pop()
            value = values[0]
            values = values[1:]
            if values:
                todo.append((cell_pos, values, board))
            next_board = board.set_value(cell_pos, value)
            if next_board.is_valid():
                cell_pos, values = next_board.get_free_cell()
                if cell_pos is None:
                    # Solution found!
                    return next_board
                else:
                    todo.append((cell_pos, values, next_board))
                    
        return None
                    
    def get_layout(self):
        return self._layout
                    
    def get_free_cell(self):
        
        min_pos = None
        min_vals = []
                
        for cell_pos in self._layout.get_all_cells():
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
        
        for grp in self._layout.get_all_groups():
            if not self._group_valid(grp):
                return False
        return True
    
    def _group_valid(self, cells):
        values = []
        for cell in cells:
            values += self.get_candidates(cell)
        distinct_vals = set(values)
        return len(distinct_vals) == self._layout.get_size()
                    
    def get_candidates(self, cell_pos):
        
        board = self
        
        while board:
            if cell_pos in board._cells:
                return board._cells[cell_pos][:]
            board = board._parent
            
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
