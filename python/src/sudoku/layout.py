class Layout(object):
    
    def __init__(self, nrows_area=3, ncols_area=3):
        self._nrows_area = nrows_area
        self._ncols_area = ncols_area
                    
    def get_size(self):
        return self._nrows_area * self._ncols_area
    
    def get_all_cells(self):
        cells = []
        size = self.get_size()
        for row in range(0, size):
            for col in range(0, size):
                cells.append((row, col))
        return cells
    
    def get_all_groups(self):
        
        groups = []
        size = self.get_size()
        
        for row in range(0, size):
            groups.append(self.get_row(row))
        
        for col in range(0, size):
            groups.append(self.get_column(col))
        
        for x in range(0, self._nrows_area):
            for y in range(0, self._ncols_area):
                groups.append(self.get_area(x, y))
                
        return groups
    
    def get_siblings(self, cell_pos):
        
        row, col = cell_pos
        x = col // self._ncols_area
        y = row // self._nrows_area
        
        cells = self.get_row(row)
        cells += self.get_column(col)
        cells += self.get_area(x, y)
        
        ret = set(cells)
        ret.remove(cell_pos)
        
        return ret
    
    def get_row(self, row):
        return [(row, c) for c in range(0, self.get_size())]

    def get_column(self, col):
        return [(r, col) for r in range(0, self.get_size())]
    
    def get_area(self, x, y):
        cells = []
        r_min = y * self._nrows_area
        c_min = x * self._ncols_area
        for i in range(0, self._nrows_area):
            for j in range(0, self._ncols_area):
                cell_pos = (r_min+i, c_min+j)
                cells.append(cell_pos)
        return cells
