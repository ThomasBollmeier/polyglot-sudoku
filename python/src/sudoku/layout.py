class Layout(object):
    
    def __init__(self, nrows_area=3, ncols_area=3):
        self._nrows_area = nrows_area
        self._ncols_area = ncols_area
                    
    def get_size(self):
        return self._nrows_area * self._ncols_area
    
    def get_xmax_area(self):
        return self._nrows_area
    
    def get_ymax_area(self):
        return self._ncols_area
    
    def get_siblings(self, cell_pos):
        return set(
            self._get_row_siblings(cell_pos) +
            self._get_column_siblings(cell_pos) +
            self._get_area_siblings(cell_pos))
    
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
        
    def _get_row_siblings(self, cell_pos):
        row, col = cell_pos
        size = self.get_size()
        return [(row, c) for c in range(0, size) if c != col]
    
    def _get_column_siblings(self, cell_pos):
        row, col = cell_pos
        size = self.get_size()
        return [(r, col) for r in range(0, size) if r != row]
    
    def _get_area_siblings(self, cell_pos):
        siblings = []
        row, col = cell_pos
        r_min = (row // self._nrows_area) * self._nrows_area
        c_min = (col // self._ncols_area) * self._ncols_area
        for i in range(0, self._nrows_area):
            for j in range(0, self._ncols_area):
                sibling = (r_min+i, c_min+j)
                if sibling != cell_pos:
                    siblings.append(sibling)
        return siblings
    
if __name__ == "__main__":
    
    layout = Layout()
    
    print(len(layout.get_siblings((0, 0))))