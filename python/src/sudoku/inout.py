from layout import Layout
from board import Board

def load_board(filepath):
    
    f = open(filepath)
    lines = f.readlines()
    f.close()
    
    head = lines[0]
    lines = lines[1:]
    
    nrows_area, ncols_area = [int(s.strip()) for s in head.split()]
    board = Board(Layout(nrows_area, ncols_area))
    
    row = 0
    for line in lines:
        line = line.strip()
        col = 0
        for ch in line:
            try:
                value = int(ch)
                if value:
                    board = board.set_value((row, col), value)
                col += 1
            except ValueError:
                pass
        row += 1
        if row == board.get_layout().get_size():
            break
    return board
                
def print_board(board):
    
    layout = board.get_layout()
    size = layout.get_size()
    for r in range(0, size):
        row = ""
        for c in range(0, size):
            values = board.get_candidates((r, c))
            if len(values) == 1:
                row += "{}".format(values[0])
            else:
                row += "."
        print(row)

