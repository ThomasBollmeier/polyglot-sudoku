def solve(board):

    cell_pos, values = board.get_free_cell()
    if not cell_pos:
        return None
    
    todo = [(cell_pos, values, board)]
    
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


if __name__ == "__main__":
    
    #from board import Board
    #from layout import Layout
    import inout as io
    
    board = io.load_board("expert_game")
    #board = Board(Layout(3, 3))
    io.print_board(board)
    
    solved_board = solve(board)
    if solved_board:
        print()
        io.print_board(solved_board)
    else:
        print()
        print("No solution!")
    
    
            
        
    
    