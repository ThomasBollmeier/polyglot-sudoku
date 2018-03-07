import inout as io

board = io.load_board("expert_game")
io.print_board(board)

solved_board = board.solve()
if solved_board:
    print()
    io.print_board(solved_board)
else:
    print()
    print("No solution!")

            
        
    
    