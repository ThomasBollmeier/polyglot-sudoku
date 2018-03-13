const sudoku = require('./sudoku')
const io = require('./io')

const problem = io.readSudoku("schwer.txt")

io.printSudoku(problem)
console.log()

const solution = sudoku.solve(problem)
if (solution) {
    io.printSudoku(solution)
} else {
    console.log("No solution")
}
