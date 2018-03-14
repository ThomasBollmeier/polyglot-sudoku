const sudoku = require('./sudoku')
const io = require('./io')

const problem = io.readSudoku(process.argv[2])

io.printSudoku(problem)
console.log()

const solution = sudoku.solve(problem)
if (solution) {
    io.printSudoku(solution)
} else {
    console.log("No solution")
}
