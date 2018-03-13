const sudoku = require('./sudoku')
const fs = require('fs')

const readSudoku = (filePath) => {

    const content = fs.readFileSync(filePath, 'utf8')
    let lines = content.split(/\r?\n/)
    const firstLine = lines.shift()
    
    const geometry = firstLine.split(/\s+/)
        .map(s => parseInt(s, 10))
    const cells = lines
        .reduce((acc, line) => acc.concat(line), "")
        .split('')
        .map(s => parseInt(s, 10))

    return {cells, geometry}

}

const printSudoku = (problem) => {

    const {cells, geometry} = problem
    const size = sudoku.getSize(problem)
    
    let row = ""
    for (let i=0; i<size*size; i++) {
        if (i % size == 0 && row.length > 0) {
            console.log(row)
            row = ""
        }
        row += cells[i] > 0 ? cells[i] : "."
    }
    console.log(row)
}

module.exports = {
    readSudoku,
    printSudoku
}