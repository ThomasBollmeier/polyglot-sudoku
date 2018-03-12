
const getSize = (geometry) => {
    const [nRowsArea, nColsArea] = geometry;
    return nRowsArea * nRowsArea;
}

const getRowIndices = (geometry, row) => {
    
    const size = getSize(geometry)
    
    let ret = []
    for (let col=0; col<size; col++) {
        ret.push(row*size + col)
    }

    return ret
}

const getColumnIndices = (geometry, col) => {
    
    const size = getSize(geometry)
    
    let ret = []
    for (let row=0; row<size; row++) {
        ret.push(row*size + col)
    }

    return ret
}

const getAreaIndices = (geometry, x, y) => {
    
    const [nRowsArea, nColsArea] = geometry
    const size = getSize(geometry)
    
    let ret = []

    for (let r=0; r<nRowsArea; r++) {
        let row = y*nRowsArea + r
        for (let c=0; c<nColsArea; c++) {
            let col = x*nColsArea + c
            ret.push(row*size + col)
        }
    }

    return ret
}

const getSiblings = (geometry, idx) => {

    const size = getSize(geometry)
    const [nRowsArea, nColsArea] = geometry
    const row = Math.floor(idx/size)
    const col = idx % size
    const x = Math.floor(col / nColsArea)
    const y = Math.floor(row / nRowsArea)
    
    let ret = new Set(
        getRowIndices(geometry, row)
        .concat(getColumnIndices(geometry, col))
        .concat(getAreaIndices(geometry, x, y)))
    ret.delete(idx)

    return ret
}

const getCandidates = (board, geometry, idx) => {

    let candidates = new Set()

    const nonEmptySiblings = [...getSiblings(geometry, idx)].filter((idx) => {
        return board[idx] > 0
    })
    const usedValues = nonEmptySiblings.reduce((vals, idx) => {
        return vals.add(board[idx])
    }, new Set())
    const size = getSize(geometry)

    for (let val=1; val<=size; val++) {
        if (!usedValues.has(val)) {
            candidates.add(val)
        }
    }

    return candidates
}

const solve = (board, geometry, curIdx=0) => {

    const size = getSize(geometry)

    if (curIdx == size*size) {
        return true
    }

    if (board[curIdx] > 0) {
        return solve(board, geometry, curIdx+1)
    } else {
        let vals = getCandidates(board, geometry, curIdx)
        for (let val of vals) {
            board[curIdx] = val
            let valid = solve(board, geometry, curIdx+1)
            if (valid) return true
            board[curIdx] = 0
        }
        return false
    }

}

const printBoard = (board, size) => {

    let row = ""
    for (let i=0; i<size*size; i++) {
        if (i % size == 0 && row.length > 0) {
            console.log(row)
            row = ""
        }
        row += board[i] > 0 ? board[i] : "."
    }
    console.log(row)
}


const geometry = [3, 3]

let board = [
    6, 0, 0, 0, 0, 0, 4, 0, 0,
    0, 9, 0, 0, 0, 0, 0, 0, 3,
    0, 0, 0, 0, 0, 5, 0, 8, 0,
    0, 4, 0, 9, 0, 0, 6, 0, 0,
    5, 0, 0, 0, 0, 0, 0, 2, 0,
    0, 0, 3, 0, 0, 7, 0, 0, 1,
    0, 2, 0, 6, 0, 0, 0, 5, 0,
    0, 0, 0, 0, 3, 0, 9, 0, 0,
    0, 0, 1, 0, 0, 4, 0, 0, 7 
]

printBoard(board, getSize(geometry))
console.log()

if (solve(board, geometry)) {
    printBoard(board, getSize(geometry))
} else {
    console.log("No solution")
}

