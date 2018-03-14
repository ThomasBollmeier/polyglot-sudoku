const { intRange, cartesianProduct } = require('./listutils')

const getSize = (geometry) => {
    const [nRowsArea, nColsArea] = geometry;
    return nRowsArea * nRowsArea;
}

const getRowIndices = (geometry, row) => {
    const size = getSize(geometry)
    return intRange(0, size).map(col => row*size + col)
}

const getColumnIndices = (geometry, col) => {
    const size = getSize(geometry)
    return intRange(0, size).map(row => row*size + col)
}

const getAreaIndices = (geometry, x, y) => {
    
    const [nRowsArea, nColsArea] = geometry
    const size = getSize(geometry)

    return cartesianProduct(
        intRange(0, nRowsArea), 
        intRange(0, nColsArea)).map((r, c) => {
            const row = y*nRowsArea + r
            const col = x*nColsArea + c
            return row * size + col
        })
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

const solveInternal = (cells, geometry, curIdx) => {

    const size = getSize(geometry)

    if (curIdx == size*size) {
        return true
    }

    if (cells[curIdx] > 0) {
        return solveInternal(cells, geometry, curIdx+1)
    } else {
        let vals = getCandidates(cells, geometry, curIdx)
        for (let val of vals) {
            cells[curIdx] = val
            let valid = solveInternal(cells, geometry, curIdx+1)
            if (valid) return true
            cells[curIdx] = 0
        }
        return false
    }

}

const solve = ({cells, geometry}) => {
    let cellsClone = [...cells]
    return solveInternal(cellsClone, geometry, 0) ?
        {cells: cellsClone, geometry} : null
}

module.exports = {
    getSize: ({geometry}) => getSize(geometry),
    solve
}

