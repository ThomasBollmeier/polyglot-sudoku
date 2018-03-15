const { intRange, cartesianProduct } = require('./listutils')

const getSize = ([nRowsArea, nColsArea]) => nRowsArea * nRowsArea

const getSiblings = (geometry, idx) => {

    const [nRowsArea, nColsArea] = geometry
    const size = getSize(geometry)
    const row = Math.floor(idx/size)
    const col = idx % size
    const x = Math.floor(col / nColsArea)
    const y = Math.floor(row / nRowsArea)
    
    const rowIndices = intRange(0, size).map(c => row*size + c)
    const colIndices = intRange(0, size).map(r => r*size + col)
    const areaIndices = cartesianProduct(
        intRange(0, nRowsArea), 
        intRange(0, nColsArea)).map((r, c) => {
            const row = y*nRowsArea + r
            const col = x*nColsArea + c
            return row * size + col
        })
    
    let ret = new Set([
        ...rowIndices,
        ...colIndices,
        ...areaIndices
    ])
    ret.delete(idx)

    return ret
}

const getCandidates = (board, geometry, idx) => {

    const nonEmptySiblings = [...getSiblings(geometry, idx)].filter((idx) => {
        return board[idx] > 0
    })
    const usedValues = nonEmptySiblings.reduce((vals, idx) => {
        return vals.add(board[idx])
    }, new Set())
    const size = getSize(geometry)
    
    return new Set(intRange(1, size+1).filter(v => !usedValues.has(v)))
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

