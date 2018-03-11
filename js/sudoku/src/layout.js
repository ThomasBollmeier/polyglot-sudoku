class Layout {

    constructor(nrowsArea, ncolsArea) {
        this.nrowsArea = nrowsArea
        this.ncolsArea = ncolsArea
    }

    getSize() {
        return this.nrowsArea * this.ncolsArea
    }

    getCells() {
        const size = this.getSize()
        cells = []
        for (let row=0; row < size; row++) {
            for (let col=0; col < size; col++) {
                cells.push([row, col])
           }
        }
        return cells
    }

}