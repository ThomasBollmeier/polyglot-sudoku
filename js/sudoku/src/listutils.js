const intRangeInternal = (low, high, step, result) => 
    low < high ? intRangeInternal(low+step, high, step, [...result, low]) : result

const intRange = (low, high, step=1) => intRangeInternal(low, high, step, [])

const cartesianProduct = (as, bs) => 
    as.reduce(
        (acc, a) => acc.concat(bs.map(b => [a, b])),
        [])

module.exports = {
    intRange,
    cartesianProduct    
}