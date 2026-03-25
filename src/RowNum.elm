module RowNum exposing (rowNum)


-- Given the index into a rows array, return
-- the row number matching e.g. Model.playerRow.
rowNum : Int -> Int -> Int
rowNum numRows rowIdx =
    numRows - (rowIdx + 1)
