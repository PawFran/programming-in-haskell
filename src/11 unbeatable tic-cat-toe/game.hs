import Data.Function
import Data.List

size :: Int
size = 3

data GridValue = O | B | X -- B is for blank
                    deriving (Eq, Show)

type Grid = [[GridValue]] -- two dimentional board

emptyGrid :: Grid 
emptyGrid = replicate size blankRow
                where blankRow = replicate size B

isFull :: Grid -> Bool
isFull grid = grid & concat & notElem B

numberOf :: GridValue -> Grid -> Int
numberOf v g = length $ filter (== v) flattened
                    where flattened = concat g

turn :: Grid -> GridValue
turn g = if os <= xs then O else X
            where
                os = numberOf O g
                xs = numberOf X g

diag :: Grid -> [GridValue]
diag g = [g !! n !! n | n <- [0..size-1]]

wins :: GridValue -> Grid -> Bool
wins v g = any wholeLine (rows ++ cols ++ dias)
            where 
                wholeLine = all (== v)
                rows = g
                cols = transpose g
                dias = [diag g, reverseDiag g]
                reverseDiag x = diag (map reverse x)

won :: Grid -> Bool
won g = wins O g || wins X g

-- printGrid :: Grid -> IO ()
-- printGrid