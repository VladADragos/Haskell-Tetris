-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape (w,h) = S (replicate h (emptyRow w))


emptyRow:: Int -> Row
emptyRow len = replicate len Nothing

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S shape) = (length (head shape), length shape)

-- ** A03

testShape = emptyShape (3,3)
-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S shape )= length concatList - length emptyValues
        where concatList = concat shape
              emptyValues = filter (==Nothing) concatList

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)

prop_Shape :: Shape -> Bool
prop_Shape (S rows) | null rows = False
                    | isSameLength rows = True
                    | otherwise = False
  where isSameLength rows = all (==length(head rows)) (map length rows)
  



  

    



-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]
  
instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes
  
instance Arbitrary Shape where
  arbitrary = rShape

  
-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S shape) = S (map reverse (transpose shape))

-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (x, y) (S shape) = S (shiftRight x (shiftDown y shape))
  where
    shiftRight dist =
      map (emptyRow dist ++ )
    shiftDown dist shape = 
      replicate dist (emptyRow (length (head shape))) ++ shape

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (x, y) (S shape) = S (shiftLeft x (shiftUp y shape))
  where
    shiftLeft dist =
      map (++ emptyRow dist)
    shiftUp dist shape = 
      shape ++ replicate dist (emptyRow (length (head shape)))

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (x, y) shape = padShape (x - first shape, y - second shape) shape
  where first shape  = fst (shapeSize shape)
        second shape = snd (shapeSize shape)



-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps (S row1) (S row2) = overlaps' row1  row2

overlaps' (x:xs) (y:ys) | rowsOverlap x y = True
                  | otherwise       = overlaps' xs ys
overlaps' _ _                       = False



rowsOverlap :: Row -> Row -> Bool
rowsOverlap (x:xs) (y:ys) | isNothing x || isNothing y = rowsOverlap xs ys
                          | otherwise                  = True
rowsOverlap _ _                                        = False

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S shape1) (S shape2) = S (zipWith (zipWith f) shape1 shape2)


blackClashes :: Shape -> Shape -> Shape
blackClashes shape1 shape2 = zipShapeWith clash shape1 shape2  
  where clash :: Square -> Square -> Square 
        clash Nothing Nothing = Nothing
        clash Nothing s       = s
        clash s       Nothing = s
        clash (Just c1) (Just c2) = Just Black

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
combine shape1 shape2 = blackClashes (pad shape1 shape2) (pad shape2 shape1)
  where
    pad shape1 shape2 = padShapeTo ( width shape1 shape2, height shape1 shape2) shape1
    height (S shape1) (S shape2) = max (length shape1) (length shape2) 
    width  (S shape1) (S shape2) = max (length (head shape1)) (length (head shape2))