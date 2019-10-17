-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Data.Maybe(isNothing)
--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
  -- that the falling shape in the well satisfies the Shape Invariant (prop_Shape),
  -- that the size of the well is correct, i.e. equal to wellSize.
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (vector,shape) well r) = prop_Shape shape && wellSize == shapeSize well



-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls shape = addWalls' (addWalls'' shape) 

-- (Just Black)
emptyBlackRow len = replicate len (Just Black)
  
addWalls' :: Shape -> Shape 
addWalls' (S shape) = S (shiftLeft (shiftUp shape))
  where
    shiftLeft shape = map (++ [Just Black]) shape
    shiftUp shape = shape ++ replicate 1 (emptyBlackRow (length (head shape)))


addWalls'' ::Shape -> Shape 
addWalls'' (S shape) = S (addRight (addDown shape))
  where
    addRight shape= map ([Just Black]++) shape
    addDown shape = [emptyBlackRow (length (head shape))] ++ shape

myShape = allShapes !! 1
myRow = head (rows myShape)





-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls ((shiftShape v p) `combine` w)




-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply 
  where
    supply = [ allShapes !! randomFloatToIndex x | x <- rs]
    shape1 =  supply !! randomFloatToIndex (head rs) 

randomFloatToIndex::Double->Int
randomFloatToIndex float = (round (float * 10)) `mod` 7







-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris a t 
  | a == Tick = tick t
  | a == MoveDown = tick t
  | a == MoveLeft = Just (0, movePiece (-1) t)
  | a == MoveRight = Just (0, movePiece 1 t)
  | a == Rotate = Just (0, rotatePiece t)
  | otherwise = Just (0,t)

  -- data Action = Tick | MoveLeft | MoveRight | MoveDown | Rotate
  -- deriving ...

move :: Vector -> Tetris -> Tetris
move vector1 (Tetris (vector2,piece) well r) = Tetris (vector1 `vAdd` vector2,piece) well r 

movePiece :: Int -> Tetris -> Tetris
movePiece n (Tetris (v,p) w r) 
  | collision newTetris = Tetris (v,p) w r 
  | otherwise = newTetris
  where 
    newTetris = move (n,0) (Tetris (v,p) w r)


    
rotate :: Tetris -> Tetris
rotate (Tetris (v , p)w r ) = Tetris (v, rotateShape p)w r
  
rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision newTetris = t
  | otherwise = newTetris
  where 
    newTetris = rotate t

tick :: Tetris -> Maybe (Int,Tetris)
tick t 
  |collision (newState t) = dropNewPiece t
  |otherwise =  Just (0,newState t)
    where 
      newState t = move (0,1) t


dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v,p) w r) 
  | gameOver = Nothing
  | otherwise = Just(score, Tetris (startPosition, newPiece) newWell' newList)
  where 
    newWell = w `combine` (place (v,p))
    newPiece = head r
    newList = tail r
    (score, newWell') = clearLines newWell 
    gameOver = place(startPosition, newPiece) `overlaps` w



clearLines :: Shape -> (Int,Shape)
clearLines shape = (numRows,(shiftShape (0,numRows) filteredShape))
    where 
      filteredShape = filterShape shape
      height = (snd (shapeSize filteredShape))
      numRows = wellHeight-height
    



filterShape:: Shape->Shape
filterShape (S rows) = S(filter (any isNothing) rows)

collision :: Tetris -> Bool
collision (Tetris ((vectorX,vectorY),fallingPiece) well r)
  | vectorX < 0 = True
  | vectorX + shapeWidth > wellWidth = True  
  | vectorY + shapeHeight > wellHeight = True
  | overlaps  place' well = True
  | otherwise = False
  where  
    (shapeWidth,shapeHeight) = shapeSize fallingPiece
    place' = place ((vectorX,vectorY), fallingPiece)
        


