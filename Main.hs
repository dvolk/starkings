{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random
import UI.NCurses

type Vector2 = (Int, Int)

data Star = Star
  { starPosition :: Vector2
  , starName     :: String
  , starSym      :: Char
  } deriving Show

data BGStar = BGStar
  { bgStarPosition :: Vector2
  , bgStarSym      :: Char
  , bgStarDepth    :: Int 
  }

data GameState = GameState
  { stars      :: [Star]
  , background :: [BGStar]
  , camera     :: Vector2
  , viewSize   :: Vector2
  , players    :: [Player]
  , starLinks  :: M.Map Vector2 (S.Set Vector2)
  }

data Player = Player 
  { isHuman     :: Bool
  , playerId    :: Int
  , playerName  :: String
  , ownedStars  :: [Vector2]
  , focusedStar :: Int
  }

newtype GameT s m a = GameT (StateT s m a)
    deriving (Functor, Monad, MonadIO, MonadTrans, MonadState s)

type Game = GameT GameState Curses

runGame1 :: GameState -> Game a -> Curses GameState
runGame1 s (GameT f) = execStateT f s

io :: IO a -> Game a
io = liftIO

curses :: Curses a -> Game a
curses = lift

up, down, left, right :: Vector2 -> Vector2
up    (x, y) = (x, y - 1)
down  (x, y) = (x, y + 1)
right (x, y) = (x + 1, y)
left  (x, y) = (x - 1, y)

genBGStars :: Int -> Vector2 -> Vector2 -> IO [BGStar]
genBGStars density (a, b) (c, d) = do
  let sx = abs a + abs c
      sy = abs b + abs d
      n = (sx * sy) `div` density
  replicateM n $ do
    x <- randomRIO (-100, 100)
    y <- randomRIO (-100, 100)
    s <- randomElem "`.+*"
    dp <- randomRIO (2,5)
    return $ BGStar (x, y) s dp

drawEverything :: Game ()
drawEverything = do    
  g <- get
  (cx, cy)    <- gets camera
  sz@(sy, sx) <- curses screenSize'
  curses $ do
    draw $ do
      clear sz
      forM_ (background g) $ \s -> do
        let (px, py)   = bgStarPosition s
            (cx', cy') = (cx `div` bgStarDepth s, cy `div` bgStarDepth s)
        when (  px > cx'
              && px < cx' + sx 
              && py > cy'
              && py < cy' + sy) $ do
          drawStringAt (px - cx', py - cy') [bgStarSym s]
      withReverse $ do
      forM_ (stars g) $ \s -> do
        let (px, py) = starPosition s
        when (  px > cx 
              && px < cx + sx 
              && py > cy
              && py < cy + sy) $ do
          drawStringAt (px - cx, py - cy) [starSym s]
      withReverse $ 
        drawStringAt (1,1) $ "Star Kings 0.1 | Camera: " 
          ++ show (cx, cy) ++ " Star: "

resetViewSize :: Game ()
resetViewSize = do
  (sy, sx) <- curses screenSize'
  modify $ \st -> st { viewSize = (sx, sy) }

five :: (a -> a) -> a -> a
five f = f.f.f.f.f

loop :: Game GameState
loop = do
  c <- curses $ waitForChrs "qhlfHLJK"
  resetViewSize
  (p,_) <- getPlayer
  if c == 'q'
   then get
   else do 
    case c of
      'K' -> moveCamera (five down)
      'J' -> moveCamera (five up)
      'H' -> moveCamera (five left)
      'L' -> moveCamera (five right)
      'h' -> playerStar p pred >>= focusStar
      'l' -> playerStar p succ >>= focusStar
      'f' -> playerStar p id   >>= focusStar
    drawEverything
    loop

getPlayer :: Game (Player, [Player])
getPlayer = gets players >>= \(p0:ps) -> return (p0,ps)

modifyPlayer :: Player -> (Player -> Player) -> Game ()
modifyPlayer p f = 
  modify $ \st -> st { players = 
    map (\p' -> if playerId p == playerId p' then f p' else p') (players st) }

currentStar :: Game Int 
currentStar = focusedStar `fmap` head `fmap` gets players

playerStar :: Player -> (Int -> Int) -> Game Int
playerStar pl offset = do
  let n  = focusedStar pl
      ns = length (ownedStars pl)
      n' = offset n `mod` ns
  modifyPlayer pl $ \p -> p { focusedStar = n' }
  return $ n'

focusStar :: Int -> Game ()
focusStar n = do
  (p,_) <- getPlayer
  bx <- gets viewSize
  setCamera (centerOn bx (ownedStars p !! n))

moveCamera :: (Vector2 -> Vector2) -> Game ()
moveCamera f = modify $ \st -> st { camera = f (camera st) }

setCamera :: Vector2 -> Game ()
setCamera c = modify $ \st -> st { camera = c }

centerOn :: Vector2 -- bounds of level
         -> Vector2 -- coord to focus on
         -> Vector2 -- new camera position
centerOn (bx, by) (fx, fy) = (fx - bx `div` 2, fy - by `div` 2)

main :: IO ()
main = do
  let ss = [ Star (0,0) "Sol" 'S'
           , Star (10,10) "Proxima Centauri" 'P'
           , Star (70,30) "Bla" 'B'
           , Star (100,100) "Out of range!" 'O'
           , Star (-100,-100) "Hello!" 'H'
           , Star (70,13) "Sup" 'U'] 
      p1 = Player 
             { isHuman = True
             , playerName = "Player 1"
             , playerId = 0             
             , ownedStars = [(0,0), (10,10), (-100,-100)]
             , focusedStar = 0
             }
  bgs <- genBGStars 10 (-50,-50) (50,50)
  let gs = GameState 
             { stars = ss
             , background = bgs 
             , camera = (-5, -5) 
             , players = [p1]
             , viewSize = (0,0)
             , starLinks = M.fromList [((0,0), S.fromList [(10,10)])]
             }
  
  r <- runCurses $ do
    setEcho False
    _ <- setCursorMode CursorInvisible
    runGame1 gs loop
    
  print (starLinks r)

waitForChrs :: String -> Curses Char
waitForChrs cs = loop' where
  loop' = do
    w  <- defaultWindow
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop'
      Just (EventCharacter c) -> if c `elem` cs then return c else loop'
      _ -> loop'

draw :: Update () -> Curses ()
draw ioact = do
  w <- defaultWindow
  updateWindow w ioact
  render

withReverse :: Update () -> Update ()
withReverse upact = do
  setAttribute AttributeReverse True
  upact
  setAttribute AttributeReverse False

drawStringAt :: Vector2 -> String -> Update ()
drawStringAt (x, y) s = do
  moveCursor' (y - 1, x - 1)
  drawString s

clear :: (Int, Int) -> Update ()
clear (sy, sx) = do
  forM_ [1..sy] $ \y ->
    drawStringAt (1, y) (replicate (sx - 1) ' ')

screenSize' :: Curses Vector2
screenSize' = screenSize >>= \(y, x) ->
  return (fromIntegral y, fromIntegral x)

moveCursor' :: Vector2 -> Update ()
moveCursor' (y, x) = moveCursor (fromIntegral y) (fromIntegral x)

replaceElem :: [a] -> a -> Int -> [a]
replaceElem es e n = take n es ++ e : drop (n + 1) es

removeElem :: [a] -> Int -> [a]
removeElem es n = take n es ++ drop (n + 1) es

randomElem :: [a] -> IO a
randomElem xs = (xs !!) `fmap` randomRIO (0, length xs - 1)
