module Main where

import UI.NCurses
import System.Random
import Control.Monad
import Data.Time.Clock
import Control.Monad.IO.Class

data Direction = DirUp | DirDown | DirRight | DirLeft deriving (Enum, Eq, Show)
data Result = Running | Finished deriving (Enum)
data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)

toPoint :: Direction -> Point
toPoint dir = case dir of
        DirUp -> Point {x = 0, y = -1}
        DirDown -> Point {x = 0, y = 1}
        DirLeft -> Point {x = -1, y = 0}
        DirRight -> Point {x = 1, y = 0}

add :: Point -> Point -> Point
add a b = Point{x = (x a) + (x b), y = (y a) + (y b)}

data GameState = GameState {
     currentDir :: Direction,
     currentPos :: Point,
     body :: [Point],
     prevUpdate :: UTCTime,
     desiredLength :: Int,
     randGen :: StdGen,
     foodPos :: Point
     } deriving (Show)

updateDir :: GameState -> Direction -> GameState
updateDir state dir = state {currentDir = dir}

updateState :: GameState -> UTCTime -> GameState
updateState state time = do
           let dur = diffUTCTime time (prevUpdate $ state)
           if dur >= secondsToNominalDiffTime 0.3
           then updateBody $ state {prevUpdate = time}
           else state

updateBody :: GameState -> GameState
updateBody state = do
           let nextPos = add (currentPos $ state) (toPoint . currentDir $ state)
           let appendedBody = nextPos : body state
           let truncatedBody = reverse . tail . reverse $ appendedBody

           let nextBody = if length appendedBody <= desiredLength state
                          then appendedBody else truncatedBody

           let foodEaten = foodPos state == currentPos state
           let nextLength = desiredLength state + (if foodEaten then  1 else 0)
           let nextFood = if foodEaten then placeFood state else (foodPos state, randGen state)

           state {body = nextBody, currentPos = nextPos, desiredLength = nextLength, foodPos = fst nextFood, randGen = snd nextFood}

placeFood :: GameState -> (Point, StdGen)
placeFood state = do
          let rand gen = randomR (1, 19) gen
          let firstVal = rand (randGen state)
          let secondVal = rand (snd firstVal)
          let randX = fst firstVal
          let randY = fst secondVal
          let randPoint = Point {x = randX, y = randY}
          (randPoint, snd secondVal)

gameOver :: GameState -> Bool
gameOver state = do
              let pos = currentPos state
              let outOfBounds = x pos >= 20 || x pos <= 0 || y pos >= 20 || y pos <= 0
              let emptyBody = null . body $ state
              let selfCollision = elem (currentPos $ state)  (tail . body $ state)
              outOfBounds || (not emptyBody && selfCollision)

initialState :: UTCTime -> StdGen -> GameState
initialState time rnd = GameState {
             currentDir = DirDown,
             currentPos = Point {x = 5, y = 5},
             body = [],
             prevUpdate = time,
             desiredLength = 5,
             randGen = rnd,
             foodPos = Point {x = 2, y = 3} }

resetState :: GameState -> GameState
resetState state = initialState (prevUpdate state) (randGen state)

main ::IO()
main = runCurses $ do
  setEcho False
  win <- defaultWindow
  now <- liftIO $ getCurrentTime
  rnd <- liftIO $ getStdGen
  let start = initialState now rnd
  runLoop win start Running



drawBorders ::  Update()
drawBorders = do
         moveCursor 20 20
         drawGlyph glyphPlus
         moveCursor 20 40
         drawGlyph glyphPlus
         moveCursor 40 20
         drawGlyph glyphPlus
         moveCursor 40 40
         drawGlyph glyphPlus

         moveCursor 20 21
         drawLineH (Just glyphLineH) 19
         moveCursor 40 21
         drawLineH (Just glyphLineH) 19

         moveCursor 21 20
         drawLineV (Just glyphLineV) 19
         moveCursor 21 40
         drawLineV (Just glyphLineV) 19

drawFood :: GameState -> Update()
drawFood state = do
            let p = foodPos state
            let nextX = toInteger $ 20 + (x p)
            let nextY = toInteger $ 20 + (y p)

            moveCursor nextY nextX
            drawGlyph glyphDegree

drawHead :: GameState -> Update()
drawHead state = do
            moveToPoint . currentPos $ state
            drawGlyph glyphDiamond

drawBody :: GameState -> Update()
drawBody state = mapM_ drawSegment (body state)


drawSegment :: Point -> Update()
drawSegment p = do
            moveToPoint p
            drawGlyph glyphBlock


moveToPoint :: Point -> Update()
moveToPoint p = do
            let nextX = toInteger $ 20 + (x p)
            let nextY = toInteger $ 20 + (y p)
            moveCursor nextY nextX


getDir :: Event -> GameState  -> Direction
getDir ev state = case ev of
                 EventCharacter 'w' -> DirUp
                 EventCharacter 'a' -> DirLeft
                 EventCharacter 's' -> DirDown
                 EventCharacter 'd' -> DirRight
                 _                  -> currentDir state


-- The main loop of the game
runLoop :: Window -> GameState -> Result -> Curses ()
runLoop win prevState Running = do
            now <- liftIO $ getCurrentTime

            let state = updateState prevState now
            let dir = currentDir state

            updateWindow win $ do
                clear
                moveCursor 1 10
                drawString . show $ dir
                moveCursor 2 10
                drawString . show . body $ state
                moveCursor 3 10
                drawString . show . prevUpdate $ state
                moveCursor 4 10
                drawString . show $ now

                drawBorders
                drawFood state
                drawBody state
                drawHead state

                moveCursor 0 0
            render

            let idle = runLoop win state (if gameOver state then Finished else Running)

            ev <- getEvent win (Just 100)
            case ev of
                Nothing -> idle
                Just ev' -> do
                                let nextDir = getDir ev' state
                                runLoop win (updateDir state nextDir) Running


-- The game over screen
runLoop win prevState Finished = do
            now <- liftIO $ getCurrentTime
            updateWindow win $ do
                clear
                moveCursor 1 10
                drawString "Game Over"
                moveCursor 2 10
                drawString "Press 'q' to exit game, 'e' to start a new game..."
            render

            let idle = runLoop win prevState Finished
            let newGame = runLoop win (resetState prevState) Running

            ev <- getEvent win (Just 100)
            case ev of
                Nothing -> idle
                Just ev' -> if ev' == EventCharacter 'q'
                            then return () else if ev' == EventCharacter 'e'
                                                then newGame else idle
