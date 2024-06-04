{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
-- | Main routine for Pong game
module Main (main, PongGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad (zipWithM)

{---------------------------- Base ------------------------------
Holds things common to serveral modules.  Code tends to migrate
here to avoice circular dependencies, calling up in violation
of heirarchy.
----------------------------------------------------------------}
width, height, offset :: Int
width = 300
height = 300
offset = 100

-- | The main window for the game
window :: Display
window = InWindow "Pong" (width,height) (offset,offset)

-- | A default background color 
background :: Color
background = black

-- | Number of frames to show per second
fps :: Int
fps = 60

{----------------------------  Model ------------------------------
Here is the basic abstract model of the game.  Represents the state
of game and how that state changes over discrete incrments of time.
-------------------------------------------------------------------}

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float,Float) -- ^ Pong ball (x,y) location.
  , ballVel :: (Float,Float) -- ^ Pong ball (x,y) velocity.
  , player1 :: Float         -- ^ Left player paddle height.
                             -- Zero is the middle of the screen. 
  , player2 :: Float         -- ^ Right player paddle height
  , paused :: Bool
  } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (-20, 10)
  , player1 = 0
  , player2 = 0
  , paused = False 
  }

-- | Update the ball position using its current velocity
moveBall :: Float  -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x',y') }
  where
    -- Old locations and velocities
    (x,y) = ballLoc game
    (vx,vy) = ballVel game

    -- New locations 
    x' = x + vx * seconds
    y' = y + vy * seconds


-- | Update the game by moving the ball 
-- Ignore the ViewPort argument 
update :: Float -> PongGame -> PongGame
update seconds game = 
  if paused game then game 
  else (paddleBounce . wallBounce . moveBall seconds) game 
 
-- | Detect a collision with a paddle.  Upon collisions,
-- change the velocity of the ball to bounce off the paddle. 
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    -- Radius.  Use the same things as in `render`.
    radius = 10

    -- The old velocities
    (vx,vy) = ballVel game

    vx' = if paddleCollision game radius 
          then
            -- Update the velocity 
            (-1) * vx
          else
            -- Do nothing.  Return old velocity. 
            vx

-- | Detect a collision with one of the side walls.  Upon collisions,
-- update the velocity of hte ball to bounce it off the wall. 
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius.  Use the same things as in `render`.
    radius = 10

    -- The old velocities
    (vx,vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
            -- Update the velocity 
            -vy
          else
            -- Do nothing.  Return old velocity. 
            vy


type Radius = Float
type Position = (Float,Float)

-- | Give position and radius of the ball, return whether a collision
-- occurred. 
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= - (fromIntegral width / 2)
    bottomCollision = y + radius >=   fromIntegral width / 2

between :: Float -> Float -> Float -> Bool
between x lob hib = lob <= x && x <= hib 

paddleCollision :: PongGame -> Radius -> Bool 
paddleCollision game _r = player1Collision || player2Collision
  where
    (x,y) = ballLoc game
    player1Collision = between y (player1 game - 40) (player1 game + 40) && x < 0 && x + 100 < 0
    player2Collision = between y (player2 game - 40) (player2 game + 40) && x > 0 && x - 100 > 0


{------------------------------------ Control ----------------------------------
The use controls, how the user interacts with the model.  They mostly (always)
poke at the game state.  The model takes over the physics of the game after
that.

Note how the semantics start as inline code inside the event handlers.
As the game ecolves these tend to get factored out into an API like layer.
--------------------------------------------------------------------------------}

-- | Respond to key events. 
handleKeys :: Event -> PongGame -> PongGame 

-- | For an 'c' keypress, reset the ball to the center. 
handleKeys (EventKey (Char 'c') _ _ _) game = game { ballLoc = (0,0) }

-- | For a 'p' keypress, toggle the paused field.
handleKeys (EventKey (Char 'p') Down _ _) game = game { paused = (not . paused) game }

-- | For 'w' move the left paddle up
handleKeys (EventKey (Char 'w') Down _ _) game = game { player1 = player1 game + 10 }

-- | For 's' move the left paddle down
handleKeys (EventKey (Char 's') Down _ _) game = game { player1 = player1 game - 10 }

-- | For up arrow move the right paddle up 
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { player2 = player2 game + 10}

-- | for down arrow move the right paddle down
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2 = player2 game - 10 }

-- | Do nothing for the other events. 
handleKeys _ game = game 

main :: IO ()
main = play window background fps initialState render handleKeys update


{-----------------------------------  View -------------------------------
Rendering a view onto the model state.  Changes to the visual appearance
of the game go here.
--------------------------------------------------------------------------}

-- | Convert a game state into a picture
render :: PongGame -> Picture
render game =
  pictures
    [ ball
    , walls
    , mkPaddle rose (-120) (player1 game)
    , mkPaddle orange 120  (player2 game)
    , color white $ line [(0.0, 150.0), (0.0, -150.0)]
    , translate (-100) (-100) $ scale 0.1 0.1 $ color white $ text (show $ fst (ballLoc game))
    , translate (-100) (-115) $ scale 0.1 0.1 $ color white $ text (show $ snd (ballLoc game))
    ]
  where
    -- The pong ball 
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    -- The bottom and top walls 
    wall :: Float -> Picture
    wall offset' = translate 0 offset' $ color wallColor $ rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    -- Make a paddle of a given border and vertical offset 
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 20 80 ]

    -- paddleColor = light (light blue)
