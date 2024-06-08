{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- | Main routine for Pong like game.  This program is intended to be
-- a minimal example of a framework that can be used as a template
-- or a skeleton for more complex games. (See README.md)

module Main (main, PongGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{---------------------------- Base ------------------------------
Holds things common to serveral modules.  Code tends to migrate
here to avoice circular dependencies, calling up in violation
of heirarchy.
----------------------------------------------------------------}

{-- Base/StaticConfig.hs --}

-- | Width of the main game window
width :: Int
width = 600

-- | Height of the main game window
height :: Int
height = 300

-- | How far to offset the game window inside the screen
offset :: Int
offset = 200

-- | The main window for the game
window :: Display
window = InWindow "Pong" (width,height) (offset,offset)

-- | A default background color 
background :: Color
background = black

-- | Radius of the ball
radius :: Float
radius = 10.0

paddleWidth :: Float 
paddleWidth = 20

paddleHeight :: Float
paddleHeight = 80

-- | Distance of the paddle from the net
paddleDistance :: Float
paddleDistance = 200  --100.0

-- | Speed of paddle in pixels per second
paddleSpeed :: Float 
paddleSpeed = 100

wallColor :: Color
wallColor = greyN 0.5

-- | Number of frames to show per second
fps :: Int
fps = 60

{----------------------------  Model -----------------------------
Here is the basic abstract model of the game.  Represents the state
of game and how that state changes over discrete incrments of time.
-------------------------------------------------------------------}

{-- Model/State.hs --}

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float,Float) -- ^ Pong ball (x,y) location.
  , ballVel :: (Float,Float) -- ^ Pong ball (x,y) velocity.
  , player1 :: Float         -- ^ Left player paddle height.
                             -- Measured from center of paddle to y=0.
                             -- Zero is the middle of the screen. 
  , player2 :: Float         -- ^ Right player paddle height.
  , paddleVel1 :: Float      -- ^ y-velocity of paddle 1.
  , paddleVel2 :: Float      -- ^ y-velocity of paddle 2.
  , paused :: Bool           -- ^ True iff play is paused.
  } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (-100, 50)
  , player1 = 0
  , player2 = 0
  , paddleVel1 = 0
  , paddleVel2 = 0
  , paused = False 
  }

{-- Model/Step --}

type Seconds = Float 

-- | Update the ball position using its current velocity
moveBall :: Seconds    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall dt game = game { ballLoc = (x',y') }
  where
    (x,y) = ballLoc game
    (vx,vy) = ballVel game
    x' = x + vx * dt
    y' = y + vy * dt

-- | Update the game by moving the ball 
-- Ignore the ViewPort argument 
update :: Seconds -> PongGame -> PongGame
update seconds game = 
  if paused game then game 
  else (paddleBounce . paddleMove seconds . wallBounce . moveBall seconds) game 
 

-- | Move the paddle as indicated by paddle velocity
paddleMove :: Seconds -> PongGame -> PongGame 
paddleMove dt game = game { player1 = p1' , player2 = p2' }
  where 
    limit = (fromIntegral height / 2) - (paddleHeight / 2)
    bound y = max (min y limit) (-limit)
    p1' = bound (player1 game + paddleVel1 game * dt) 
    p2' = bound (player2 game + paddleVel2 game * dt) 

-- | Detect a collision with a paddle.  Upon collisions,
-- change the velocity of the ball to bounce off the paddle. 
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    --radius = 10       -- Radius.  Use the same things as in `render`.
    (vx,vy) = ballVel game      -- The old velocities
    vx' = if paddleCollision game radius then -vx else vx

-- | Detect a collision with one of the side walls.  Upon collisions,
-- update the velocity of hte ball to bounce it off the wall. 
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    (vx,vy) = ballVel game      -- The old velocities
    vy' = if wallCollision (ballLoc game) radius then -vy else vy

type Radius = Float
type Position = (Float,Float)

-- | Give position and radius of the ball, return whether a collision
-- occurred. 
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) r = topCollision || bottomCollision
  where
    topCollision    = y - r <= - (fromIntegral height / 2)
    bottomCollision = y + r >=   fromIntegral height / 2

between :: Float -> Float -> Float -> Bool
between x lob hib = lob <= x && x <= hib 

paddleCollision :: PongGame -> Radius -> Bool 
paddleCollision game _r = player1Collision || player2Collision
  where
    (x,y) = ballLoc game
    hph = paddleHeight / 2
    player1Collision = between y (player1 game - hph) (player1 game + hph) && x < 0 && x + paddleDistance - paddleWidth < 0
    player2Collision = between y (player2 game - hph) (player2 game + hph) && x > 0 && x - paddleDistance + paddleWidth > 0

{------------------------------------ Control ---------------------------------- 
The use controls, how the user interacts with the model.  They mostly (always)
poke at the game state.  The model takes over the physics of the game after
that.

Note how the semantics start as inline code inside the event handlers.sss
As the game ecolves these tend to get factored out into an API like layer.

I find I use this common trick to refactor the semantics tied to keystrokes and 
mouseclick events to simple actions I can call as an API.
--------------------------------------------------------------------------------}

{--- Control/Play.hs  -- uses Control/Command.hs for actions -}

-- | Respond to key events. 
handleKeys :: Event -> PongGame -> PongGame 

-- | For an 'c' keypress, reset the ball to the center. 
handleKeys (EventKey (Char 'c') _ _ _) game = centerBall game 

-- | For a 'p' keypress, toggle the paused field.
handleKeys (EventKey (Char 'p') Down _ _) game = togglePause game 

-- | For 'w' move the left paddle north
handleKeys (EventKey (Char 'w') Down _ _) game = move Player1 North game
handleKeys (EventKey (Char 'w') Up   _ _) game = move Player1 Stop game 

-- | For 's' move the left paddle south
handleKeys (EventKey (Char 's') Down _ _) game = move Player1 South game 
handleKeys (EventKey (Char 's') Up   _ _) game = move Player1 Stop  game 

-- | For up arrow move the right paddle north 
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = move Player2 North game 
handleKeys (EventKey (SpecialKey KeyUp) Up   _ _) game = move Player2 Stop  game

-- | for down arrow move the right paddle south
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = move Player2 South game 
handleKeys (EventKey (SpecialKey KeyDown) Up   _ _) game = move Player2 Stop  game 

-- | Do nothing for the other events. 
handleKeys _ game = game 

{-- Control/Command.hs -- this is a callable API that provides all the user gameplay mechanics. ---}

-- | reset the ball to the center
centerBall :: PongGame -> PongGame 
centerBall game = game { ballLoc = (0,0) }

-- | pause and unpause the game
togglePause :: PongGame -> PongGame 
togglePause game = game { paused = (not . paused) game }

data Player = Player1 | Player2 
data Action = North | South | Stop

-- | start moving player in the given direction
move :: Player -> Action -> PongGame -> PongGame 
move Player1 North game = game { paddleVel1 = paddleSpeed } 
move Player2 North game = game { paddleVel2 = paddleSpeed } 
move Player1 South game = game { paddleVel1 = - paddleSpeed } 
move Player2 South game = game { paddleVel2 = - paddleSpeed } 
move Player1 Stop  game = game { paddleVel1 = 0 }
move Player2 Stop  game = game { paddleVel2 = 0 }

{-- App/Main --}

main :: IO ()
main = play window background fps initialState render handleKeys update

{-----------------------------------  View -------------------------------
Rendering a view onto the model state.  Changes to the visual appearance
of the game go here.
--------------------------------------------------------------------------}

{--- View/Play.hs --}

-- | Convert a game state into a picture
render :: PongGame -> Picture
render game =
  pictures
    [ renderBall
    , renderWalls
    , renderPaddle rose (-paddleDistance) (player1 game)
    , renderPaddle orange paddleDistance  (player2 game)
    , color green $ line [(0.0, fromIntegral height / 2), (0.0, (- (fromIntegral height)) / 2)]
    , translate (-100) (-100) $ scale 0.1 0.1 $ color white $ text (show $ fst (ballLoc game))
    , translate (-100) (-115) $ scale 0.1 0.1 $ color white $ text (show $ snd (ballLoc game))
    ]
  where
    -- The pong ball 
    renderBall = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    -- The bottom and top walls 
    wall :: Float -> Picture
    wall y = translate 0 y $ color wallColor $ rectangleSolid (fromIntegral width) 10

    renderWalls = pictures [wall (fromIntegral height / 2), wall (- (fromIntegral height / 2))]

    -- Picture of a paddle at given x,y offset from origin
    renderPaddle :: Color -> Float -> Float -> Picture
    renderPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight ]

    -- paddleColor = light (light blue)
