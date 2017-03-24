import Numbers


import Network.MateLight.Simple
import Network.MateLight

import SDLEventProvider.SDLKeyEventProvider

import Control.Monad.State
import Control.Monad.Reader

import Data.Maybe
import qualified Network.Socket as Sock

import Numbers

type KeyStatus = (String, String, Integer)


-- data structure describing the current state of the game
data Status = Status { playerpos :: (Int, Int)      -- position of the player
                   , points :: [Point]              -- list of all red and green points currently on the field
                   , score :: Int                   -- the score
                   , nextSpawn :: (Int, Int, Float) -- information about spawning: (time to next spawn, time until turning green, velocity)
                   , generator :: BBS               -- the randomness generator
                   , timeToNext :: Int              -- frames left to next spawn
                   , playing :: Bool                -- are we in-game or showing the score?
                   , explosion :: Bool}             -- is there an explosion this frame?

-- data structure describing a point (red or green)
data Point = Point { position :: (Float, Float)     -- position of the point
                   , direction :: (Float, Float)    -- direction of the point (movement vector)
                   , life :: Int}                   -- 0 if green, otherwise frames left until point turns green

-- data structure describing a Blum-Blum-Shub randomness generator
data BBS = BBS Int Int

-- interaction
-- move the player
move :: KeyStatus -> Status -> Status
move ("Pressed", key, _) | key == "W" || key == "UP"                    = toUP
                         | key == "S" || key == "DOWN"                  = toDOWN
                         | key == "A" || key == "LEFT"                  = toLEFT
                         | key == "D" || key == "RIGHT"                 = toRIGHT
move ("Held", key, dur)  | dur >= 100 && (key == "W" || key == "UP")    = toUP
                         | dur >= 100 && (key == "S" || key == "DOWN")  = toDOWN
                         | dur >= 100 && (key == "A" || key == "LEFT")  = toLEFT
                         | dur >= 100 && (key == "D" || key == "RIGHT") = toRIGHT
move _ = id


toUP    (Status (x, y) ps sc nsp g ttn p ex) = Status (x, max (y - 1) 0) ps sc nsp g ttn p ex
toDOWN  (Status (x, y) ps sc nsp g ttn p ex) = Status (x, min (y + 1) 11) ps sc nsp g ttn p ex
toLEFT  (Status (x, y) ps sc nsp g ttn p ex) = Status (max (x - 1) 0, y) ps sc nsp g ttn p ex
toRIGHT (Status (x, y) ps sc nsp g ttn p ex) = Status (min (x + 1) 29, y) ps sc nsp g ttn p ex


-- query space key
querySpace :: KeyStatus -> Bool
querySpace ("Pressed", "SPACE", _) = True
querySpace ("Held", "SPACE", dur)  = dur >= 100
querySpace _                       = False


-- Lukas
-- renders the current status to the frame
toFrame :: Status -> ListFrame
toFrame (Status (x, y) ps _ _ _ _ _ ex) = ListFrame $ drawPlayer $ drawExpl drawPoints
  where
  -- drawing the red and green points on a black surface
  drawPoints                = [[foldl (helper i j) (Pixel 0 0 0) ps | i <- [0 .. 29]]| j <- [0 .. 11]]
  -- coloring the pixels around the player to show the explosion, if necessary
  drawExpl pxss | ex        = [[if fromIntegral ((x - i)^2 + (y - j)^2) <= 20.25 then Pixel 0x78 0xf5 0xff else (pxss !! j) !! i | i <- [0 .. 29]]| j <- [0 .. 11]]
                | otherwise = pxss
  -- draw the pixel where the player is white
  drawPlayer pxss           = [[if x == i && y == j then Pixel 0xff 0xff 0xff else (pxss !! j) !! i | i <- [0 .. 29]]| j <- [0 .. 11]]
  -- helper function to color a single pixel, finds out if it should be red/green/black
  helper fx fy pix (Point (px, py) _ l) | floor px == fx && floor py == fy = if l > 0 then Pixel 0xff 0 0 else Pixel 0 0xff 0
                                        | otherwise                        = pix

-- updates the position and lifetime of points
updatePoints :: Status -> Status
updatePoints (Status po ps sc nsp g ttn p ex) = Status po (map (updateL . borderY . borderX . updateXY) ps) sc nsp g ttn p ex
  where
  -- move the point with its movement vector
  updateXY (Point (x, y) (xvel, yvel) time)            = Point (x + xvel, y+yvel) (xvel, yvel) time
  -- reflect the point on the left or right border, if necessary
  borderX (Point (x, y) (xvel, yvel) time) | x < 0     = Point (-x, y) (-xvel, yvel) time
                                           | x > 30    = Point (60-x, y) (-xvel, yvel) time
                                           | otherwise = Point (x, y) (xvel, yvel) time
  -- reflect the point at the top or bottom border, if necessary
  borderY (Point (x, y) (xvel, yvel) time) | y < 0     = Point (x, -y) (xvel, -yvel) time
                                           | y > 12    = Point (x, 24-y) (xvel, -yvel) time
                                           | otherwise = Point (x, y) (xvel, yvel) time
  -- update the life (time until turning green)
  updateL (Point po vel 0)                             = Point po vel 0
  updateL (Point po vel time)                          = Point po vel (time - 1)


-- Oliver
-- renders the current score to the frame
showScore :: Status -> ListFrame
showScore state = display (digitToPixels ((score state) `div` 1000 `mod` 10)) (digitToPixels ((score state) `div` 100 `mod` 10)) (digitToPixels ((score state) `div` 10 `mod` 10)) (digitToPixels ((score state) `mod` 10))
  where
  -- calls the helper function on every pixel of the screen
  display d1 d2 d3 d4 = ListFrame $ map (\y -> map (\x -> helper x y d1 d2 d3 d4) [0..29]) [0..11]
  -- copies data from the number-representation-list and turns it into a pixel
  helper x y d1 d2 d3 d4  | x == 0 || x == 29 || y <= 1 || y >= 10               = Pixel 0 0 0
                          | (x >= 1 && x <= 7) && (d1 !! (y-2) !! (x-1)) == 1    = Pixel 0xff 0xff 0xff
                          | (x >= 8 && x <= 14) && (d2 !! (y-2) !! (x-8)) == 1   = Pixel 0xff 0xff 0xff
                          | (x >= 15 && x <= 21) && (d3 !! (y-2) !! (x-15)) == 1 = Pixel 0xff 0xff 0xff
                          | (x >= 22 && x <= 28) && (d4 !! (y-2) !! (x-22)) == 1 = Pixel 0xff 0xff 0xff
                          | otherwise                                            = Pixel 0 0 0


-- Dogukan
-- detects collisions, updates playing & explosion
collisionDetect :: Status -> Status
collisionDetect (Status (x, y) ps sc nsp g ttn _ _)  | collisiongreen = Status (x, y) ps sc nsp g ttn True True
                                                     | collisionred   = Status (x, y) ps sc nsp g ttn False False
                                                     | otherwise      = Status (x, y) ps sc nsp g ttn True False
  where
  collisiongreen = not $ null $ filter (\(Point (a,b) _ l) -> l == 0 && floor a == x && floor b == y) ps
  collisionred   = not $ null $ filter (\(Point (a,b) _ l) -> l > 0 && floor a == x && floor b == y) ps


-- if explosion: destroys the points in the explosion, updates score
-- otherwise: returns the state unchanged
killPoints :: Status -> Status
killPoints (Status (x, y) ps sc nsp g ttn p False) = Status (x, y) ps sc nsp g ttn p False
killPoints (Status (x, y) ps sc nsp g ttn p True)  = Status (x, y) ps' sc' nsp g ttn p True
  where
  -- ps' is ps without the points in the explosionradius
  ps' = filter (\(Point (a,b) _ _) -> (a - (fromIntegral x + 0.5))^2 + (b - (fromIntegral y + 0.5))^2 > 20.25) ps
  -- updated score
  sc' = sc + (length ps - length ps' + 1)^2


-- Thomas
-- randomness

-- generate n pseudo-random bits, changes the generator accordingly
randomBits :: BBS -> Int -> (BBS, [Bool])
randomBits b n = foldl (\(b', xs) _ -> let (b'', x) = randomBit b' in (b'', x:xs)) (b, []) [1..n]
  where randomBit (BBS modul s) = let s' = (s * s) `mod` modul in (BBS modul s', odd s')

-- generates a pseudo-random number between 0 and n (both ends included), changes the generator accordingly
randomNumber :: BBS -> Int -> (BBS, Int)
randomNumber b n | r <= n    = (b', r)
                 | otherwise = randomNumber b' n
  where
  -- the number generated from the bitlist
  r           = foldl (\x b -> if b then 2 * x + 1 else 2 * x) 0 xs
  -- new generator, bit-list
  (b', xs)    = randomBits b (bitLength n)
  -- function for calculating how many bits should be generated (bitlength of n)
  bitLength 0 = 0
  bitLength m = 1 + bitLength (m `div` 2)

-- generates a pseudo-random number between l and u (both ends included), changes the generator accordingly
randomRange :: BBS -> Int -> Int -> (BBS, Int)
randomRange b l u = let (b', r) = randomNumber b (u - l) in (b', r + l)

-- changes the randomness-generator of the state using the player-position,
-- this makes the game harder to predict
forwardStateRandomness :: Status -> Status
forwardStateRandomness (Status (x, y) ps sc nsp g ttn p ex) = Status (x, y) ps sc nsp (randomForward g ((x + y) `mod` 15)) ttn p ex
  where randomForward b n = fst (randomBits b n)

-- generates a new point if necessary, updates timeToNext
spawn :: Status -> Status
spawn (Status po ps sc nsp g ttn pl ex) | (ttn > 0 && not (null ps)) || (ttn < 5 && ttn > 0) = Status po ps sc nsp g (ttn - 1) pl ex
                                       | ttn >= 5 && null ps                                = Status po ps sc nsp g 4 pl ex
spawn (Status (x, y) ps sc (fn, fg, v) g _ pl ex)                                            = Status (x, y) (newPoint:ps) sc (fn', fg', v') g' ttn' pl ex
  where
  -- spawn where the player is not, move away from the player
  (g1, angle)       | x <= 14  = randomRange g 10 80
                    | x > 14   = randomRange g (-80) (-10)
  (g2, posInTopRow) | x <= 14  = randomRange g1 15 29
                    | x > 14   = randomRange g1 0 14
  -- point spawning gets faster, points stay red longer and get faster
  (g3, greenTime)              = randomRange g2 (fg - 6) (fg + 6)
  (g', ttn')                   = randomRange g3 (fn - 6) (fn + 6)
  fn'               | fn >= 37 = fn - 12
                    | fn < 37  = 25
  fg'                          = fg + 12
  v'                | v >= 1.5 = 1.5
                    | v < 1.5  = v + 0.05
  dx                           = v * (sin ((fromIntegral angle) * pi / 180))
  dy                           = v * (cos ((fromIntegral angle) * pi / 180))
  newPoint                     = Point (0.5 + fromIntegral posInTopRow, 0.5) (dx, dy) greenTime

getKeyDataTuples keyState = (map (\(k,t) -> ("Pressed",k,t)) (pressed $ keyState)) ++ (map (\(k,d) -> ("Held",k,d)) (held $ keyState)) ++ (map (\(k,t) -> ("Released",k,t)) (released $ keyState))

eventTest :: [EventT] -> MateMonad ListFrame Status IO ListFrame
eventTest evs = do
    state <- get
    let state' = if playing state
                 then killPoints $ collisionDetect $ spawn $ updatePoints $ foldl (\st (EventT mod ev) -> if mod == "SDL_KEY_DATA" then foldl (\st' key -> move key st') st (getKeyDataTuples (read $ show ev)) else st) state evs
                 else if foldl (\b (EventT mod ev) -> if mod == "SDL_KEY_DATA" then foldl (\b' key -> b' || querySpace key) b (getKeyDataTuples (read $ show ev)) else b) False evs
                      then Status (10, 6) [] 0 (350, 360, 0.4) (generator state) 1 True False
                      else state
    let frame  = if playing state' then toFrame state' else showScore state'
    put $ state'
    return frame


-- initialising the game
main :: IO ()
main = do
    showSDLControlWindow
    Sock.withSocketsDo $ runMateM (Config (fromJust $ parseAddress "134.28.70.172") 1337 (30, 12) (Just 40000) True [sdlKeyEventProvider]) eventTest (Status (10, 6) [] 0 (350, 360, 0.4) (BBS 45735016373 246912) 1 True False)

-- running code with "134.28.70.172" in line above will result in output at "pixelkino.haskell.de"
    