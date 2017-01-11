{- Simple game loop example. -}

import System.IO
import Data.Fixed
import Data.Bifunctor
import Data.Maybe (maybe)

inputTimeout :: Int
inputTimeout = 50000

stepLength :: Double
stepLength = 0.1

rotationStep :: Double
rotationStep = 0.06

mapSize, screenSize :: (Int, Int)
mapHeight, mapWidth, screenHeight, screenWidth :: Int
mapSize@(mapWidth,mapHeight) = (15,15)
screenSize@(screenWidth,screenHeight) = (175,55)

fieldOfView :: Double
fieldOfView = pi / 1.5

focalLength :: Double
focalLength = 0.5

maxRaycastIterations :: Int
maxRaycastIterations = 20

totalMapSquares :: Int
totalMapSquares = uncurry (*) mapSize

rayAngleStep :: Double
rayAngleStep = fieldOfView / fromIntegral screenWidth

data MapSquare = Empty | Wall deriving (Eq, Show)

data Normal = North | East | South | West

gameMap1 :: [MapSquare]
gameMap1 = map (\s -> if s == (0 :: Int) then Empty else Wall)
  [0,0,0,0,0,0,0,0,0,0,0,1,1,0,0
  ,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0
  ,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0
  ,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0
  ,0,0,0,1,0,0,1,0,0,0,0,0,1,0,1
  ,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ]

data GameState = GameState
  {
    playerPos :: (Double,Double),     -- x, y, starting top left (map AND square)
    playerRot :: Double,             -- rotation in radians, CCW, 0 = facing right
    gameMap :: [MapSquare]
  } deriving (Show)

initialGameState :: GameState
initialGameState = GameState
  {
    playerPos = (0.5,0.5),
    playerRot = 0.0,
    gameMap = gameMap1
  }

grayscaleMap :: [Char]
grayscaleMap = [                    -- characters sorted by brigtness
 -- '#','$','@','B','%','8','&','W','M','0','Q','*','o','a','h','k','b','d','p','q','w','m','Z','O','I','L','C','J','U','Y','X','z','c','v','u','n',
 -- 'x','r','j','f','t','\\','|','(',')','1','{','}','[',']','?','-','_','+','~','>','i','!','l',';',':',',','\'','\"','^','`','\'','.']
    'M','$','o','?','/','!',';',':','\'','.','-']

-----------------------------------------------   Ensures given values is in given interval by clamping it.

clamp :: (Ord a) => a -> a -> a -> a
clamp value lower upper = min upper $ max lower value

-----------------------------------------------   Adds two 2-item couples tuples, itemwise.

addCouples :: (Num a) => (Num b) => (a, b) -> (a, b) -> (a, b)
addCouples (a1,a2) (b1,b2) = (a1 + b1, a2 + b2)

-----------------------------------------------   Applies floor function to both items of a 2 item tuple.

floorCouple :: (RealFrac a) => (RealFrac b) => (a, b) -> (Int, Int)
floorCouple = bimap floor floor

-----------------------------------------------   Converts 2D map coords to 1D array coords.

mapToArrayCoords :: (Int, Int) -> Int
mapToArrayCoords = uncurry (+) . second (* mapWidth)

-----------------------------------------------   Converts 1D array coords to 2D map coords.

arrayToMapCoords :: Int -> (Int, Int)
arrayToMapCoords coords = let (y,x) = coords `divMod` mapWidth in (x,y)

-----------------------------------------------

angleTo02Pi :: Double -> Double
angleTo02Pi = (`mod'` (2 * pi))

-----------------------------------------------   Maps normalized intensity to ASCII character.

intensityToChar :: Double -> Char
intensityToChar intensity =
  grayscaleMap !! clamp (floor (intensity * fromIntegral (length grayscaleMap))) 0 (length grayscaleMap - 1)

-----------------------------------------------   Returns an intensity addition (possibly negative) cause by distance.

distanceToIntensity :: Double -> Double
distanceToIntensity distance = min (distance / 7.0) 1.0 * (-0.3)

-----------------------------------------------   Renders the 3D player view into String.

render3Dview :: [(Double, Normal)] -> Int -> String
render3Dview drawInfo height = unlines [map (toChar i) drawInfo | i <- [1..height]]
  where
    heightDouble = fromIntegral height
    toChar row item =
      if distanceFromMiddle < columnHeight
      then let shadowTerm = case snd item of
                 North -> 0.0
                 East  -> 0.3
                 South -> 0.6
                 West  -> 0.9
           in intensityToChar $ shadowTerm + distanceToIntensity (fst item)
      else ' ' --intensityToChar ( 5 *  (fromIntegral distanceFromMiddle) / heightFrac )
      where
        middle = div height 2 + 1
        distanceFromMiddle = abs (middle - row)
        columnHeight = floor ((1.0 / (fst item + 1.0)) * heightDouble)

-----------------------------------------------   Renders the game in 3D.

renderGameState3D :: GameState -> String
renderGameState3D gameState =
  --  (renderGameStateSimple gameState)
  --  ++
  --  "\n"
  --  ++
  render3Dview drawInfo screenHeight
  where drawInfo = castRays gameState

-----------------------------------------------   Gets the distance from projection origin to projection plane.

distanceToProjectionPlane :: Double -> Double -> Double
distanceToProjectionPlane focalDistance angleFromCenter =
  focalDistance * tan angleFromCenter

-----------------------------------------------   Casts all rays needed to render player's view, returns a list of ray cast results.

castRays :: GameState -> [(Double, Normal)]
castRays gameState@(GameState pPos pRot _) =
  [let (dist, normal) = rayResult $ fromIntegral x
   in  (max (dist - distanceToProjectionPlane focalLength (abs $ pRot - rayDirection (fromIntegral x))) 0.0, normal) | x <- [0..screenWidth - 1]]
  where
    rayDirection x = pRot + fieldOfView / 2 - x * rayAngleStep
    rayResult x = castRay gameState pPos (floorCouple pPos) (rayDirection x) maxRaycastIterations

-----------------------------------------------   Casts a ray and returns an information (distance, normal) about a wall it hits.

castRay :: GameState -> (Double, Double) -> (Int, Int) -> Double -> Int ->  (Double, Normal)
castRay gameState rayOrigin square rayDirection maxIterations =
    if mapSquareAt gameState square /= Empty || maxIterations == 0 then (0,North)
    else let squareCastResult = castRaySquare square rayOrigin angle
             recursionResult = castRay gameState (fst squareCastResult) (addCouples square (snd squareCastResult)) angle (maxIterations - 1)
         in (pointPointDistance rayOrigin (fst squareCastResult) + fst recursionResult,
            if fst recursionResult /= 0
              then snd recursionResult
              else case snd squareCastResult of
                     (1,0)  -> East
                     (0,1)  -> South
                     (-1,0) -> West
                     _      -> North)
  where
    -- squareCoords = floorCouple rayOrigin
    angle = angleTo02Pi rayDirection

-----------------------------------------------   Casts a ray inside a single square, returns (intersection point with square bounds,next square offset)

castRaySquare :: (Int, Int) -> (Double, Double) -> Double -> ((Double, Double),(Int, Int))
castRaySquare (x,y) rayPosition rayAngle
  | pointPointDistance rayPosition intersection1 <=
    pointPointDistance rayPosition intersection2 = (intersection1,(if boundX == x then -1 else 1,0))
  | otherwise = (intersection2,(0,if boundY == y then -1 else 1))
  where
    angle = 2 * pi - rayAngle
    boundX = x + if angle < (pi / 2) || angle > (pi + pi / 2) then 1 else 0
    boundY = y + if angle < pi then 1 else 0
    intersection1 = lineLineIntersection rayPosition angle (fromIntegral boundX,fromIntegral y) (pi / 2)
    intersection2 = lineLineIntersection rayPosition angle (fromIntegral x,fromIntegral boundY) 0

-----------------------------------------------   Gets distance of two points.

pointPointDistance :: (Double, Double) -> (Double, Double) -> Double
pointPointDistance (x1,y1) (x2,y2) = sqrt (dx * dx + dy * dy)
  where
    dx = x1 - x2
    dy = y1 - y2

-----------------------------------------------   Makes the angle safe for tan function.

tanSafeAngle :: Double -> Double
tanSafeAngle angle
  | angle `mod'` (pi / 2) == 0.0 = angle + 0.00001
  | otherwise                    = angle

-----------------------------------------------   Computes an intersection point of two lines.

lineLineIntersection :: (Double, Double) -> Double -> (Double, Double) -> Double -> (Double, Double)
lineLineIntersection (p1x, p1y) angle1 (p2x, p2y) angle2
  | abs tan1 < abs tan2 = (x, tan1 * x + (p1y - tan1 * p1x))
  | otherwise           = (x, tan2 * x + (p2y - tan2 * p2x))
  where
    tan1 = tan (tanSafeAngle angle1)
    tan2 = tan (tanSafeAngle angle2)
    x = (p2y - tan2 * p2x - p1y + tan1 * p1x) / (tan1 - tan2)

-----------------------------------------------   Renders the game state into string, simple version.

renderGameStateSimple :: GameState -> String
renderGameStateSimple (GameState pPos@(px,py) pRot gMap) =
  concatMap (\(cell,index) ->
              (if index `mod` mapWidth == 0 then "\n" else "") ++
              (if isPlayer index then playerRep else if cell == Empty then "  " else "[]"))
            (zip gMap [0..])
  ++ "\n" ++
  unlines ["pos: " ++ show pPos, "rot: " ++ show pRot]
  where
    isPlayer index = (floor px, floor py) == arrayToMapCoords index
    playerRep = case round (4.0 * pRot / pi) :: Int of
                  0 -> "->"
                  1 -> "/^"
                  2 -> "|^"
                  3 -> "^\\"
                  4 -> "<-"
                  5 -> "./"
                  6 -> ".|"
                  7 -> "\\."
                  8 -> "->"
                  _ -> error $ "Impossible player rotation: " ++ show pRot

-----------------------------------------------   Returns map square at given coords.

mapSquareAt :: GameState -> (Int, Int) -> MapSquare
mapSquareAt (GameState _ _ gMap) coords@(x,y) =
  if x < mapWidth && x >= 0 && y < mapHeight && y >= 0
    then gMap !! mapToArrayCoords coords
    else Wall

-----------------------------------------------   Moves the player forward by given distance, with collisions.

movePlayer :: GameState -> Double -> GameState
movePlayer (prev@GameState {playerPos = pPos}) dist = prev
  { playerPos = bimap (+ if isWalkable (first  (+ plusX) pPos) then plusX else 0)
                      (+ if isWalkable (second (+ plusY) pPos) then plusY else 0)
                      pPos
  }
  where
    plusX = cos (playerRot prev) * dist
    plusY = -1 * (sin (playerRot prev) * dist)
    isWalkable pos = mapSquareAt prev (bimap floor floor pos) == Empty

-----------------------------------------------   Computes the next game state.

nextGameState :: GameState -> Char -> Maybe GameState
nextGameState _    'q' = Nothing
nextGameState prev 'w' = Just $ movePlayer prev stepLength
nextGameState prev 's' = Just $ movePlayer prev (-1 * stepLength)
nextGameState prev 'a' = Just $ prev {playerRot = angleTo02Pi (playerRot prev + rotationStep)}
nextGameState prev 'd' = Just $ prev {playerRot = angleTo02Pi (playerRot prev - rotationStep)}
nextGameState prev _   = Just prev

-----------------------------------------------   Main game loop.

loop :: GameState -> IO ()
loop gameState = do
  putStrLn (renderGameState3D gameState)
  hFlush stdout
  maybe (return ()) loop . nextGameState gameState =<< getChar

-----------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering                     -- to read char without [enter]
  hSetBuffering stdout (BlockBuffering (Just 20000))  -- to read flickering
  hSetEcho stdin False
  loop initialGameState
