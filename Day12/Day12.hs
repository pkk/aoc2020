
data Direction = East Int | West Int | Noth Int | South Int deriving (Show)

main :: IO ()
main = do
    directions <- map parseAction . lines <$> readFile "input.txt"
    print $ directions !! 0
    print $ manhattanDistance $ navigateShip directions
    print $ manhattanDistance $ navigateShipWaypoint directions

type Action = (Char, Int)

data Dir = N | E | S | W deriving (Show, Eq, Ord)

data Ship = Ship {
    direction :: Dir,
    coords :: (Int, Int)
} deriving (Show, Eq)

manhattanDistance :: Ship -> Int
manhattanDistance ship = abs x + abs y
    where
        (x,y) = coords ship

parseAction :: String -> (Char, Int)
parseAction (action:amount) = (action, (read :: String -> Int) amount)

navigateShip :: [Action] -> Ship
navigateShip = foldl guideShip (Ship E (0,0))

navigateShipWaypoint :: [Action] -> Ship
navigateShipWaypoint actions = fst (go actions)
  where
    go = foldl guideShipWayPoint ((Ship E (0,0)), (10,1))

guideShip :: Ship -> Action -> Ship
guideShip ship (action, amount) = do 
    let (x,y) = coords ship
    case action of
        'F' -> ship {
            coords = case direction ship of
                N -> (x, y + amount)
                E -> (x + amount, y)
                W -> (x - amount, y)
                S -> (x, y - amount)
        }
        'N' -> ship { coords = (x, y + amount) }
        'E' -> ship { coords = (x + amount, y)}
        'W' -> ship { coords = (x - amount, y)}
        'S' -> ship { coords = (x, y - amount) }
        _ -> case amount of 
            90 -> ship {
                direction = case action of
                    'R' -> case direction ship of
                        N -> E
                        E -> S
                        S -> W
                        W -> N
                    'L' -> case direction ship of 
                        N -> W
                        E -> N
                        S -> E
                        W -> S
            }
            180 -> ship {
                direction = case direction ship of
                    N -> S
                    S -> N
                    E -> W
                    W -> E
            }
            270 -> ship {
               direction = case action of
                    'R' -> case direction ship of
                        N -> W
                        E -> N
                        S -> E
                        W -> S
                    'L' -> case direction ship of 
                        N -> E
                        E -> S
                        S -> W
                        W -> N 
            }

type Waypoint = (Int,Int)

startWaypoint :: Waypoint
startWaypoint = (10,1)

guideShipWayPoint
  :: (Ship, Waypoint)
  -> Action
  -> (Ship, Waypoint)
guideShipWayPoint (ship, (x,y)) (action, amount) =
  case action of
    'F' -> (movedShip, (x,y))
    'N' -> (ship, (x,y+amount))
    'E' -> (ship, (x+amount,y))
    'W' -> (ship, (x-amount,y))
    'S' -> (ship, (x,y-amount))
    _   ->
     case amount of
       90 ->
         case action of
           'R' -> (ship, (y, negate x))
           'L' -> (ship, (negate y, x))
       180 ->
         iterate
           (flip guideShipWayPoint
              (action, 90)) (ship, (x,y)) !! 2
       270 ->
         iterate
           (flip guideShipWayPoint
              (action, 90)) (ship, (x,y)) !! 3
    where
      (shipX, shipY) = coords ship
      movedShip =
        ship { coords =
                 ( shipX + (x * amount)
                 , shipY + (y * amount)
                 )
             }