{-# LANGUAGE ViewPatterns, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}

import qualified Data.Map as M
-- import qualified Data.Set as S
import Control.Monad.State
import Control.Arrow (first,(&&&), (***))
import Data.Data
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Ord

-- import Control.Parallel.Strategies

--------------------------------------------------
-- Data Types
--------------------------------------------------

-- Board
data Token = Black | White
           deriving (Show, Eq, Data, Typeable)

type Position = (Integer, Integer)

data Reversi = Reversi {board :: M.Map Position Token,
                        currentPlayer :: Token}
             deriving (Show, Eq, Data, Typeable)

-- The type for the game interpreter.
type GameREPL = StateT Reversi IO ()

-- Tree 
data Tree a = Node a [Tree a]
            deriving (Eq, Show, Data, Typeable)

instance Functor Tree where
    f `fmap` (Node n children) = Node (f n) (map (fmap f) children)

nextPlayer Black = White
nextPlayer _     = Black

emptyGame :: Reversi
emptyGame = Reversi { board = M.empty, currentPlayer = Black }

initialGame = emptyGame {board = b}
    where b = M.fromList [((i,j), if (odd $ i + j) then Black else White) | i <- [0,1], j <- [0,1]]

liftBin :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
liftBin f (x1,y1) (x2,y2) = (x1 `f` x2, y1 `f` y2)

(<+>) :: Position -> Position -> Position
(<+>)  = liftBin (+)

(<->) :: Position -> Position -> Position
(<->)  = liftBin (-)

(<*>) :: Position -> Position -> Position
(<*>)  = liftBin (*)

nexts :: [Position]
nexts = [(i,j) | i <- l, j <- l, (i,j) /= (0,0)]
        where l = [-1..1]

rots = [(i,j) | i <- l, j <- l]
    where l = [-1,1]
play :: Position -> Reversi -> Reversi
play p (Reversi b cp) = Reversi newBoard (nextPlayer cp)
    where newBoard = foldr (\k b -> M.insert k cp b) b (p:getPositions)
          getPositions = do
            move <- nexts
            takePositons [] $ iterate (move <+>) (move <+> p)
              where takePositons accum (p:ps)  = 
                        case M.lookup p b of 
                          Just x | x == cp   -> accum
                                 | otherwise -> takePositons (p:accum) ps
                          Nothing -> []

-- removeSymMoves :: [[Position]] -> [[Position]]
-- removeSymMoves positions = 
--     do
--       rot <- rots
--       return $ map (\a -> first (<*> rot) (a,a)) positions

 -- nub $ map translate games 
 --     where translate (Reversi b cp) = Reversi (M.mapKeysMonotonic (<-> minXY) b) cp
 --               where minXY = fst $ M.findMax b
 --                     -- (minProj fst, minProj snd)
 --                     minProj f = minimum $ map f (M.keys b)


    -- nub $ map sort $ 
    --     do
    --       game <- games
    --       rot <- rots
    --       return $ map (rot<*>) game

    
moves :: Reversi -> [Reversi]
moves g@(Reversi b _) =  map (flip play g) moves
    where moves = 
              if M.keys b == [] then [(1,1)]
              else
                  nub $ 
                      do
                        pos <- M.keys b
                        inc <- nexts
                        let neighbor = (pos <+> inc)
                        Nothing <- return $ M.lookup neighbor b
                        return neighbor

gameTree :: Reversi -> Tree Reversi
gameTree g = Node g $ map gameTree $ moves g 

gameTree' n g | (M.size . board $ g) >= n =  Node g []
              | otherwise = Node g $ map (gameTree' n) $ moves g 

prun 0 (Node g _) = Node g []
prun (n+1) (Node g cs) = Node g $ map (prun n) cs

lastLevel n tree = [x | Node x [] <- universe $ prun n tree]
-- Just a meaningful alias
getIO = lift

static :: Reversi -> Int
static (Reversi b cp) = length $ filter (cp==) (M.elems b)

maximize :: (Ord a) => Tree a -> a
maximize (Node n []) = n
maximize (Node n cs) = maximum (map minimize cs)
minimize :: (Ord a) => Tree a -> a
minimize (Node n []) = n
minimize (Node n cs) = minimum (map maximize cs)


-- wfp matters version:
minLeq :: (Ord t) => [t] -> t -> Bool
minLeq [] pot = False 
minLeq (num: rest) pot = 
    if  num <= pot then True
    else minLeq rest pot

maxLeq :: (Ord t) => [t] -> t -> Bool
maxLeq [] pot = False 
maxLeq (num: rest) pot = 
    if  num >= pot then True
    else maxLeq rest pot

omitMin :: (Ord a) => a -> [[a]] -> [a]
omitMin pot [] = []
omitMin pot (nums:rest) = 
    if minLeq nums pot then omitMin pot rest
    else 
        let min = minimum nums
        in min : (omitMin min rest)

omitMax pot [] = []
omitMax pot (nums:rest) = 
    if maxLeq nums pot then omitMax pot rest
    else 
        let max = maximum nums
        in max : (omitMax max rest)

mapMin [] = []
mapMin (nums:rest) = 
    let min = minimum nums
    in min : (omitMin min rest)

mapMax [] = []
mapMax (nums:rest) = 
    let max = maximum nums
    in max : (omitMax max rest)

maximize' (Node n []) = n:[]
maximize' (Node n cs) = mapMin (map minimize' cs)
minimize' (Node n []) = n:[]
minimize' (Node n cs) = mapMax (map maximize' cs)

-- compareTree (Node n1 _) (Node n2 _)  = compare n1 n2
-- highFirst (Node n cs) = Node n (sortBy compareTree (map lowFirst cs))
-- lowFirst (Node n cs) = Node n (sortBy (flip compareTree) (map highFirst cs))

-- evaluateN n = maximize . fmap static . prun n . gameTree
gameSize = 12

evaluateN :: (Integral t) => t -> Reversi -> Int
evaluateN n = maximum . maximize' . fmap static . prun n . gameTree' gameSize
evaluate = evaluateN 2

-- Where n is the n-levels to explore.

minMaxPlayer n | even n  = strategy minimumBy
               | otherwise = strategy maximumBy
    where strategy f = snd . f (comparing fst) . map (evaluateN n &&& id) . moves 


alwaysFirstMove :: Reversi -> Reversi
alwaysFirstMove = head  . moves

alwaysLastMove = head . reverse . moves

-- main :: IO ()
-- main = liftM fst $ runStateT loop newGame
--     where loop = do
--             pos::Position <- getIO $
--                   readLn 
--                   `catch` (\_ -> return (0,0))
--             modify $ play pos
--             game <- get
--             getIO $ print game
--             loop
-- game =  foldl1 (.) $ take 10 $ cycle [minMaxPlayer, (minMaxPlayer 2)]

createGame :: Int -> (b -> b) -> (b -> b) -> [b -> b]
createGame n playerBlack playerWhite =  take n $ cycle [playerBlack, playerWhite]

game1 =  createGame gameSize alwaysLastMove   (minMaxPlayer 2 )
game2 =  createGame gameSize (minMaxPlayer 2) alwaysLastMove
game3 =  createGame gameSize alwaysFirstMove  (minMaxPlayer 2 )
game4 =  createGame gameSize (minMaxPlayer 2 ) alwaysFirstMove
game5 =  createGame gameSize (minMaxPlayer 2 ) (minMaxPlayer 2 )
game6 =  createGame gameSize (minMaxPlayer 2 ) (minMaxPlayer 0 )

showNumTokens color = map (length . filter (==color) . M.elems . board . ($initialGame)) . scanl1 (.)
showLastPosition = board . ($initialGame) . foldl1 (.) 
main =
    do
      let game = createGame (gameSize - 4) (minMaxPlayer 0) (minMaxPlayer 2)
      print $ showNumTokens Black $ game
      print $ showLastPosition game

      -- print $ showNumTokens Black game5

--           -- length nodes
--           --     where nodes = [() | Node _ _ <- universe $ prun 6 $ gameTree newGame]  

{-
  let g1 = newGame
  let g2 =  (play (1,1) newGame)
  let g3 =  (play (1,2) $ play (1,1) newGame)
  let g4 =  (play (1,3) $ play (1,2) $ play (1,1) newGame)
  let g5 =  (play (1,4) $ play (1,3) $ play (1,2) $play (1,1) newGame)

  sort $ lastLevel 0 $ (static `fmap` (gameTree g1))
  evaluateN 2 (g1)


map (length . (filter (==Black)) . M.elems . board . ($newGame)) $ game'

x0
0x

xxx
0x

x0
0x
  x

  x
x0
0x
  


-}