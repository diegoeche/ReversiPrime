{-# LANGUAGE ViewPatterns, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Monoid
import Data.Generics.PlateData
import Data.Data
import Data.Typeable
import Control.Parallel.Strategies

--------------------------------------------------
-- Data Types
--------------------------------------------------

-- Board
data Token = Black | White
           deriving (Show, Eq, Data, Typeable)

type Position = (Integer, Integer)

data Reversi = Reversi {board :: M.Map Position Token,
                        currentPlayer :: Token}
             deriving (Show, Data, Typeable)

-- The type for the game interpreter.
type GameREPL = StateT Reversi IO ()

-- Tree 
data Tree a = Node a [Tree a]
            deriving (Eq, Show, Data, Typeable)

nextPlayer Black = White
nextPlayer _     = Black

newGame :: Reversi
newGame = Reversi { board = M.empty, currentPlayer = Black }

(<+>) :: Position -> Position -> Position
(x1,y1) <+> (x2,y2) = (x1 + x2, y1 + y2)

nexts :: [Position]
nexts = [(i,j) | i <- l, j <- l, (i,j) /= (0,0)]
        where l = [-1..1]

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

moves g@(Reversi b _) = map (flip play g) moves
    where moves = 
              if M.keys b == [] then [(1,1)]
              else
                  nub $ do
                    pos <- M.keys b
                    inc <- nexts
                    let neighbor = (pos <+> inc)
                    Nothing <- return $ M.lookup neighbor b
                    return neighbor

gameTree :: Reversi -> Tree Reversi
gameTree g = Node g $ map gameTree $ moves g 

prun 0 (Node g _) = Node g []
prun (n+1) (Node g cs) = Node g $ map (prun n) cs

-- Just a meaningful alias
getIO = lift

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
main = do
    print $ length (nodes `using` parList rwhnf)
       where nodes = [() | Node _ _ <- universe $ prun 6 $ gameTree newGame]  


-- -- a is the game.
-- class Player a where 
--     -- generate a game from a given game.
--     play  :: a -> a
--     -- generates the succesors from a game state.
--     games :: a -> [a]

