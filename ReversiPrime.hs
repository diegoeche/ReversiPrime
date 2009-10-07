{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
import qualified Data.Map as M
import Control.Monad.State

--------------------------------------------------
-- Data Types
--------------------------------------------------

data Token = Black | White
           deriving Show

type Position = (Integer, Integer)
data Reversi = Reversi {board :: M.Map Position Token,
                        currentPlayer :: Token}
             deriving Show

-- The type for the game interpreter.
type GameREPL = StateT Reversi IO ()

nextPlayer Black = White
nextPlayer _     = Black

newGame :: Reversi
newGame = Reversi { board = M.empty, currentPlayer = Black }

play :: Position -> Reversi -> Reversi
play p (Reversi b cp) = Reversi (M.insert p cp b) next 
    where next = nextPlayer cp

-- Just a meaningful alias
getIO = lift

main :: IO ()
main = liftM fst $ runStateT loop newGame
    where loop = do
            pos::Position <- getIO $
                  readLn 
                  `catch` (\_ -> return (0,0))
            modify $ play pos
            game <- get
            getIO $ print game
            loop
            
               
             

-- -- a is the game.
-- class Player a where 
--     -- generate a game from a given game.
--     play  :: a -> a
--     -- generates the succesors from a game state.
--     games :: a -> [a]

