import qualified Data.Map.Strict as Map
import Control.Monad.State

data Location = RegInt Reg
              | RegFloat Reg
              | Stack Offset
              | Heap Offset
              | Global String

type Reg = String
type Offset = Int
type Count = (Int, Int, Int, Table)
type Table = Map.Map String Location

newReg :: State Count Reg
newReg = do (reg, stackP, dataP, tbl) <- get
            put (reg+1, stackP, dataP, tbl)
            return (if reg < 10 then "t" ++ show reg else "s" ++ show (reg - 10))

newStackP :: Int -> State Count Int
newStackP n = do (reg, stackP, dataP, tbl) <- get
                 put (reg, stackP+n, dataP, tbl)
                 return stackP

