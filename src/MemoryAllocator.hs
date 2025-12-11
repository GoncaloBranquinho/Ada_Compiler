module MemoryAllocator where
import qualified Data.Map.Strict as Map
import Control.Monad.State

data Location = RegI Reg
              | RegF Reg
              | Stack Offset
              | Global String
              deriving (Show,Eq)

type Reg = String
type Offset = Int
type Count = (Int, Int, Int, Adresses, ScpInfo)
type Adresses = Map.Map String [Location]
type ScpInfo = Map.Map Int (Int,Int,Int)
type TableIR = [(Int, [(String,Int,Bool)])]

emptyMem :: Count
emptyMem = (0,2,0,Map.empty,Map.empty)

newRegI :: State Count Reg
newRegI = do (regI, regF, stackP, tbl, scpInfo) <- get
             put (regI+1, regF, stackP, tbl, scpInfo)
             return (if regI < 10 then "$t" ++ show regI else "$s" ++ show (regI - 10))


popRegI :: Int -> State Count ()
popRegI n = do (regI,regF,stackP,tbl,scpInfo) <- get
               put (regI-n,regF,stackP,tbl,scpInfo)

newRegF :: State Count Reg
newRegF = do (regI,regF,stackP,tbl,scpInfo) <- get
             put (regI, regF+1, stackP, tbl, scpInfo)
             return (if regF < 12 then "$f" ++ show regF else "$f" ++ show (regF + 4))


popRegF :: Int -> State Count ()
popRegF n = do (regI,regF,stackP,tbl,scpInfo) <- get
               put (regI,regF-n,stackP,tbl,scpInfo)


popStackP :: Int -> State Count ()
popStackP n = do (regI,regF,stackP,tbl,scpInfo) <- get
                 put (regI,regF,stackP-n,tbl,scpInfo)

deallocate :: Int -> State Count ()
deallocate scp = do (_,_,_,_,scpInfo) <- get
                    let (scpRegI,scpRegF,scpStackP) = scpInfo Map.! scp
                    popRegI scpRegI
                    popRegF scpRegF
                    popStackP scpStackP


allocate :: TableIR -> [Int] -> [String] -> [Float] -> State Count (Adresses,ScpInfo)
allocate [] [] strs flts = do allocateLits strs flts 0
                              (_,_,_,tbl,scpInfo) <- get
                              return (tbl,scpInfo)
allocate [] (scp:scps) xs ys = do deallocate scp
                                  allocate [] scps xs ys

allocate ((scp, content):remainder) (scp':nextScps) xs ys = do allocateScope scp content
                                                               if scp == scp'
                                                                 then deallocate scp
                                                                 else return ()
                                                               let scps = if scp == scp'
                                                                            then nextScps
                                                                            else scp':nextScps
                                                               allocate remainder scps xs ys

allocateLits :: [String] -> [Float] -> Int -> State Count ()
allocateLits [] [] _ = return ()
allocateLits [] (flt:flts) n = do (regI,regF,stackP,tbl,scpInfo) <- get
                                  let tbl' = Map.insertWith (++) (show flt) [Global ("flt" ++ show n)] tbl
                                  put (regI,regF,stackP,tbl',scpInfo)
                                  allocateLits [] flts (n+1)
allocateLits (str:strs) flts n = do (regI,regF,stackP,tbl,scpInfo) <- get
                                    let tbl' = Map.insertWith (++) str [Global ("str" ++ show n)] tbl
                                    put (regI,regF,stackP,tbl',scpInfo)
                                    allocateLits strs flts (n+1)

allocateScope :: Int -> [(String,Int,Bool)] -> State Count ()
allocateScope _ [] = return ()
allocateScope scp (idInfo:nextIds) = do allocateReg scp idInfo
                                        allocateScope scp nextIds


-- atualmente cada temporario tem um registro para int e outro para float, ou entao vai para a stack. Mas consigo modificar o codigo intermedio para verificar se um temporario vai alguma vez guardar floats
allocateReg :: Int -> (String,Int,Bool) -> State Count ()
allocateReg scp (id,bytes,isFloat) = do (regI,regF,stackP,tbl,scpInfo) <- get
                                        if (regI == 18 && (not isFloat || head id == '_')) ||
                                           (regF == 28 && (isFloat || head id == '_'))
                                           then allocateStack scp (id,bytes,isFloat)
                                           else do
                                             (newRegI', newRegF', tbl', scpInfo') <-
                                               if head id == '_'
                                                 then do
                                                    rI <- newRegI
                                                    rF <- newRegF
                                                    let tbl'' = Map.insertWith (++) id [RegI rI, RegF rF] tbl
                                                    sInfo <- updateScp scp [1,2] 1
                                                    return (regI+1, regF+1, tbl'', sInfo)
                                                  else if (not isFloat)
                                                  then do
                                                    rI <- newRegI
                                                    let tbl'' = Map.insertWith (++) id [RegI rI] tbl
                                                    sInfo <- updateScp scp [1] 1
                                                    return (regI+1, regF, tbl'', sInfo)
                                                  else do
                                                    rF <- newRegF
                                                    let tbl'' = Map.insertWith (++) id [RegF rF] tbl
                                                    sInfo <- updateScp scp [2] 1
                                                    return (regI, regF+1, tbl'', sInfo)
                                             put (newRegI', newRegF', stackP, tbl', scpInfo')


allocateStack :: Int -> (String,Int,Bool) -> State Count ()
allocateStack scp (id,bytes,isFloat) = do (regI,regF,stackP,tbl,scpInfo) <- get
                                          sInfo <- updateScp scp [3] bytes
                                          let tbl' = Map.insertWith (++) id [Stack (-stackP)] tbl
                                          put (regI,regF,stackP+bytes,tbl',sInfo)

updateScp :: Int -> [Int] -> Int -> State Count ScpInfo
updateScp scp [] _ = do (_,_,_,_,scpInfo) <- get
                        return scpInfo
updateScp scp (idx:idxs) bytes = do (regI,regF,stackP,tbl,scpInfo) <- get
                                    let scpInfo' = Map.alter
                                                        (\val ->
                                                            case val of
                                                              Nothing -> Just (updateIdx idx bytes (0,0,0))
                                                              Just val' -> Just (updateIdx idx bytes val')
                                                        ) scp scpInfo

                                    put (regI,regF,stackP,tbl,scpInfo')
                                    updateScp scp idxs bytes


updateIdx :: Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
updateIdx 1 bytes (x, y, z) = (x+bytes, y, z)
updateIdx 2 bytes (x, y, z) = (x, y+bytes, z)
updateIdx 3 bytes (x, y, z) = (x, y, z+bytes)



removeUnusedTemps :: TableIR -> [String] -> TableIR
removeUnusedTemps [] _ = []
removeUnusedTemps ((scope,info):xs) ys = (scope,removeUnusedTempsScope info ys):removeUnusedTemps xs ys

removeUnusedTempsScope :: [(String,Int,Bool)] -> [String] -> [(String,Int,Bool)]
removeUnusedTempsScope [] _ = []
removeUnusedTempsScope ((x,y,z):xs) ys = if elem x ys then (x,y,z):removeUnusedTempsScope xs ys else removeUnusedTempsScope xs ys
