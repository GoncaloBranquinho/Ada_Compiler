module LivenessAnalysis where

import Lexer
import Parser
import SymbolTable
import IR

import Data.List
import Control.Monad.State
import Control.Monad (when)


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type SuccLA    = Map.Map Int (Set.Set Int)
type GenLA     = Map.Map Int (Set.Set String)
type KillLA    = Map.Map Int (Set.Set String)
type InLA      = Map.Map Int (Set.Set String)
type OutLA     = Map.Map Int (Set.Set String)
type CounterLA = Int
type MapLabelToInstrLA = Map.Map String Int

type InfoLA = (SuccLA, GenLA, KillLA, InLA, OutLA, CounterLA, MapLabelToInstrLA, [Instr])

emptyLA :: InfoLA
emptyLA = (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, 1, Map.empty, [])

prepareLA :: [Instr] -> State InfoLA InfoLA
prepareLA [] = get >>= \t0 -> return t0
prepareLA (x:xs) = do (s, g, k, i, o, c, m, instr) <- get
                      case x of
                             JUMP l             -> put (Map.insert c (Set.insert (Map.findWithDefault (-1) l m) (Map.findWithDefault (Set.empty) c s)) s, g, k, i, o, c + 1, Map.insert l c m, instr)
                             COND _ t1 t2 l1 l2 -> put (Map.insert c (Set.insert (Map.findWithDefault (-1) l2 m) (Map.findWithDefault (Set.empty) c s)) (Map.insert c (Set.insert (Map.findWithDefault (-1) l1 m) (Map.findWithDefault (Set.empty) c s)) s), Map.insert c (Set.insert t1 (Set.insert t2 (Map.findWithDefault (Set.empty) c g))) g, k, i, o, c + 1, Map.insert l2 c (Map.insert l1 c m), instr)
                             LABEL l            -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) (Map.insert (Map.findWithDefault (-1) l m) (Set.insert c (Map.findWithDefault (Set.empty) (Map.findWithDefault (-1) l m) s)) s)) else (Map.insert (Map.findWithDefault (-1) l m) (Set.insert c (Map.findWithDefault (Set.empty) (Map.findWithDefault (-1) l m) s)) s), g, k, i, o, c + 1, Map.insert l c m, instr)
                             MOVE _ t1 t2       -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s) else s, Map.insert c (Set.insert t2 (Map.findWithDefault (Set.empty) c g)) g, Map.insert c (Set.insert t1 (Map.findWithDefault (Set.empty) c k)) k, i, o, c + 1, m, instr)
                             MOVEI t1 _         -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s) else s, g, Map.insert c (Set.insert t1 (Map.findWithDefault (Set.empty) c k)) k, i, o, c + 1, m, instr)
                             OP _ t1 t2 t3      -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s) else s, Map.insert c (Set.insert t3 (Set.insert t2 (Map.findWithDefault (Set.empty) c g))) g, Map.insert c (Set.insert t1 (Map.findWithDefault (Set.empty) c k)) k, i, o, c + 1, m, instr)
                             PRINT t1           -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s) else s, Map.insert c (Set.insert t1 (Map.findWithDefault (Set.empty) c g)) g, k, i, o, c + 1, m, instr)
                             READ t1 t2         -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s) else s, g, Map.insert c (Set.insert t1 (Set.insert t2 (Map.findWithDefault (Set.empty) c k))) k, i, o, c + 1, m, instr)
                             IR.TOSTR _ t1 t2   -> put (if (xs /= []) then (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s) else s, Map.insert c (Set.insert t2 (Map.findWithDefault (Set.empty) c g)) g, Map.insert c (Set.insert t1 (Map.findWithDefault (Set.empty) c k)) k, i, o, c + 1, m, instr)
                             _                  -> if (xs /= []) then (put (Map.insert c (Set.insert (c + 1) (Map.findWithDefault (Set.empty) c s)) s, g, k, i, o, c + 1, m, instr)) else (put (s, g, k, i, o, c + 1, m, instr))
                      newState <- prepareLA xs
                      return newState


buildOutLA :: Int -> State InfoLA InfoLA
buildOutLA 0 = get >>= \t0 -> return t0
buildOutLA n = do
    (s, g, k, i, o, c, m, instr) <- buildInLA n

    let s' = Map.findWithDefault Set.empty n s
        newOut = Set.unions (map (\x -> Map.findWithDefault Set.empty x i) (Set.toList s'))
    when (newOut /= Map.findWithDefault Set.empty n o) $ put (s, g, k, i, Map.insert n newOut o, c, m, instr)
    newState <- get
    return newState

buildInLA :: Int -> State InfoLA InfoLA
buildInLA 0 = get >>= \t0 -> return t0
buildInLA n = do (s, g, k, i, o, c, m, instr) <- buildOutLA (n - 1)
                 let g'  = Map.findWithDefault Set.empty n g
                     o'  = Map.findWithDefault Set.empty n o
                     k' = Map.findWithDefault Set.empty n k
                     newIn = Set.union g' (Set.difference o' k')
                 when (newIn /= Map.findWithDefault Set.empty n i) $ put (s, g, k, Map.insert n newIn i, o, c, m, instr)
                 newState <- get
                 return newState


iterateLA :: State InfoLA InfoLA
iterateLA = do (s, g, k, i, o, c, m, instr) <- get
               (s', g', k', i', o', c', m', instr') <- buildOutLA (c - 1)
               finalState <- if (i /= i' || o /= o') then iterateLA else return (s', g', k', i', o', c', m', instr')
               return finalState


callDeadCodeElim :: Int -> [Instr] -> State InfoLA InfoLA
callDeadCodeElim n [] = get >>= \t0 -> return t0
callDeadCodeElim n xs = do put (emptyLA)
                           prepareLA xs
                           iterateLA
                           (s', g', k', i', o', c', m', instr') <- deadCodeElim n xs
                           put (s', g', k', i', o', c', m', [])
                           finalState <- if ((length xs) > (length instr')) then (callDeadCodeElim n instr') else return (s', g', k', i', o', c', m', instr')
                           return finalState

deadCodeElim :: Int -> [Instr] -> State InfoLA InfoLA
deadCodeElim n [] = get >>= \t0 -> return t0
deadCodeElim n (x:xs) = do (s, g, k, i, o, c, m, instr) <- get
                           case x of
                                  MOVE _ _ _  -> if (Set.null $ Set.intersection (Map.findWithDefault Set.empty n k) (Map.findWithDefault Set.empty n o)) then (put (s, Map.delete n g, Map.delete n k, Map.delete n i, Map.delete n o, c, m, instr)) else (put (s, g, k, i, o, c, m, x:instr))
                                  MOVEI _ _   -> if (Set.null $ Set.intersection (Map.findWithDefault Set.empty n k) (Map.findWithDefault Set.empty n o)) then (put (s, Map.delete n g, Map.delete n k, Map.delete n i, Map.delete n o, c, m, instr)) else (put (s, g, k, i, o, c, m, x:instr))
                                  OP _ _ _ _  -> if (Set.null $ Set.intersection (Map.findWithDefault Set.empty n k) (Map.findWithDefault Set.empty n o)) then (put (s, Map.delete n g, Map.delete n k, Map.delete n i, Map.delete n o, c, m, instr)) else (put (s, g, k, i, o, c, m, x:instr))
                                  --DECL _ _    -> if (Set.null $ Set.intersection (Map.findWithDefault Set.empty n k) (Map.findWithDefault Set.empty n o)) then (put (s, g, k, i, o, c, m, instr)) else (put (s, g, k, i, o, c, m, x:instr))
                                  _           -> (put (s, g, k, i, o, c, m, x:instr))
                           (_, _, _, _, _, _, _, instr') <- get
                           finalState <- if (n < c - 1) then deadCodeElim (n + 1) xs else (return (s, g, k, i, o, c, m, reverse instr'))
                           return finalState


{-



type PredFA    = Map.Map Int (Set.Set Int)
type GenFA     = Map.Map Int (Set.Set String)
type KillFA    = Map.Map Int (Set.Set String)
type InFA      = Map.Map Int (Set.Set String)
type OutFA     = Map.Map Int (Set.Set String)
type CounterFA = Int
type MapLabelToInstrFA = Map.Map String Int

type InfoFA = (PredFA, GenFA, KillFA, InFA, OutFA, CounterFA, MapLabelToInstrFA, [Instr])


emptyFA :: InfoFA
emptyFA = (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, 1, Map.empty, [])



prepareFA :: [Instr] -> State InfoFA InfoFA
prepareFA [] = get >>= \t0 -> return t0
prepareFA (x:xs) = do (s, g, k, i, o, c, m, instr) <- get
                      case x of
                             JUMP l             -> ()
                             COND _ t1 t2 l1 l2 -> (s, g, k, i, o, c, m, instr)
                             LABEL l            -> (s, g, k, i, o, c, m, instr)
                             MOVE _ t1 t2       -> (s, g, k, i, o, c, m, instr)
                             MOVEI t1 _         -> (s, g, k, i, o, c, m, instr)
                             OP _ t1 t2 t3      -> (s, g, k, i, o, c, m, instr)
                             PRINT t1           -> (s, g, k, i, o, c, m, instr)
                             READ t1 t2         -> (s, g, k, i, o, c, m, instr)
                             IR.TOSTR _ t1 t2   -> (s, g, k, i, o, c, m, instr)
                             _                  -> (s, g, k, i, o, c, m, instr)

                      newState <- prepareLA xs
                      return newState


buildInFA :: Int -> State InfoFA InfoFA
buildInFA 0 = get >>= \t0 -> return t0
buildInFA n = do
    (p, g, k, i, o, c, m, instr) <- buildOutFA n
    let p' = Map.findWithDefault Set.empty n p
        newIn = intersectionAll (map (\x -> if (x /= 1) then (Map.findWithDefault Set.empty x o) else (Set.empty)) (Set.toList p'))
    when (newIn /= Map.findWithDefault Set.empty n i) $ put (p, g, k, Map.insert n newIn i, o, c, m, instr)
    newState <- get
    return newState

buildOutFA :: Int -> State InfoFA InfoFA
buildOutFA 0 = get >>= \t0 -> return t0
buildOutFA n = do
    (p, g, k, i, o, c, m, instr) <- buildInFA (n - 1)
    let g'  = Map.findWithDefault Set.empty n g
        i'  = Map.findWithDefault Set.empty n i
        k'  = Map.findWithDefault Set.empty n k
        newOut = Set.union g' (Set.difference i' k')
    when (newOut /= Map.findWithDefault Set.empty n o) $ put (p, g, k, i, Map.insert n newOut o, c, m, instr)
    newState <- get
    return newState

iterateFA :: State InfoFA InfoFA
iterateFA = do (p, g, k, i, o, c, m, instr) <- get
               (p', g', k', i', o', c', m', instr') <- buildOutLA (c - 1)
               finalState <- if (i /= i' || o /= o') then iterateLA else return (p', g', k', i', o', c', m', instr')
               return finalState


availableAssignments :: Int -> [Instr] -> State InfoLA InfoLA
availableAssignments n [] = get >>= \t0 -> return t0
availableAssignments n (x:xs) = do (s, g, k, i, o, c, m, instr) <- get
                                   case x of
                                          OP op t1 t2 t3 -> let temp = (concat $ map (\x1 -> case ((instr !! x1) x1) of OP op' t1' t2' t3' -> (if (((show op') ++ t2' ++ t3') == ((show op) ++ t2 ++ t3)) then ([(MOVE (val op) t1 t3)]) else []); _ -> []) (Set.toList (Map.findWithDefault (Set.empty) n i))) in (put (s, g, k, i, o, c, m, (if (temp == []) then [x] else ([head temp])) ++ instr))
                                          _              -> (put (s, g, k, i, o, c, m, x:instr))
                                   (_, _, _, _, _, _, _, instr') <- get
                                   finalState <- if (n < c - 1) then availableAssignments (n + 1) xs else (return (s, g, k, i, o, c, m, reverse instr'))
                                   return finalState

intersectionAll :: [Set.Set String] -> Set.Set String
intersectionAll = foldr1 Set.intersection

-}
