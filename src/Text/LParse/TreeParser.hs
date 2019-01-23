module Text.LParse.TreeParser (treeParser) where

import qualified Data.Map.Strict as M

import Control.Arrow
import Control.DoubleContinuations

import Text.LParse.Parser
import Text.LParse.Metaparser

noRest (r,_) = (r,undefined)

treeParser :: Parser r' String r -> Parser r' Integer r -> M.Map String (Parser r' [r] r) -> Parser r' AST r
treeParser fs fi ps = Parser $ \ast ->
    case ast of
        (SLeaf s) -> noRest <$> pFunc fs s
        (ILeaf i) -> noRest <$> pFunc fi i
        (Node s ns) | s `M.notMember` ps -> throw $ "Unknown Node type '" ++ s ++ "'"
                    | otherwise          -> do
            let p = ps M.! s
            cs <- mapM (pFunc (treeParser fs fi ps)) ns
            noRest <$> pFunc p (map fst cs)

combinationParser ::Parser r' String r -> Parser r' Integer r -> M.Map String (Parser r' [r] r) -> Parser r'' String (Parser r' String r) 
combinationParser fs fi ps = (>>> treeParser fs fi ps) <$> metaParser