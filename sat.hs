-- | SAT - solver
-- | find out if a set of prop. logical sentences is satisfiable (that is: is there a assignment of truth-values such that all sentences are true?)
-- | the algorithm used to search for solutions tries to find these by backtracking and propagation of known facts
-- | step by step the system of sentences are simplified (by choosing truth-assignments for open variables and propagating the effects)
-- | till either there is a known FALSE fact in the system (in this case backtracking happens and on the top level there is no solution)
-- | or all facts in the system are TRUE in this case the found truth-assignment is returned
-- | On top of this sits a simple expression parser that can handle simple strings like this:
-- | * variables starts with a letter followed by letters or numbers
-- | * not         = '~'
-- | * and         = '&'
-- | * or          = '|'
-- | * implication = '=>'
-- | * equivalence = '=='
-- | precedence is from top to bottom in this list, sub-sentences can be grouped with parentheses '(..)''
-- | Example: 'p => q | ~r'
-- | you can solve such a string with a friendly message using satTest
-- | the main will ask you for it
module Main 
    ( main
    , satTest
    , trySatisfy
    , evaluate
    ) where

import Control.Applicative ((<$>))
import Data.List(intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)

import SatDefs
import SatParser

-- * EXPORTED

-- | main-program - ask the user for a system and tries to solve it
main :: IO()
main = do
    putStrLn ".. SAT - tester .."
    putStrLn "please input your system of sentences (separeted by ';'): "
    input <- getLine
    let output = satTest input
    putStrLn output

-- | tries to parse and solve a system - output is an parse-error-message, or a message telling you if it's satisfiable (and if so the first truth-assignment found)
satTest :: String -> String
satTest input = case parseSystem input of
                    Left err         -> "Error while parsing\n" ++ err
                    Right ans        -> "the system [" ++ input ++ (prettyPrint . trySatisfy $ ans)
  where prettyPrint Nothing   = "] is NOT satisfiable" 
        prettyPrint (Just ta) = "] is satisfied by " ++ (intercalate ", " . map showAssoc $ M.assocs ta)
        showAssoc (v, b)      = v ++ "=" ++ show b

-- | tries to satisfy a given system - returns 'Nothing' or 'Just' the found truth-assignment
trySatisfy :: System -> Maybe TruthAssignment
trySatisfy = trySatisfyWith M.empty

-- | evaluates a Sentence given a truth-assignment
evaluate :: TruthAssignment -> Sentence -> Maybe Bool
evaluate _ TrueS        = Just True
evaluate _ FalseS       = Just False
evaluate ta (VarS v)    = M.lookup v ta
evaluate ta (NotS s)    = not <$> evaluate ta s
evaluate ta (AndS a b)  = evaluate ta a <&> evaluate ta b
evaluate ta (OrS a b)   = evaluate ta a <|> evaluate ta b
evaluate ta (ImplS a b) = (not <$> evaluate ta a) <|> evaluate ta b
evaluate ta (EqS a b)   = evaluate ta a <=> evaluate ta b

-- * helpers

-- | tries to compute a assignment that satisfies the given System (based on a current truth-assignment) recursively
trySatisfyWith :: TruthAssignment -> System -> Maybe TruthAssignment
trySatisfyWith ta sys
    | null sys'         = Just ta
    | not isSatisfiable = Nothing
    | isJust prop       = let (v, b) = fromJust prop in
                          trySatisfyWith (add v b) sys'
    | otherwise         = case firstFreeVar sys' of
                          Nothing -> error "found no variable to try"
                          Just v  -> trySatisfyWith (add v True) sys' ?> trySatisfyWith (add v False) sys'
    where sys'          = simplifySystem ta sys
          isSatisfiable = FalseS `notElem` sys'
          prop          = firstUnit sys'
          add v b       = M.insert v b ta

-- | simplifies a sentence-system using the given truth-assoziation and removing TRUE-sentences (see 'simplify')
simplifySystem :: TruthAssignment -> System -> System
simplifySystem ta = filter (/= TrueS) . map (simplify ta)

-- | simplifies a Sentence using the given truth-assoziations and a set of simple rules like "TRUE | a == TRUE"
simplify :: TruthAssignment -> Sentence -> Sentence
simplify _  TrueS       = TrueS
simplify _  FalseS      = FalseS
simplify ta s@(VarS v)  = maybe s fromBool $ M.lookup v ta
simplify ta (NotS s)    = case simplify ta s of
                            FalseS                   -> TrueS
                            TrueS                    -> FalseS
                            NotS s'                  -> s'
                            s'                       -> NotS s'
simplify ta (AndS a b)  = case (simplify ta a, simplify ta b) of
                            (FalseS, _)              -> FalseS
                            (_, FalseS)              -> FalseS
                            (TrueS, b')              -> b'
                            (a', TrueS)              -> a'
                            (a', b') | a' == b'      -> a'
                                     | NotS a' == b' -> FalseS
                                     | a' == NotS b' -> FalseS
                                     | otherwise     -> AndS a' b'
simplify ta (OrS a b)   = case (simplify ta a, simplify ta b) of
                            (TrueS, _)               -> TrueS
                            (_, TrueS)               -> TrueS
                            (FalseS, b')             -> b'
                            (a', FalseS)             -> a'
                            (a', b') | a' == b'      -> a'
                                     | NotS a' == b' -> TrueS
                                     | a' == NotS b' -> TrueS
                                     | otherwise     -> OrS a' b'
simplify ta (ImplS a b) = case (simplify ta a, simplify ta b) of
                            (FalseS, _)              -> TrueS
                            (a', TrueS)              -> a'
                            (a', FalseS)             -> simplify ta (NotS a')
                            (TrueS, b')              -> b'
                            (a', b')                 -> ImplS a' b'
simplify ta (EqS a b)   = case (simplify ta a, simplify ta b) of
                            (TrueS, b')              -> b'
                            (FalseS, b')             -> simplify ta (NotS b')
                            (a', TrueS)              -> a'
                            (a', FalseS)             -> simplify ta (NotS a')
                            (a', b')                 -> EqS a' b'

-- | finds the first not assoziated variable in a system
firstFreeVar :: System -> Maybe Variable
firstFreeVar []               = Nothing
firstFreeVar (TrueS :sys)     = firstFreeVar sys
firstFreeVar (FalseS :sys)    = firstFreeVar sys
firstFreeVar (VarS v :_)      = Just v
firstFreeVar (NotS s :sys)    = firstFreeVar [s] ?> firstFreeVar sys
firstFreeVar (AndS a b :sys)  = firstFreeVar [a] ?> firstFreeVar [b] ?> firstFreeVar sys
firstFreeVar (OrS a b :sys)   = firstFreeVar [a] ?> firstFreeVar [b] ?> firstFreeVar sys
firstFreeVar (ImplS a b :sys) = firstFreeVar [a] ?> firstFreeVar [b] ?> firstFreeVar sys
firstFreeVar (EqS a b :sys)   = firstFreeVar [a] ?> firstFreeVar [b] ?> firstFreeVar sys

-- | finds the first Unit (either a Variable or ~Variable) to propagate a direct assoziation
firstUnit :: System -> Maybe (Variable, Bool)
firstUnit []                  = Nothing
firstUnit (VarS v :_)         = Just (v, True)
firstUnit (NotS (VarS v) : _) = Just (v, False)
firstUnit (_ : sys)           = firstUnit sys

fromBool :: Bool -> Sentence
fromBool True = TrueS
fromBool False = FalseS

-- * custom operators

(<&>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
Just False <&> _       = Just False
Just True <&> a        = a
Nothing <&> Just False = Just False
Nothing <&> _          = Nothing

(<|>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
Just True <|> _        = Just True
Just False <|> a       = a
Nothing <|> Just True  = Just True
Nothing <|> _          = Nothing

(<=>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
Just a <=> Just b = Just (a == b)
_ <=> _           = Nothing

(?>) :: Maybe a -> Maybe a -> Maybe a
a@(Just _) ?> _ = a
Nothing ?> a    = a