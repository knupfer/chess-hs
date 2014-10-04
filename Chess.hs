{-# LANGUAGE UnicodeSyntax #-}

import System.Environment
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Applicative
import System.Console.ANSI
import qualified Data.Set as S
import qualified Data.Map as M
import Prelude.Unicode
import qualified Data.Set.Unicode as S
import qualified Data.Map.Unicode as M

type Move     = (Position, Position)
type Board    = M.Map Position Figure
type Player   = Color
data Figure   = Figure {getPiece∷Piece, getColor∷Color} deriving (Eq)
type Position = (Int, Int)
data Piece    = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq)

main ∷ IO ()
main = clearScreen >> nextTurn White startBoard

interaction ∷ Player → Board → IO ()
interaction pl fs = do
  let nextPl      = if pl ≡ Black then White else Black
  a ← getLine
  clearScreen
  either (\x → putStrLn (colorize Red x ++ "\n") >> nextTurn pl fs)
    (nextTurn nextPl) $ validateInput a >>= validateMove pl fs

nextTurn ∷ Player → Board → IO ()
nextTurn pl fs = do
  pc ← fmap ("-c" ∈) getArgs
  pc2 ← fmap ("-c2" ∈) getArgs
  when (threat fs pl) ∘  putStrLn $ colorize Yellow "Your King is threaten!\n"
  putStr $ prompt fs
  when (pc ∧ pc2) $ getLine >>= putStr
  clearScreen
  if (pc ∧ pl ≡ Black) ∨ pc2
    then computerMove [] pl fs
    else interaction pl fs

computerMove ∷ [Move] → Player → Board → IO ()
computerMove bad pl fs = do
  let nextPl           = if pl ≡ Black then White else Black
  let move             = bestMove bad pl fs
  if length bad > 50
    then putStrLn $ prompt fs
         ++ colorize Red ("\nThe " ++ show pl ++ " King was slayn!\n")
    else
    either (\_ → computerMove (move:bad) pl fs)
    (nextTurn nextPl) $validateMove pl fs move

bestMove ∷ [Move] → Player → Board → Move
bestMove bad pl fs = snd a
  where
    a = M.foldrWithKey
        (\k list acc@(s,_) →
          if fst (scoreFold k list) < s
          then (fst $ scoreFold k list, (k,snd $ scoreFold k list))
          else if getPiece (fromJust (M.lookup k fs)) ≡ Pawn
                  ∧ (k,head list) ∉ bad
               then (s,(k,head list))
               else acc)
        ((500,500),((1,2),(5,5))) g
    scoreFold k = foldr (\l acc@(s,_) →
                          (if (score k l < s) ∧ (k,l) ∉ bad
                           then (score k l, l)
                           else acc))
                  ((200,200),(1,2))
    g = M.filter (not ∘ null)
        ∘ M.mapWithKey (\k _ → S.toList $ possibleMoves k fs)
        $ M.filter ((≡) pl  ∘ getColor) fs
    score old new = getScore ∘ pawnMutation
                    ∘ M.mapKeys (\x → if x ≡ old then new else x)
                    $ M.delete new fs

colorize ∷ Color → String → String
colorize col s = setSGRCode [SetColor Foreground Vivid col] ++ s
                 ++ setSGRCode [Reset]

getScore ∷ Board → (Int,Int)
getScore =  M.foldr (\(Figure p c) (b,w) →
              (\m → if c ≡ White then (b+m,w) else (b,w+m))
              (case p of King   → 100
                         Queen  → 9
                         Rook   → 5
                         Knight → 3
                         Bishop → 3
                         Pawn   → 1))
            (-100,-100)

threat ∷ Board → Player → Bool
threat fs pl = M.foldr (\x y → kingPos ∈ x ∨ y) False
               (M.mapWithKey (\k _ → S.toList $ possibleMoves k fs)
                $ M.filter ((≢) pl ∘ getColor) fs)
  where kingPos = head ∘ M.keys $ M.filter (Figure King pl≡) fs

validateInput ∷ String → Either String Move
validateInput a
  | length g ≢ 2 =
      Left "You have to enter two coordinates like a2 b4."
  | any ((≢) 2 ∘ length) g =
      Left "The coordinates must be of length 2."
  | not $ all (\(x:y:"") → isAlpha x ∧ isNumber y) g =
      Left "Enter first a column, then a row."
  | not $ all (\(x:y:"") → x ∈ ['a'..'h'] ∧ y ∈ ['1'..'8']) g =
      Left "The column must be between a and h, the row between 1 and 8."
  | otherwise = Right ((col,row),(toCol,toRow))
  where g     = words a
        col   = (ord ∘ head ∘ head $ g) - ord 'a' + 1
        row   = read ∘ tail ∘ head $ g ∷ Int
        toCol = (ord ∘ head ∘ last $ g) - ord 'a' + 1
        toRow = read ∘ tail ∘ last $ g ∷ Int

validateMove ∷ Player → Board → Move → Either String Board
validateMove pl fs (oldPos,newPos)
  | oldPos M.∉ fs = Left "Your first coordinate isn't a piece."
  | pl ≢ (getColor ∘ fromJust $ M.lookup oldPos fs) = Left "Wrong Color."
  | newPos S.∈ (possibleMoves oldPos fs)
    ∧ not (threat newBoard pl) = Right newBoard
  | otherwise    = Left "This move is invalid."
  where newBoard = pawnMutation
                   ∘ M.mapKeys (\x → if x ≡ oldPos then newPos else x)
                   $ M.delete newPos fs

pawnMutation ∷ Board → Board
pawnMutation = M.mapWithKey (\(_,y) a@(Figure p c)
                             → if y ∈ [1,8] ∧ p ≡ Pawn
                                then Figure Queen c else a)

possibleMoves ∷ Position → Board → S.Set (Int,Int)
possibleMoves (x,y) fs = g $ case piece of
  King → [(x+dx,y+dy) | dx ← [-1..1], dy ← [-1..1]]
  Queen → bishop ++ rook
  Rook → rook
  Knight → [(x+dx,y+dy) | dx ← [-2..2], dy ← [-2..2], (≡2).abs $ dx*dy]
  Bishop → bishop
  Pawn → if color (x,y) ≡ Black then pawn (-) 7 else pawn (+) 2
  where
    g = S.fromList ∘ filter
          (\(fx,fy) → all (∈ [1..8]) [fx,fy] ∧ (fx,fy) ≢ (x,y)
                       ∧ ((fx,fy) M.∉ fs
                           ∨ color (fx,fy) ≢ color (x,y)))
    color pos = getColor ∘ fromJust $ M.lookup pos fs
    piece     = getPiece ∘ fromJust $ M.lookup (x,y) fs
    l dc dr   = (\s → s ++ [maximum s + 1] ++ [minimum s - 1]) $ 0 :
              concatMap (takeWhile (\d → (dc x d,dr y d) M.∉ fs))
              [[1..8],[-1,-2..(-8)]]
    pawn f r = [z | z ← [(x+1,f y 1),(x-1,f y 1)], z M.∈ fs] ++
               (if (x,f y 1) M.∉ fs
                then (x,f y 1):[(x,f y 2)| y ≡ r ∧ (x,f y 2) M.∉ fs]
                else [])
    bishop = [(x+dx,y+dx) | dx ← l (+) (+)]++[(x-dy,y+dy) | dy ← l (-) (+)]
    rook   = [(x+dx,y+dy) | dx ← l (+) const, dy ← l const (+)
                          , dy ≡ 0 ∨ dx ≡ 0]

startBoard ∷ Board
startBoard = M.fromList $ g 8 Black ++ p 7 Black ++ g 1 White ++ p 2 White
  where g row color = zipWith (\col piece → ((col, row), Figure piece color))
                      [1..8] [ Rook   , Knight
                             , Bishop , King
                             , Queen  , Bishop
                             , Knight , Rook]
        p row color = [ ((col,row), Figure Pawn color) | col ← [1..8] ]

charOfPiece ∷ Figure → String
charOfPiece (Figure p c) = head
 [if c ≡ Black then colorize Magenta b else w | (piece, b, w)
                        ← [ (King    , "K" , "k")
                           , (Queen  , "Q" , "q")
                           , (Rook   , "R" , "r")
                           , (Knight , "N" , "n")
                           , (Bishop , "B" , "b")
                           , (Pawn   , "P" , "p") ], p ≡ piece]

prompt ∷ Board → String
prompt fs = unlines ∘ map ("    "++) $
            [ "┏━━" ++ colorize Blue "abcdefgh" ++ "━━┓   Score "
              ++ colorize Magenta (show (max 0 ∘ fst ∘ getScore $ fs))
              ++ " " ++ show (max 0 ∘ snd ∘ getScore $ fs)
            , "┃            ┃"
            ] ++ (g <$> [8,7..1]) ++
            [ "┃            ┃"
            , "┗━━" ++ colorize Blue "abcdefgh" ++ "━━┛"
            , ""
            , " Enter a move"]
            where g y = (colorize Blue ∘ show) y ++ "  " ++ extractRow y fs
                        ++ "  " ++ (colorize Blue ∘ show) y

extractRow ∷ Int → Board → String
extractRow r fs = concat $ (\c → maybe (if even (c+r) then "░" else " ")
                                  charOfPiece $ M.lookup (c,r) fs) <$> [1..8]
