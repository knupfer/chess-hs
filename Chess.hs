import System.Environment
import Data.Char
import Control.Monad
import Control.Applicative
import System.Console.ANSI
import qualified Data.Set as S
import qualified Data.Map as M
import System.Random

type Move       = (Position, Position)
type AllFigures = M.Map Position Figure
type Player     = Color
data Figure     = Figure Piece Color deriving (Eq)
type Position   = (Int, Int)
data Piece      = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq)

main :: IO ()
main = clearScreen >> nextTurn White startBoard

interaction :: Player -> AllFigures -> IO ()
interaction player fs = do
  let nextPl          = if player == Black then White else Black
  a <- getLine
  clearScreen
  either (\x -> putStrLn (colorize Red x ++ "\n")
                >> nextTurn player fs)
    (nextTurn nextPl) $ validateInput a >>= validateMove player fs

nextTurn :: Player -> AllFigures -> IO ()
nextTurn pl fs = do
  pc  <- fmap ("-c" `elem`) getArgs
  pc2 <- fmap ("-c2" `elem`) getArgs
  when (threat fs pl) . putStrLn $ colorize Yellow "Your King is threaten!\n"
  putStr $ prompt fs
  when (pc && pc2)$ getLine >>= putStr
  clearScreen
  if win fs pl
    then putStrLn . colorize Red $ "\nThe " ++ show pl ++ " King was slayn!\n"
    else
    if (pc && pl == Black) || pc2
    then computerMove [] pl fs
    else interaction pl fs

computerMove :: [Move] -> Player -> AllFigures -> IO ()
computerMove bad player fs = do
  let nextPl               = if player == Black then White else Black
  let move = bestMove bad player fs
  if length bad > 50
    then putStrLn $ prompt fs
         ++ colorize Red ("\nThe " ++ show player ++ " King was slayn!\n")
    else
    either (\_ -> computerMove (move:bad) player fs)
    (nextTurn nextPl) $validateMove player fs move

bestMove :: [Move] -> Player -> AllFigures -> Move
bestMove bad pl fs = snd a
  where
    a = M.foldrWithKey
        (\k list acc@(s,_) ->
          if fst (scoreFold k list) < s
          then (fst $ scoreFold k list,(k,snd $ scoreFold k list))
          else if snd (distanceFold k list) /= (0,0) && rand 10 > 4
               then (s,(k,snd (distanceFold k list))) else acc)
        ((500,500),((1,2),(5,5))) g
    rand r = fst (randomR (0,r-1)
                  (mkStdGen (length bad * M.size fs * r
                             * sum [x*y| (x,y) <- M.keys fs])))
    distanceFold k = foldr (\(lx,ly) acc@(s,_) ->
                             if (lx-fst kingPos)+(ly-snd kingPos)<(s+rand 10)
                                && notElem (k,(lx,ly)) bad
                             then ((lx-fst kingPos)+(ly-snd kingPos), (lx,ly))
                             else acc) (64,(0,0))
    kingPos = head . M.keys $ M.filter (Figure King (if pl == White
                                       then Black
                                       else White)==) fs
    scoreFold k = foldr (\l acc@(s,_) ->
                          (if ((score k l < s) && notElem (k,l) bad) ||
                              ((score k l == s) && notElem (k,l) bad &&
                               rand 10 > 4) then (score k l, l) else
                             acc)) ((200,200),(1,2))
    g = M.filter (not . null)
        . M.mapWithKey (\k _ -> S.toList $ possibleMoves k fs)
        $ M.filter (\(Figure _ c) -> c == pl) fs
    score old new = getScore . pawnMutation
                   . M.mapKeys (\x -> if x == old then new else x)
                   $ M.delete new fs

colorize :: Color -> String -> String
colorize col s = setSGRCode [SetColor Foreground Vivid col] ++ s
                 ++ setSGRCode [Reset]

getScore :: AllFigures -> (Int,Int)
getScore =  M.foldr (\(Figure p c) (b,w) ->
              (\m -> if c == White then (b+m,w) else (b,w+m))
              (case p of King -> 100
                         Queen -> 9
                         Rook -> 5
                         Knight -> 3
                         Bishop -> 3
                         Pawn -> 1))
            (-100,-100)

threat :: AllFigures -> Player -> Bool
threat fs pl = M.foldr (\x y -> kingPos `elem` x || y) False
               (M.mapWithKey (\k _ -> S.toList $ possibleMoves k fs)
                $ M.filter (\(Figure _ c) -> c /= pl) fs)
  where kingPos = fst . head . M.toList $ M.filter (Figure King pl==) fs

win :: AllFigures -> Player -> Bool
win fs pl
  | M.null $ M.filter (Figure King pl==) fs = True
  | otherwise = False

validateInput :: String -> Either String Move
validateInput a
  | length g /= 2 =
      Left "You have to enter two coordinates like a2 b4."
  | any (\x -> length x /= 2) g =
      Left "The coordinates must be of length 2."
  | not (all (\(x:y:"") -> isAlpha x && isNumber y) g) =
      Left "Enter first a column, then a row."
  | not (all (\(x:y:"") -> x `elem` ['a'..'h'] && y `elem` ['1'..'8']) g) =
      Left "The column must be between a and h, the row between 1 and 8."
  | otherwise = Right ((col,row),(toCol,toRow))
  where g     = words a
        col   = (ord . head . head $ g) - ord 'a' + 1
        row   = read . tail . head $ g :: Int
        toCol = (ord . head . last $ g) - ord 'a' + 1
        toRow = read . tail . last $ g :: Int

validateMove :: Player -> AllFigures -> Move -> Either String AllFigures
validateMove player fs (oldPos,newPos)
  | not (M.member oldPos fs) = Left "Your first coordinate isn't a piece."
  | player /= (\(Just (Figure _ c)) -> c) g   = Left "Wrong Color."
  | S.member newPos (possibleMoves oldPos fs)
    && not (threat newBoard player) = Right newBoard
  | otherwise    = Left "This move is invalid."
  where g        = M.lookup oldPos fs
        newBoard = pawnMutation
                   . M.mapKeys (\x -> if x == oldPos then newPos else x)
                   $ M.delete newPos fs

pawnMutation :: AllFigures -> AllFigures
pawnMutation = M.mapWithKey (\(_,y) a@(Figure p c)
                             -> if y `elem` [1,8] && p == Pawn
                                then Figure Queen c else a)

possibleMoves :: Position -> AllFigures -> S.Set (Int,Int)
possibleMoves (x,y) fs
  | piece == King   = g [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1]]
  | piece == Queen  = g $ bishop ++ rook
  | piece == Rook   = g rook
  | piece == Knight = g
    [(x+dx,y+dy) | dx <- [-2,-1,1,2], dy <- [-2,-1,1,2], abs dx /= abs dy]
  | piece == Bishop = g bishop
  | piece == Pawn = g $ if color == Black then pawn (-) 7 else pawn (+) 2
  where
    g p = S.fromList $ filter
          (\(fx,fy) -> all (`elem` [1..8]) [fx,fy] && (fx,fy)/=(x,y) &&
      null ["1" | (Just (Figure _ c)) <- [M.lookup (fx,fy) fs], c == color]) p
    color   = (\(Just (Figure _ c)) -> c) $ M.lookup (x,y) fs
    piece   = (\(Just (Figure p _)) -> p) $ M.lookup (x,y) fs
    l dc dr = (\s -> s ++ s ++ [maximum s + 1] ++ [minimum s - 1]) $ 0 :
              concatMap (takeWhile (\d -> not $ M.member (dc x d,dr y d) fs))
              [[1..8],[-1,-2..(-8)]]
    pawn f r = [p | p <- [(x+1,f y 1),(x-1,f y 1)], M.member p fs] ++
               (if not (M.member (x,f y 1) fs)
                then (x,f y 1):[(x,f y 2)|y == r && not(M.member(x,f y 2) fs)]
                else [])
    bishop = [(x+dx,y+dx) | dx <- l (+) (+)]++[(x-dy,y+dy) | dy <- l (-) (+)]
    rook   = [(x+dx,y+dy) | dx <- l (+) const, dy <- l const (+),
              dy == 0 || dx == 0]

startBoard :: AllFigures
startBoard = M.fromList $ g 8 Black ++ p 7 Black ++ g 1 White ++ p 2 White
  where g row color = zipWith (\col piece -> ((col, row), Figure piece color))
                      [1..8] [ Rook   , Knight
                             , Bishop , King
                             , Queen  , Bishop
                             , Knight , Rook]
        p row color = [ ((col,row) , Figure Pawn color) | col <- [1..8] ]

charOfPiece :: Figure -> String
charOfPiece (Figure p c) = ((if c == Black
                             then colorize Magenta . fst else snd) . head $
 [(b,w) | (piece, b, w) <- [ (King   , "K" , "k")
                           , (Queen  , "Q" , "q")
                           , (Rook   , "R" , "r")
                           , (Knight , "N" , "n")
                           , (Bishop , "B" , "b")
                           , (Pawn   , "P" , "p") ], p == piece])
                           ++ setSGRCode [Reset]

prompt :: AllFigures -> String
prompt fs = unlines . map ("    "++) $
            [ "┏━━" ++ colorize Blue "abcdefgh" ++ "━━┓   Score "
              ++ colorize Magenta (show (max 0 . fst . getScore $ fs))
              ++ " " ++ show (max 0 . snd . getScore $ fs)
            , "┃            ┃"
            ] ++ (g <$> [8,7..1]) ++
            [ "┃            ┃"
            , "┗━━" ++ colorize Blue "abcdefgh" ++ "━━┛"
            , ""
            , " Enter a move"]
            where g y = (colorize Blue . show) y ++ "  " ++ extractRow y fs
                        ++ "  " ++ (colorize Blue . show) y

extractRow :: Int -> AllFigures -> String
extractRow r fs = concat $ (\c -> maybe (if even (c+r) then "░" else " ")
                                  charOfPiece $ M.lookup (c,r) fs)
                  <$> [1..8]
