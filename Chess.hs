import Data.Char
import Control.Applicative
import System.Console.ANSI
import qualified Data.Map as M

type AllFigures = M.Map Position Figure
type Player     = Color
data Figure     = Figure Piece Color deriving (Eq)
type Position   = (Int, Int)
data Piece      = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq)

main :: IO ()
main = clearScreen >> interaction White startBoard

interaction :: Player -> AllFigures -> IO ()
interaction player fs = do
  putStr $ prompt fs
  let nextPlayer = if player == Black then White else Black
  if win fs player
    then putStrLn . colorize Red $ "\nThe " ++ show player
                                   ++ " King was slayn!\n"
    else do a <- getLine
            clearScreen
            either (\x -> putStrLn (colorize Red x++"\n")
                          >> interaction player fs)
                   (interaction nextPlayer) $
                   validateInput a >>= validateMove player fs

-- printPossibleMoves :: Player -> AllFigures -> IO ()
-- printPossibleMoves pl fs =
--   (putStrLn . M.showTree)
--   (M.filter (not . null) . M.mapWithKey (\k _ -> possibleMoves k fs)
--          $ M.filter (\(Figure _ c) -> c == pl) fs)

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

win :: AllFigures -> Player -> Bool
win fs pl
  | M.null $ M.filter (Figure King pl==) fs = True
  | otherwise = False

validateInput :: String -> Either String (Position, Position)
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

validateMove :: Player -> AllFigures -> (Position, Position)
                -> Either String AllFigures
validateMove player fs (oldPos,newPos)
  | not (M.member oldPos fs) = Left "Your first coordinate isn't a piece."
  | player /= (\(Just (Figure _ c)) -> c) g = Left "Wrong Color."
  | newPos `elem` possibleMoves oldPos fs   = newBoard
  | otherwise    = Left "This move is invalid."
  where g        = M.lookup oldPos fs
        newBoard = Right . pawnMutation
                         . M.mapKeys (\x -> if x == oldPos then newPos else x)
                         $ M.delete newPos fs

pawnMutation :: AllFigures -> AllFigures
pawnMutation = M.mapWithKey (\(_,y) a@(Figure p c)
                             -> if y `elem` [1,8] && p == Pawn
                                then Figure Queen c else a)

possibleMoves :: Position -> AllFigures -> [Position]
possibleMoves (x,y) fs
  | piece == King   = g [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1]]
  | piece == Queen  = bishop ++ rook
  | piece == Rook   = rook
  | piece == Knight = g
    [(x+dx,y+dy) | dx <- [-2,-1,1,2], dy <- [-2,-1,1,2], abs dx /= abs dy]
  | piece == Bishop = bishop
  | piece == Pawn   = g $ case color of
     Black -> pawn (-) 7
     White -> pawn (+) 2
  where
    g = filter $ \(fx,fy) -> all (`elem` [1..8]) [fx,fy] && (fx,fy)/=(x,y) &&
      null ["1" | (Just (Figure _ c)) <- [M.lookup (fx,fy) fs], c == color]
    color   = (\(Just (Figure _ c)) -> c) $ M.lookup (x,y) fs
    piece   = (\(Just (Figure p _)) -> p) $ M.lookup (x,y) fs
    l dc dr = (\s -> concat [s ++ s ++ [maximum s + 1] ++ [minimum s - 1]
                            | not $ null s]) .
              concatMap (takeWhile (\d -> not $ M.member (dc x d,dr y d) fs))
              $ [[1..8],[-1,-2..(-8)]]
    pawn f r = [p | p <- [(x+1,f y 1),(x-1,f y 1)], M.member p fs] ++
               (if not (M.member (x,f y 1) fs)
                then (x,f y 1):[(x,f y 2)|y == r && not(M.member(x,f y 2) fs)]
                else [])
    bishop = g [(x+dx,y+dy) | dx <- l (+) (+), dy <- l (-) (+),
                abs dy == abs dx]
    rook = g [(x+dx,y+dy) | dx <- 0:l (+) const, dy <- 0:l const (+),
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
