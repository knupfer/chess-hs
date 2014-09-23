import System.IO
import Data.Char
import Control.Applicative
import qualified System.Console.ANSI as Term
import qualified Data.Map            as M

type AllFigures = M.Map Position Figure
type Player     = Color
data Move       = Move | Attack      deriving (Eq, Ord)
data Figure     = Figure Piece Color deriving (Eq)
data Color      = Black | White      deriving (Eq, Show)
type Position   = (Int, Int)
data Piece      = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq)

main :: IO ()
main = Term.clearScreen >> interaction Black startBoard

interaction :: Player -> AllFigures -> IO ()
interaction player fs = do
  putStr $ prompt fs
  let nextPlayer = if player == Black then White else Black
  if win fs player
    then putStrLn $ "The " ++ show player ++ " King was slayn!"
    else do a <- getLine
            Term.clearScreen
            either (\x -> putStrLn x >> interaction player fs)
                   (interaction nextPlayer) $
                   validateInput a >>= validateMove player fs

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

validateMove :: Player -> AllFigures -> (Position, Position) -> Either String AllFigures
validateMove player fs (oldPos,newPos)
  | not (M.member oldPos fs) = Left "Your first coordinate isn't a piece."
  | player /= (\(Just (Figure _ c)) -> c) g = Left "Wrong Color."
  | validTarget  = newBoard
  | otherwise    = Left "This move is invalid."
  where g        = M.lookup oldPos fs
        newBoard = Right $ M.mapKeys (\x -> if x == oldPos then newPos else x) fs
--        newBoard = Right . M.delete oldPos . M.insert newPos
--                       ((\(Just x) -> x) g) $ fs
        validTarget  = newPos `elem` possibleMoves oldPos fs

possibleMoves :: Position -> AllFigures -> [Position]
possibleMoves (col,row) fs
  | piece == King   = g [(col+x,row+y) | x <- [-1..1], y <- [-1..1]]
  | piece == Queen  = bishop ++ rook
  | piece == Rook   = rook
  | piece == Knight = g
    [(col+x,row+y) | x <- [-2,-1,1,2], y <- [-2,-1,1,2], abs x /= abs y]
  | piece == Bishop = bishop
  | piece == Pawn = g $ case color of
     Black -> (if row == 7 && not (M.member (col,row-1) fs) && not (M.member (col,row-2) fs)
                   then [(col,row-1),(col,row-2)]
               else [(col,row-1) | not (M.member (col,row-1) fs)]) ++
       [x | x <- [(col+1,row-1),(col-1,row-1)],
                    (Just (Figure _ White)) <- [M.lookup x fs]]
     White -> (if row == 2 && not (M.member (col,row+1) fs) && not (M.member (col,row+2) fs)
                   then [(col,row+1),(col,row+2)]
               else [(col,row+1) | not (M.member (col,row+1) fs)]) ++
              [x | x <- [(col+1,row+1),(col-1,row+1)],
                   (Just (Figure _ Black)) <- [M.lookup x fs]]
  where
    g = filter $ \(x,y) -> all (`elem` [1..8]) [x,y] && (x,y) /= (col,row) && null ["1" | (Just (Figure _ c)) <- [M.lookup (x,y) fs], c == color]
    color = (\(Just (Figure _ c)) -> c) $ M.lookup (col,row) fs
    piece = (\(Just (Figure p _)) -> p) $ M.lookup (col,row) fs
    l dc dr = (\y -> concat [y ++ y ++ [maximum y + 1] ++ [minimum y - 1] | not $ null y]) .
              concatMap (takeWhile
                         (\x -> not $ M.member (dc col x,dr row x) fs)) $
              [[1..8],[-1,-2..(-8)]]
    bishop = g [(col+x,row+y) | x <- l (+) (+), y <- l (-) (+),
         abs y == abs x]
    rook = g [(col+x,row+y) | x <- 0:l (+) const, y <- 0:l const (+),
         (y == 0 && x /= 0) || (x == 0 && y /= 0)]

startBoard :: AllFigures
startBoard = M.fromList $ g 8 Black ++ p 7 Black ++ g 1 White ++ p 2 White
  where g row color = zipWith (\col piece -> ((col, row), Figure piece color))
                      [1..8] [ Rook   , Knight
                             , Bishop , King
                             , Queen  , Bishop
                             , Knight , Rook]
        p row color = [ ((col,row) , Figure Pawn color) | col <- [1..8] ]

charOfPiece :: Figure -> Char
charOfPiece (Figure p c) = (if c == Black then fst else snd) . head $
 [(b,w) | (piece, b, w) <- [ (King   , 'K' , 'k')
                           , (Queen  , 'Q' , 'q')
                           , (Rook   , 'R' , 'r')
                           , (Knight , 'N' , 'n')
                           , (Bishop , 'B' , 'b')
                           , (Pawn   , 'P' , 'p') ], p == piece]

prompt :: AllFigures -> String
prompt fs = unlines $ ["","   abcdefgh",""] ++ (g <$> [8,7..1])
            ++ ["","   abcdefgh","\nEnter your move:"]
            where g y = show y ++ "  " ++ extractRow y fs ++ "  " ++ show y

extractRow :: Int -> AllFigures -> String
extractRow r fs = (\c -> maybe '.' charOfPiece $ M.lookup (c,r) fs) <$> [1..8]
