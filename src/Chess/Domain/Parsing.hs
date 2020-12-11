module Chess.Domain.Parsing where

------------------------------------------------------------------------------------------------------------------------
import Control.Monad (void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
------------------------------------------------------------------------------------------------------------------------
import Chess.Domain.Elements as E
------------------------------------------------------------------------------------------------------------------------

type Parser = Parsec String String
type ParseErrors = ParseErrorBundle String String

rank :: Parser Rank
rank = do
  c <- oneOf ['1', '2', '3', '4', '5', '6', '7', '8']
  let 
    r = case c of
      '1' -> One
      '2' -> Two
      '3' -> Three
      '4' -> Four
      '5' -> Five
      '6' -> Six
      '7' -> Seven
      '8' -> Eight
  return r

file :: Parser File
file = do
  c <- oneOf ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
  let 
    f = case c of
      'a' -> A
      'b' -> B
      'c' -> C
      'd' -> D
      'e' -> E
      'f' -> F
      'g' -> G
      'h' -> H
  return f

pos :: Parser E.Pos
pos = do
  f <- file
  r <- rank
  return (f, r)

pieceType :: Parser PieceType
pieceType = do
  c <- option 'P' $ oneOf ['K', 'Q', 'R', 'B', 'N']
  let 
    f = case c of
      'K' -> King
      'Q' -> Queen
      'R' -> Rook
      'B' -> Bishop
      'N' -> Knight
      'P' -> Pawn
  return f
  
capture :: Parser CaptureType
capture = do
  mc <- optional $ char 'x'
  case mc of
    Nothing -> return NoCapture
    Just _  -> return Capture
  
promotion :: Parser PieceType
promotion = do
  char '='
  c <- oneOf ['Q', 'R', 'B', 'N']
  let 
    f = case c of
      'Q' -> Queen
      'R' -> Rook
      'B' -> Bishop
      'N' -> Knight
  return f

check :: Parser Check
check = do
  c <- oneOf ['+', '#']
  case c of
    '+' -> return Check
    '#' -> return CheckMate

algebraicNotationMove :: Parser AlgebraicNotation
algebraicNotationMove = do
  pt <- pieceType
  mf <- optional file
  mr <- optional rank
  cap <- capture
  mpos <- optional pos
  mpromo <- optional promotion
  mcheck <- optional check
  (pos, mf', mr') <- case (mpos, mf, mr) of
    (Nothing, Just f, Just r) -> return ((f, r), Nothing, Nothing)
    (Just p, _, _)            -> return (p, mf, mr)
    _                         -> customFailure "Could not parse move."
  return $ MoveExp pt pos cap mf' mr' mpromo mcheck

algebraicNotationCastling :: Parser AlgebraicNotation
algebraicNotationCastling = do
  void $ string "0-0"
  ms <- optional $ string "-0"
  case ms of
    Nothing -> return $ CastlingExp KingSide
    Just _  -> return $ CastlingExp QueenSide

algebraicNotationDrawOffer :: Parser AlgebraicNotation
algebraicNotationDrawOffer = do
  void $ char '='
  return DrawOfferExp

algebraicNotation :: Parser AlgebraicNotation
algebraicNotation = try algebraicNotationMove <|> try algebraicNotationCastling <|> try algebraicNotationDrawOffer 

parseAlgebraicNotation :: String -> Either ParseErrors AlgebraicNotation
parseAlgebraicNotation = parse algebraicNotation "stdin"
