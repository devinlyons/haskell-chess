{-# LANGUAGE FlexibleInstances #-}

module Chess.Domain.Elements where

------------------------------------------------------------------------------------------------------------------------
import qualified Data.Text as T
------------------------------------------------------------------------------------------------------------------------

data Move 
  = SimpleMove BasicMove 
  | PawnDoubleMove BasicMove 
  | PawnEnPassantMove BasicMove 
  | PawnPromotionMove PieceType BasicMove
  deriving (Show)

data BasicMove
  = BasicMove
  { basicMovePiece        :: Piece
  , basicMoveSource       :: Pos
  , basicMoveDestination  :: Pos
  } deriving (Show)

data MoveMetrics
  = MoveMetrics
  { moveMetricsPiece       :: Piece
  , moveMetricsPromotion   :: Maybe PieceType
  , moveMetricsSource      :: Pos
  , moveMetricsDestination :: Pos
  , moveMetricsDirection   :: MoveDirectionType
  , moveMetricsMoveX       :: Int
  , moveMetricsMoveY       :: Int
  } deriving (Show)

class PrettyPrint a where
  prettyPrint :: a -> T.Text

data MoveDirectionType = VerticalMove | HorizontalMove | DiagonalMove | KnightMove deriving (Show, Eq)
data CaptureType = Capture | NoCapture deriving (Show)
data Check = Check | CheckMate deriving (Show)
data Side = KingSide | QueenSide deriving (Show)
data Castling = Castling Side
data DrawOffer = DrawOffer
data AlgebraicNotation 
  = MoveExp PieceType Pos CaptureType (Maybe File) (Maybe Rank) (Maybe PieceType) (Maybe Check)
  | CastlingExp Side
  | DrawOfferExp
  deriving (Show)

data Rank = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Ord)

instance Enum Rank where
  toEnum 0 = One
  toEnum 1 = Two
  toEnum 2 = Three
  toEnum 3 = Four
  toEnum 4 = Five
  toEnum 5 = Six
  toEnum 6 = Seven
  toEnum 7 = Eight
  toEnum _ = One
  fromEnum One   = 0
  fromEnum Two   = 1
  fromEnum Three = 2
  fromEnum Four  = 3
  fromEnum Five  = 4
  fromEnum Six   = 5
  fromEnum Seven = 6
  fromEnum Eight = 7

data File = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord)

instance Enum File where
  toEnum 0 = A
  toEnum 1 = B
  toEnum 2 = C
  toEnum 3 = D
  toEnum 4 = E
  toEnum 5 = F
  toEnum 6 = G
  toEnum 7 = H
  toEnum _ = A
  fromEnum A = 0
  fromEnum B = 1
  fromEnum C = 2
  fromEnum D = 3
  fromEnum E = 4
  fromEnum F = 5
  fromEnum G = 6
  fromEnum H = 7

type Pos = (File, Rank)
data Player = Black | White deriving (Show, Eq)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

instance PrettyPrint PieceType where
  prettyPrint King   = "k"
  prettyPrint Queen  = "q"
  prettyPrint Rook   = "r"
  prettyPrint Bishop = "b"
  prettyPrint Knight = "n"
  prettyPrint Pawn   = "p"

data Piece = Piece Player PieceType deriving (Show, Eq)

instance PrettyPrint Piece where
  prettyPrint (Piece Black pt) = T.toUpper $ prettyPrint pt
  prettyPrint (Piece White pt) = prettyPrint pt

instance PrettyPrint (Maybe Piece) where
  prettyPrint Nothing  = " "
  prettyPrint (Just p) = prettyPrint p

data Files
  = Files
  { fileA :: Maybe Piece
  , fileB :: Maybe Piece
  , fileC :: Maybe Piece
  , fileD :: Maybe Piece
  , fileE :: Maybe Piece
  , fileF :: Maybe Piece
  , fileG :: Maybe Piece
  , fileH :: Maybe Piece
  }
  deriving (Show)

instance PrettyPrint Files where
  prettyPrint (Files a b c d e f g h) 
    =  "| " <> prettyPrint a <> " "
    <> "| " <> prettyPrint b <> " "
    <> "| " <> prettyPrint c <> " "
    <> "| " <> prettyPrint d <> " "
    <> "| " <> prettyPrint e <> " "
    <> "| " <> prettyPrint f <> " "
    <> "| " <> prettyPrint g <> " "
    <> "| " <> prettyPrint h <> " "

data Ranks
  = Ranks
  { rankOne   :: Files
  , rankTwo   :: Files
  , rankThree :: Files
  , rankFour  :: Files
  , rankFive  :: Files
  , rankSix   :: Files
  , rankSeven :: Files
  , rankEight :: Files
  }
  deriving (Show)

instance PrettyPrint Ranks where
  prettyPrint (Ranks one two three four five six seven eight) = 
    let
      t1 = "    a   b   c   d   e   f   g   h"
      t2 = "8 " <> prettyPrint eight <> "| 8"
      t3 = "7 " <> prettyPrint seven <> "| 7"
      t4 = "6 " <> prettyPrint six   <> "| 6"
      t5 = "5 " <> prettyPrint five  <> "| 5"
      t6 = "4 " <> prettyPrint four  <> "| 4"
      t7 = "3 " <> prettyPrint three <> "| 3"
      t8 = "2 " <> prettyPrint two   <> "| 2"
      t9 = "1 " <> prettyPrint one   <> "| 1"
    in T.intercalate "\r\n" [t1, t2, t3, t4, t5, t6, t7, t8, t9, t1]

data Board = Board Player Ranks (Maybe Pos) deriving (Show)

instance PrettyPrint Board where
  prettyPrint (Board p rs _) = prettyPrint rs <> "\r\nCurrent Player: " <> (T.pack $ show p)

newGame :: Board
newGame = Board White rs Nothing
  where
    qrw = Piece White Rook
    qkw = Piece White Knight
    qbw = Piece White Bishop
    qw  = Piece White Queen
    kw  = Piece White King
    kbw = Piece White Bishop
    kkw = Piece White Knight
    krw = Piece White Rook
    paw = Piece White Pawn
    pbw = Piece White Pawn
    pcw = Piece White Pawn
    pdw = Piece White Pawn
    pew = Piece White Pawn
    pfw = Piece White Pawn
    pgw = Piece White Pawn
    phw = Piece White Pawn
    pab = Piece Black Pawn
    pbb = Piece Black Pawn
    pcb = Piece Black Pawn
    pdb = Piece Black Pawn
    peb = Piece Black Pawn
    pfb = Piece Black Pawn
    pgb = Piece Black Pawn
    phb = Piece Black Pawn
    qrb = Piece Black Rook
    qkb = Piece Black Knight
    qbb = Piece Black Bishop
    qb  = Piece Black Queen
    kb  = Piece Black King
    kbb = Piece Black Bishop
    kkb = Piece Black Knight
    krb = Piece Black Rook
    f1 = Files (Just qrw) (Just qkw) (Just qbw) (Just qw) (Just kw) (Just kbw) (Just kkw) (Just krw)
    f2 = Files (Just paw) (Just pbb) (Just pcw) (Just pdw) (Just pew) (Just pfw) (Just pgw) (Just phw)
    fn = Files Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    f7 = Files (Just pab) (Just pbw) (Just pcb) (Just pdb) (Just peb) (Just pfb) (Just pgb) (Just phb)
    f8 = Files (Just qrb) (Just qkb) (Just qbb) (Just qb) (Just kb) (Just kbb) (Just kkb) (Just krb)
    rs = Ranks f1 f2 fn fn fn fn f7 f8
