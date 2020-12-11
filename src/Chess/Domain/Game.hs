{-# Language BlockArguments #-}

module Chess.Domain.Game where

------------------------------------------------------------------------------------------------------------------------
import Debug.Trace
import Control.Monad (void)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.IO
------------------------------------------------------------------------------------------------------------------------
import Chess.Domain.Elements
import qualified Chess.Domain.Parsing as P
------------------------------------------------------------------------------------------------------------------------

startGame :: IO ()
startGame = playGame newGame

putTxtLn :: T.Text -> IO ()
putTxtLn = putStrLn . T.unpack

playGame :: Board -> IO ()
playGame b = do
  putTxtLn $ prettyPrint b
  putStrLn "What's your move?"
  mvs <- getLine
  putStrLn $ "Input: " ++ mvs
  case P.parseAlgebraicNotation mvs of
    Left er1 -> handleErr (show er1) b
    Right an -> case an of
      DrawOfferExp                   -> do
        putStrLn "The other player has offered a draw. Do you accept (n)? "
        c <- getLine
        let 
          answer = case c of
            "y" -> True
            "Y" -> True
            _   -> False
        case answer of
          True  -> putStrLn "The draw has been accepted."
          False -> do
            putStrLn "The draw has been refused."
            playGame b
      CastlingExp s                  -> do
        let
          pl = getPlayer b
          r = case pl of
            Black -> Eight
            White -> One
          f = case s of
            QueenSide -> A
            KingSide  -> H
          mr = getPlayerPiece b (f, r)
          mk = getPlayerPiece b (E, r)
        case (mr, mk, hasClearLineOfSight b (f, r) (E, r) HorizontalMove) of
          (Just ro@(Piece _ Rook), Just ki@(Piece _ King), True) -> do
            playGame . flipPlayer . setPiece (Just ki) (f, r) $ setPiece (Just ro) (E, r) b
          _                  -> do
            putStrLn "The king or rook are not in the correct positions."
            playGame b
      MoveExp pt d ct mf mr promo mc -> 
        case getCandidatePieces pt mf mr of
          [] -> handleErr "Could not find any candidate pieces." b
          xs -> case catMaybes . fmap (mkMove b) . catMaybes $ fmap (uncurry (mkMoveMetics promo d)) xs of
            []     -> handleErr "Invalid move." b
            (x:[]) -> playGame $ applyMove b x
            xs     -> handleErr "Ambiguous move." b
  where
    handleErr er b = do
      putStrLn er
      putStrLn "Press enter to try again."
      void getLine
      playGame b
    eqOrNothing a Nothing  = True
    eqOrNothing a (Just b) = a == b
    getPieces (Board pl (Ranks r1 r2 r3 r4 r5 r6 r7 r8) _) pt = catMaybes $
      getRankPieces One pl pt r1
      <> getRankPieces Two pl pt r2
      <> getRankPieces Three pl pt r3
      <> getRankPieces Four pl pt r4
      <> getRankPieces Five pl pt r5
      <> getRankPieces Six pl pt r6
      <> getRankPieces Seven pl pt r7
      <> getRankPieces Eight pl pt r8
    getRankPieces r pl pt (Files f1 f2 f3 f4 f5 f6 f7 f8) = 
      [ ifMatch r A pl pt f1
      , ifMatch r B pl pt f2
      , ifMatch r C pl pt f3
      , ifMatch r D pl pt f4
      , ifMatch r E pl pt f5
      , ifMatch r F pl pt f6
      , ifMatch r G pl pt f7
      , ifMatch r H pl pt f8
      ]
    ifMatch _ _ _ _ Nothing                      = Nothing
    ifMatch r f pl1 pt1 (Just p@(Piece pl2 pt2)) = case pt1 == pt2 && pl1 == pl2 of
      True  -> Just (p, (f, r))
      False -> Nothing
    getCandidatePieces pt mf mr = filter (\(_, (f, r)) -> eqOrNothing f mf && eqOrNothing r mr) $ getPieces b pt

applyMove :: Board -> Move -> Board
applyMove b (SimpleMove (BasicMove pi s d))                     = 
  flipPlayer . setEnPassant Nothing $ movePiece b pi s d
applyMove b (PawnDoubleMove (BasicMove pi s d))                 = 
  flipPlayer . setEnPassant (Just d) $ movePiece b pi s d
applyMove b (PawnEnPassantMove (BasicMove pi s d))              = 
  flipPlayer . captureEnPassant $ movePiece b pi s d
applyMove b (PawnPromotionMove pt (BasicMove (Piece pl _) s d)) = 
  flipPlayer $ movePiece b (Piece pl pt) s d

movePiece :: Board -> Piece -> Pos -> Pos -> Board
movePiece b pi s d = setPiece (Just pi) d $ setPiece Nothing s b

mkMove :: Board -> MoveMetrics -> Maybe Move
mkMove b (MoveMetrics pi@(Piece pl pt) promo s@(sf, sr) d@(df, dr) mdt r f) =
  let 
    ar = abs(r)
    af = abs(f)
    mdpi = getPiece b d
    mdpipl = fmap (\(Piece pl _) -> pl) mdpi
    isEmpty = mdpi == Nothing
    canCapture = mdpipl == Just (otherPlayer pl)
    isEmptyOrCanCapture = isEmpty || canCapture
  in case pt of
    King   -> 
      if (ar == 0 || ar == 1) && (af == 0 || af == 1) && isEmptyOrCanCapture
      then Just . SimpleMove $ BasicMove pi s d
      else Nothing
    Queen  -> 
      if (mdt == VerticalMove || mdt == HorizontalMove || mdt == DiagonalMove)
        && isEmptyOrCanCapture && hasClearLineOfSight b s d mdt
      then Just . SimpleMove $ BasicMove pi s d
      else Nothing
    Rook   -> 
      if (mdt == VerticalMove || mdt == HorizontalMove)
        && isEmptyOrCanCapture && hasClearLineOfSight b s d mdt
      then Just . SimpleMove $ BasicMove pi s d
      else Nothing
    Bishop -> 
      if mdt == DiagonalMove && isEmptyOrCanCapture && hasClearLineOfSight b s d mdt
      then Just . SimpleMove $ BasicMove pi s d
      else Nothing
    Knight -> 
      if mdt == KnightMove && isEmptyOrCanCapture
      then Just . SimpleMove $ BasicMove pi s d
      else Nothing
    Pawn   -> 
      let
        promoRank = case pl of
          Black -> One
          White -> Eight
        simpleOrPromote = case promo of
          Nothing  -> Just . SimpleMove $ BasicMove pi s d
          Just ppt -> 
            if promoRank == dr
            then Just . PawnPromotionMove ppt $ BasicMove pi s d
            else Nothing
      in 
        case mdt of
          VerticalMove ->
            case r of
              1 -> 
                case hasClearLineOfSightInclusive b s d mdt of
                  False -> Nothing
                  True  -> simpleOrPromote
              2 -> 
                let 
                  atFirstRank = case pl of
                    Black -> sr == Seven
                    White -> sr == Two
                in 
                  if atFirstRank && hasClearLineOfSightInclusive b s d mdt
                  then Just . PawnDoubleMove $ BasicMove pi s d
                  else Nothing
              _ -> Nothing
          DiagonalMove -> 
            if canCapture
            then simpleOrPromote
            else 
              let 
                rankMod = case pl of
                  Black -> 1
                  White -> (-1)
                enPassantRank = toEnum $ fromEnum dr + rankMod
              in
                if getEnPassant b == Just (df, enPassantRank)
                then Just . PawnEnPassantMove $ BasicMove pi s d
                else Nothing
          _ -> Nothing             

mkMoveMetics :: Maybe PieceType -> Pos -> Piece -> Pos -> Maybe MoveMetrics
mkMoveMetics mpt (df, dr) pi@(Piece pl pt) (sf, sr) = 
  let
    fi = fromEnum df - fromEnum sf
    ri = fromEnum dr - fromEnum sr
    m = case pl of
      Black -> (-1)
      White -> 1
    mmdt = deduceMoveDirectionType (sf, sr) (df, dr)
  in case mmdt of
    Nothing  -> Nothing
    Just mdt -> traceShowId . Just $ MoveMetrics pi mpt (sf, sr) (df, dr) mdt (ri * m) (fi * m)

deduceMoveDirectionType :: Pos -> Pos -> Maybe MoveDirectionType
deduceMoveDirectionType (sf, sr) (df, dr) = 
  case (compare sf df, compare sr dr) of
    (EQ, EQ) -> Nothing
    (EQ, _)  -> Just VerticalMove
    (_, EQ)  -> Just HorizontalMove
    _        -> case (abs (fromEnum df - fromEnum sf), abs (fromEnum dr - fromEnum sr)) of
      (1, 2)   -> Just KnightMove
      (2, 1)   -> Just KnightMove
      (fd, rd) -> case fd == rd of
        False -> Nothing
        True  -> Just DiagonalMove

hasClearLineOfSight :: Board -> Pos -> Pos -> MoveDirectionType -> Bool
hasClearLineOfSight b s (df, _) HorizontalMove = go $ moveToward s
  where
    moveToward (f, r) =
      let
        fi = fromEnum f
        f' = case compare fi (fromEnum df) of
          GT -> toEnum $ fi - 1
          EQ -> f
          LT -> toEnum $ fi + 1
      in (f', r)
    go pos@(f, _) = case f == df of
      True  -> True
      False -> case getPiece b pos of
        Nothing -> go $ moveToward pos
        Just _  -> False
hasClearLineOfSight b s (df, dr) VerticalMove = go $ moveToward s
  where
    moveToward (f, r) =
      let
        ri = fromEnum r
        r' = case compare ri (fromEnum dr) of
          GT -> toEnum $ ri - 1
          EQ -> r
          LT -> toEnum $ ri + 1
      in (f, r')
    go pos@(_, r) = case r == dr of
      True  -> True
      False -> case getPiece b pos of
        Nothing -> go $ moveToward pos
        Just _  -> False
hasClearLingOfSight b s (df, dr) DiagonalMove = go $ moveToward s
  where
    moveToward (f, r) =
      let
        fi = fromEnum f
        ri = fromEnum r
        f' = case compare fi (fromEnum df) of
          GT -> toEnum $ fi - 1
          EQ -> f
          LT -> toEnum $ fi + 1
        r' = case compare ri (fromEnum dr) of
          GT -> toEnum $ ri - 1
          EQ -> r
          LT -> toEnum $ ri + 1
      in (f', r')
    go fr@(f, r) = case f == df && r == dr of
      True  -> True
      False -> case getPiece b fr of
        Nothing -> go $ moveToward fr
        Just _  -> False
hasClearLingOfSight _ _ _ KnightMove = True

hasClearLineOfSightInclusive :: Board -> Pos -> Pos -> MoveDirectionType -> Bool
hasClearLineOfSightInclusive b s d mdt = hasClearLineOfSight b s d mdt && getPiece b d == Nothing

flipPlayer :: Board -> Board
flipPlayer (Board pl fs mep) = Board (otherPlayer pl) fs mep

otherPlayer :: Player -> Player
otherPlayer Black = White
otherPlayer White = Black

getPlayer :: Board -> Player
getPlayer (Board p _ _) = p

getEnPassant :: Board -> Maybe Pos
getEnPassant (Board _ _ mep) = mep

setEnPassant :: Maybe Pos -> Board -> Board
setEnPassant mep (Board pl fs _) = Board pl fs mep

captureEnPassant :: Board -> Board
captureEnPassant b@(Board _ _ Nothing) = b
captureEnPassant (Board pl fs (Just ep)) = setPiece Nothing ep $ Board pl fs Nothing

getPlayerPiece :: Board -> Pos -> Maybe Piece
getPlayerPiece b@(Board p1 _ _) pos = 
  let mp = getPiece b pos
  in case mp of
    Nothing           -> Nothing
    Just (Piece p2 _) -> case p1 == p2 of
      False -> Nothing
      True  -> mp

getPiece :: Board -> Pos -> Maybe Piece
getPiece b pos =
  let fs = getBoardFiles b pos
  in getFilePiece fs pos

setPiece :: Maybe Piece -> Pos -> Board -> Board
setPiece mpi (f, r) b@(Board pl (Ranks r1 r2 r3 r4 r5 r6 r7 r8) mep) = Board pl rs mep
  where
    r1' = setFilePiece r1 f mpi (r == One)
    r2' = setFilePiece r2 f mpi (r == Two)
    r3' = setFilePiece r3 f mpi (r == Three)
    r4' = setFilePiece r4 f mpi (r == Four)
    r5' = setFilePiece r5 f mpi (r == Five)
    r6' = setFilePiece r6 f mpi (r == Six)
    r7' = setFilePiece r7 f mpi (r == Seven)
    r8' = setFilePiece r8 f mpi (r == Eight)
    rs = Ranks r1' r2' r3' r4' r5' r6' r7' r8'

setFilePiece :: Files -> File -> Maybe Piece -> Bool -> Files
setFilePiece fs _ _ False                               = fs
setFilePiece (Files f1 f2 f3 f4 f5 f6 f7 f8) f mpi True = Files f1' f2' f3' f4' f5' f6' f7' f8'
  where
    f1' = bool f1 mpi (f == A)
    f2' = bool f2 mpi (f == B)
    f3' = bool f3 mpi (f == C)
    f4' = bool f4 mpi (f == D)
    f5' = bool f5 mpi (f == E)
    f6' = bool f6 mpi (f == F)
    f7' = bool f7 mpi (f == G)
    f8' = bool f8 mpi (f == H)

getBoardFiles :: Board -> Pos -> Files
getBoardFiles (Board _ rs _) (_, r) =
  case r of
    One   -> rankOne rs
    Two   -> rankTwo rs
    Three -> rankThree rs
    Four  -> rankFour rs
    Five  -> rankFive rs
    Six   -> rankSix rs
    Seven -> rankSeven rs
    Eight -> rankEight rs

getFilePiece :: Files -> Pos -> Maybe Piece
getFilePiece fs (f, _) =
  case f of
    A -> fileA fs
    B -> fileB fs
    C -> fileC fs
    D -> fileD fs
    E -> fileE fs
    F -> fileF fs
    G -> fileG fs
    H -> fileH fs
