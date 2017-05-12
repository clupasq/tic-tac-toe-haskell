import Test.Hspec
import Control.Exception

-- zz :: Int -> Int
-- zz 5 = 100
-- zz n = n*2

data Piece = X | O | Empty deriving (Eq, Show)
data Row = Row Piece Piece Piece deriving (Eq, Show)
data Board = Board Row Row Row deriving (Eq, Show)
data Coord = First | Second | Third

placeInRow :: Row -> Piece -> Coord -> Row
placeInRow (Row Empty s t) p First  = Row p s t
placeInRow (Row f Empty t) p Second = Row f p t
placeInRow (Row f s Empty) p Third  = Row f s p

place :: Board -> Piece -> Coord -> Coord -> Board
place (Board x y z) p First  c = let x' = placeInRow x p c in Board x' y  z
place (Board x y z) p Second c = let y' = placeInRow y p c in Board x  y' z
place (Board x y z) p Third  c = let z' = placeInRow z p c in Board x  y  z'

winner :: Board -> Piece
winner (Board (Row X X X)
              (Row _ _ _)
              (Row _ _ _)) = X

winner (Board (Row _ _ _)
              (Row X X X)
              (Row _ _ _)) = X

winner (Board (Row _ _ _)
              (Row _ _ _)
              (Row X X X)) = X

winner (Board (Row X _ _)
              (Row _ X _)
              (Row _ _ X)) = X

winner (Board (Row _ _ X)
              (Row _ X _)
              (Row X _ _)) = X

winner (Board (Row O O O)
              (Row _ _ _)
              (Row _ _ _)) = O

winner (Board (Row _ _ _)
              (Row O O O)
              (Row _ _ _)) = O

winner (Board (Row _ _ _)
              (Row _ _ _)
              (Row O O O)) = O

winner (Board (Row O _ _)
              (Row _ O _)
              (Row _ _ O)) = O

winner (Board (Row _ _ O)
              (Row _ O _)
              (Row O _ _)) = O

winner _ = Empty




emptyRow = Row Empty Empty Empty
emptyBoard = Board emptyRow emptyRow emptyRow

main = hspec $ do
  describe "TicTacToe" $ do
    it "can run a simple test" $ do
      1 `shouldBe` 1

    describe "Placing pieces in a row" $ do
      it "can place piece in an empty row" $ do
        placeInRow emptyRow X First  `shouldBe` Row X     Empty Empty
        placeInRow emptyRow X Second `shouldBe` Row Empty X     Empty
        placeInRow emptyRow X Third  `shouldBe` Row Empty Empty X
        placeInRow emptyRow O First  `shouldBe` Row O     Empty Empty
        placeInRow emptyRow O Second `shouldBe` Row Empty O     Empty
        placeInRow emptyRow O Third  `shouldBe` Row Empty Empty O

      it "can place piece alongside aothers" $ do
        let row = Row X Empty O
        placeInRow row O Second `shouldBe` Row X O O
        placeInRow row X Second `shouldBe` Row X X O

      it "cannot place piece in occupied slot" $ do
        let row = Row X Empty O
        evaluate(placeInRow row O First) `shouldThrow` anyException
        evaluate(placeInRow row O Third) `shouldThrow` anyException

    describe "Placing pieces on a board" $ do
      it "can place piece in empty board" $ do
        place emptyBoard X First  First `shouldBe` Board (Row X     Empty Empty)
                                                         (Row Empty Empty Empty)
                                                         (Row Empty Empty Empty)

        place emptyBoard X Second First `shouldBe` Board (Row Empty Empty Empty)
                                                         (Row X     Empty Empty)
                                                         (Row Empty Empty Empty)

        place emptyBoard X Third  First `shouldBe` Board (Row Empty Empty Empty)
                                                         (Row Empty Empty Empty)
                                                         (Row X     Empty Empty)


      it "can place more pieces on a board" $ do
        let initial = Board (Row X     Empty Empty)
                            (Row Empty O     Empty)
                            (Row Empty Empty Empty)

        place initial X Third Third `shouldBe` Board (Row X     Empty Empty)
                                                     (Row Empty O     Empty)
                                                     (Row Empty Empty X    )

    describe "Winner" $ do
      it "detects winner on a line" $ do
        let wonBoard = Board (Row X X X)
                             (Row O O Empty)
                             (Row O O Empty)

        winner wonBoard `shouldBe` X



