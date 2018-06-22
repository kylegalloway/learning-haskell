import Test.Hspec
import Testable ( cell2char
                , findWord
                , findWords
                , formatGrid
                , grid
                , gridWithCoords
                , languages
                )

gwc = gridWithCoords grid

testFindWord word =
    let (Just result) = findWord gwc word
        string = map cell2char result
    in string `shouldBe` word

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            (formatGrid (gridWithCoords ["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "Should find words that exist in the Grid" $ do
            testFindWord "HASKELL"
            testFindWord "PERL"
        it "Should not find words that do not exist in the Grid" $ do
            findWord gwc "HAMSTER" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words that exist in the Grid" $ do
            let found = findWords gwc languages
                asString = map (map cell2char) found
            asString `shouldBe` languages
        it "Should not find words that do not exist in the Grid" $ do
            findWords gwc ["FRENCH", "GERMAN", "ENGLISH"] `shouldBe` []

