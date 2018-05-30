module PostcodeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parser exposing (..)
import PostcodeParser exposing (..)


suite : Test
suite =
    describe "Parses postcodes correctly"
        [ test "Parses correct postcode" <|
            \_ ->
                Expect.equal (parsePostcode ("CB3 0FA")) (Ok "CB3 0FA")
        , test "Identifies nonsense" <|
            \_ ->
                Expect.equal (parsePostcode ("ksfjksfkj")) (Err { row = 1, col = 1, source = "FKJ", problem = BadRepeat, context = [ { row = 1, col = 1, description = "second part" } ] })
        , test "Parses lower case" <|
            \_ ->
                Expect.equal (parsePostcode ("cb3 0fa")) (Ok "CB3 0FA")
        , test "Parses no space case" <|
            \_ ->
                Expect.equal (parsePostcode ("cb30fa")) (Ok "CB3 0FA")
        , test "parses 1 letter postcodes" <|
            \_ ->
                Expect.equal (parsePostcode ("e17 4ru")) (Ok "E17 4RU")
        , test "parses 1 number postcodes" <|
            \_ ->
                Expect.equal (parsePostcode ("nw2 4ru")) (Ok "NW2 4RU")
        , test "parses 1 letter and 1 number postcodes" <|
            \_ ->
                Expect.equal (parsePostcode ("e4 4ru")) (Ok "E4 4RU")
        , test "parses 1 letter after first set of numbers" <|
            \_ ->
                Expect.equal (parsePostcode ("ec4v 4ru")) (Ok "EC4V 4RU")
        , test "identifies error in first part due to letters" <|
            \_ ->
                Expect.equal (parsePostcode ("abc4v 4ru")) (Err { row = 1, col = 3, source = "ABC4V", problem = BadRepeat, context = [ { row = 1, col = 1, description = "first part" } ] })
        , test "identifies error in first part due to numbers" <|
            \_ ->
                Expect.equal (parsePostcode ("ab123 4ru")) (Err { row = 1, col = 5, source = "AB123", problem = ExpectingEnd, context = [ { row = 1, col = 1, description = "first part" } ] })
        , test "fails weird character" <|
            \_ ->
                Expect.equal (parsePostcode ("e$4v 4ru")) (Err { row = 1, col = 2, source = "E$4V", problem = BadRepeat, context = [ { row = 1, col = 1, description = "first part" } ] })
        ]
