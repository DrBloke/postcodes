module PostcodeParser exposing (..)

import Parser exposing (..)
import Char
import Result
import Types exposing (Postcode)


{--
Parsing combines validation and formatting. The result could be made into
a new data type e.g. (firstPart, secondPart) rather than a single string. I
also think that the logic is much more readable than a regex, which would look
like this: ^([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z])))) [0-9][A-Za-z]{2})$
Who even knows if that would work!?
--}


parsePostcode : String -> Result Error Postcode
parsePostcode postcode =
    let
        pcFirst =
            String.toUpper <| String.trim <| String.dropRight 3 <| String.trim postcode

        pcSecond =
            String.toUpper <| String.right 3 <| String.trim postcode

        parsedFirst =
            Parser.run postcodeFirst pcFirst

        parsedSecond =
            Parser.run postcodeSecond pcSecond
    in
        --the flipped order is important here to identify to the user the problem with the second half of the postcode.
        Result.map2 (\b a -> a ++ " " ++ b) parsedSecond parsedFirst


postcodeFirst : Parser String
postcodeFirst =
    inContext "first part" <|
        source <|
            ignore (Exactly 1) Char.isUpper
                |. oneOf
                    [ ignore (Exactly 1) Char.isUpper
                    , succeed ()
                    ]
                |. ignore (Exactly 1) Char.isDigit
                |. oneOf
                    [ (ignore (Exactly 1) Char.isDigit)
                    , succeed ()
                    ]
                |. oneOf
                    [ ignore (Exactly 1) Char.isUpper
                    , succeed ()
                    ]
                |. spaces
                |. end


postcodeSecond : Parser String
postcodeSecond =
    inContext "second part" <|
        source <|
            ignore (Exactly 1) Char.isDigit
                |. ignore (Exactly 2) Char.isUpper


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
