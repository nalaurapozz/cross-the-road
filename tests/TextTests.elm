module TextTests exposing (..)


import Test exposing (describe,test)
import Expect


import FourBit exposing
    ( base64Encode
    , base64Decode
    , decode
    , encode
    , find
    , fromOctets
    , numberListToString
    , stringToNumberList
    , toOctets
    )


tests = describe "Tests of the Text conversion"

    [ test "First element in find" <|
        \() -> Expect.equal (find 0 [0, 1]) (Just 0)

    , test "Later element in find" <|
        \() -> Expect.equal (find 3 [2, 1, 3, 4]) (Just 2)

    , test "Last element in find" <|
        \() -> Expect.equal (find 3 [2, 1, 3]) (Just 2)

    , test "Cannot find" <|
        \() -> Expect.equal (find 3 [2, 1, 5, 4]) Nothing

    , test "find in empty list" <|
        \() -> Expect.equal (find 3 []) Nothing


    , test "Lowercase letters to numberlist" <|
        \() -> Expect.equal
            (stringToNumberList "ab c")
            [0x2, 0x0, 0x4, 0xe, 0xa]

    , test "Unrecognised letters to numberlist" <|
        \() -> Expect.equal
            (stringToNumberList "^|")
            [0x0, 0x0, 0x0, 0x0]

    , test "Capitals and lowercase to numberlist" <|
        \() -> Expect.equal
            (stringToNumberList "Ali B")
            [0xf, 0x2, 0x9, 0x4, 0xe, 0xf, 0x0, 0x4]

    , test "Even-length numberlist to bytes" <|
        \() -> Expect.equal
            (toOctets [0xf, 0x2, 0x9, 0x4, 0xe, 0xf, 0x0, 0x4])
            [0xf2, 0x94, 0xef, 0x04]

    , test "Odd-length numberlist to bytes" <|
        \() -> Expect.equal
            (toOctets [0xf, 0x2, 0x9, 0x4, 0xe, 0xf, 0x0, 0x4, 0x5])
            [0xf2, 0x94, 0xef, 0x04, 0x50]

    , test "Bytes to base64" <|
        \() -> Expect.equal
            (base64Encode [0xf2, 0x94, 0xef, 0x04])
            "8pTvBA=="

    , test "Base64 to bytes" <|
        \() -> Expect.equal
            (base64Decode "8pTvBA==")
            [0xf2, 0x94, 0xef, 0x04]

    , test "bytes to numberlist" <|
        \() -> Expect.equal
            (fromOctets [0xf2, 0x94, 0xef, 0x04])
            [0xf, 0x2, 0x9, 0x4, 0xe, 0xf, 0x0, 0x4]

    , test "numberlist to lowercase letters" <|
        \() -> Expect.equal
            (numberListToString [0x2, 0x0, 0x4, 0xe, 0xa])
            "ab c"

    , test "numberlist to unrecognised letters" <|
        \() -> Expect.equal
            (numberListToString [0x12, 0x0, 0xf, 0x2])
            "??a"

    , test "numberlist trailing 0 to letters" <|
        \() -> Expect.equal
            (numberListToString [0x2, 0x2, 0x0])
            "aa"

    , test "numberlist to capital and lowercase letters" <|
        \() -> Expect.equal
            (numberListToString [0xf, 0x2, 0x9, 0x4, 0xe, 0xf, 0x0, 0x4])
            "Ali B"


    , test "String all the way to base64" <|
        \() -> Expect.equal
            (encode "Ali B")
            "8pTvBA=="

    , test "Long string all the way to base64" <|
        \() -> Expect.equal
            (encode "The quick brown fox jumped, over the lazy dog")
            "9gIeDbSgjgQ1B34FUK4MsB0cDuUJE+YCHpILBuxQMA=="

    , test "base64 all the way to string" <|
        \() -> Expect.equal
            (decode "8pTvBA==")
            "Ali B"

    , test "Long base64 all the way to string" <|
        \() -> Expect.equal
            (decode "9gIeDbSgjgQ1B34FUK4MsB0cDuUJE+YCHpILBuxQMA==")
            "The quick brown fox jumped, over the lazy dog"

    ]
