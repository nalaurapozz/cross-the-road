module FourBit exposing
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


import Base64
import Bitwise
import Bytes
import Bytes.Decode
import Bytes.Encode



-- Implementation of a 4-bit code unit, 2-page encoding of some of the most
-- common characters in English.

-- Encoding (numbers written in hex):
--                             e = 0x1
--                             a = 0x2
--                             r = 0x3
--                             i = 0x4
--                             o = 0x5
--                             t = 0x6
--                             n = 0x7
--                             s = 0x8
--                             l = 0x9
--                             c = 0xa
--                             u = 0xb
--                             d = 0xc
--                             p = 0xd
--                         SPACE = 0xe
-- THE NEXT CHARACTER IS CAPITAL = 0xf

--   NEXT BYTES ARE UTF-8 (TODO) = 0x00
--                             m = 0x01
--                             h = 0x02
--                             g = 0x03
--                             b = 0x04
--                             f = 0x05
--                             y = 0x06
--                             w = 0x07
--                             k = 0x08
--                             v = 0x09
--                             x = 0x0a
--                             z = 0x0b
--                             j = 0x0c
--                             q = 0x0d
--                             , = 0x0e
--            (currently unused)   0x0f


letters1 : List Char
letters1 = String.toList "eariotnslcudp "

letters2 : List Char
letters2 = String.toList "mhgbfywkvxzjq,"


-- Find this character in this string and return its index
find : c -> List c -> Maybe Int
find ch str =
    let
        f : Int -> List c -> Maybe Int
        f idx lst =
            case lst of
                c :: rest ->
                    if c == ch then
                        Just idx
                    else
                        f (idx + 1) rest
                _ -> Nothing
    in
        f 0 str



-- Given a string, convert it to numbers in the 4-byte encoding
stringToNumberList : String -> List Int
stringToNumberList input =
    let
        charToNumbers : Char -> List Int
        charToNumbers ch =
            if Char.isUpper ch then
                [0xf] ++ lowerCharToNumbers (Char.toLower ch)
            else
                lowerCharToNumbers ch

        lowerCharToNumbers : Char -> List Int
        lowerCharToNumbers ch =
            case find ch letters1 of
                Just i -> [i + 1]
                Nothing ->
                    case find ch letters2 of
                        Just j -> [0x0, j + 1]
                        Nothing -> [0x0, 0x0]
                        -- TODO: Pad then UTF-8
    in
        List.concatMap charToNumbers (String.toList input)


type Mode = Normal | Caps


-- Give a list of 4-byte numbers in our encoding, convert it to a string
numberListToString : List Int -> String
numberListToString input =
    let
        toUpperIfCaps : Mode -> Char -> Char
        toUpperIfCaps mode =
            if mode == Caps then
                Char.toUpper
            else
                identity

        nth : Mode -> Int -> List Char -> Char
        nth mode n lst =
            List.drop n lst
                |> List.head
                |> Maybe.withDefault '?'
                |> toUpperIfCaps mode

        toChars : Mode -> List Int -> List Char
        toChars mode lst =
            case lst of
                [0x0]          -> []  -- Trailing zero is ignored
                0x0 :: x :: xs -> nth mode (x-1) letters2 :: toChars Normal xs
                0xf :: xs      -> toChars Caps xs
                x :: xs        -> nth mode (x-1) letters1 :: toChars Normal xs
                []             -> []
    in
        toChars Normal input
            |> String.fromList


-- Given numbers in the 4-bit encoding, combine into 8-bit octets
toOctets : List Int -> List Int
toOctets input =
    let
        toOctet : Int -> Int -> Int
        toOctet a b =
            (Bitwise.shiftLeftBy 4 a)  + b
    in
        case input of
            a :: b :: rest ->
                toOctet a b :: toOctets rest
            a :: [] ->
                [toOctet a 0x0]
            _ -> []


-- Given 8-bit octets, split into 4-bit chunks
fromOctets : List Int -> List Int
fromOctets input =
    let
        top : Int -> Int
        top x = Bitwise.shiftRightBy 4 x
        bottom : Int -> Int
        bottom x = Bitwise.and x 0xf
    in
        case input of
            x :: xs -> top x :: bottom x :: fromOctets xs
            [] -> []


-- Given a list of octets, encode as base 64
base64Encode : List Int -> String
base64Encode input =
    input
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode
        |> Base64.fromBytes
        |> Maybe.withDefault ""


decoderMultipleInt8s : Int -> Bytes.Decode.Decoder (List Int)
decoderMultipleInt8s len =
    let
        listStep :
            (Int, List Int) ->
            Bytes.Decode.Decoder (Bytes.Decode.Step (Int, List Int) (List Int))
        listStep (n, xs) =
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done xs)
            else
                -- I don't understand why it's not "x :: xs" below,
                -- instead of "xs ++ [x]".
                Bytes.Decode.map
                    (\x -> Bytes.Decode.Loop (n - 1, xs ++ [x]))
                    Bytes.Decode.unsignedInt8
    in
        Bytes.Decode.loop (len, []) listStep


-- Given a base 64 string, return a list of octets
base64Decode : String -> List Int
base64Decode input =
    let
        bytes : Bytes.Bytes
        bytes =
            Base64.toBytes input
                |> Maybe.withDefault
                    (Bytes.Encode.encode (Bytes.Encode.string ""))
        decoder : Bytes.Decode.Decoder (List Int)
        decoder = decoderMultipleInt8s (Bytes.width bytes)
        ints : Maybe (List Int)
        ints = Bytes.Decode.decode decoder bytes
    in
        ints |> Maybe.withDefault []


-- Encode a normal string to my 4-bit special encoding (in base64)
encode : String -> String
encode =
    stringToNumberList >> toOctets >> base64Encode


-- Decode from my special 4-bit encoding (in base64) to a normal string
decode : String -> String
decode =
    base64Decode >> fromOctets >> numberListToString
