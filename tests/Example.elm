module Example exposing (..)

import Expect exposing (Expectation)
import Pokemon as P
import Test exposing (..)


testToString : String -> P.Type -> Test
testToString str typ =
    test str <|
        \_ -> Expect.equal str (P.typeToString typ)

testOfString : P.Type -> String -> Test
testOfString typ str =
    test str <|
        \_ -> Expect.equal typ (P.typeOfString str)

suite : Test
suite =
    describe "タイプ"
        [ describe "タイプを日本語で表示"
            [ testToString "ノーマル" P.Normal
            , testToString "ほのお" P.Fire
            , testToString "みず" P.Water
            , testToString "くさ" P.Grass
            ]
        , describe "英語をタイプに変換、存在しないものはノーマルになる"
            [ testOfString P.Normal "normal"
            , testOfString P.Fire "fire"
            , testOfString P.Water "water"
            , testOfString P.Grass "grass"
            , testOfString P.Normal "go-suto"
            ]
        ]
    