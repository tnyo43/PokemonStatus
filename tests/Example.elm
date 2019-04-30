module Example exposing (..)

import Expect exposing (Expectation)
import Pokemon as P
import Test exposing (..)
import Util exposing (..)


testToString : String -> P.Type -> Test
testToString str typ =
    test str <|
        \_ -> Expect.equal str (P.typeToString typ)

testOfString : P.Type -> String -> Test
testOfString typ str =
    test str <|
        \_ -> Expect.equal typ (P.typeOfString str)

zeros : List Int
zeros = List.repeat 6 0

testEqual : String -> a -> a -> Test
testEqual str lst1 lst2 =
    test str <|
        \_ -> Expect.equal lst1 lst2

suite : Test
suite =
    describe "テスト"
        [ describe "Util"
            [ describe "要素の変更"
                [ testEqual "3つめを変更" (Just [0,0,1,0,0,0]) (updateList zeros 2 1)
                , testEqual "6つめを変更" (Just [0,0,0,0,0,100]) (updateList zeros 5 100)
                , testEqual "配列外を変更するとNothing" (Nothing) (updateList zeros 6 19)
                , testEqual "負の数で配列外を変更するとNothing" (Nothing) (updateList zeros -1 19)
                ]
            ]
        , describe "タイプ"
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
        , descript "実数値計算"
            [ descript "HPだけ式が異なる"
                [ --TODO : HPの計算式
                ]
            , describe "他は同じ式"
                [ --TODO : 他のパラの計算式
                ]
            ]
        ]