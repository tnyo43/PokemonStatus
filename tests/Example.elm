module Example exposing (..)

import Dict exposing (Dict)
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


okubyouCSGekkouga = 
    P.Pokemon
        658 "ゲッコウガ" "" [ P.Water, P.Dark ]
        [ 72, 95, 67, 103, 71, 122 ]
        [ 31, 10, 31, 31, 31, 31 ]
        [ 0, 0, 0, 252, 4, 252 ]
        [ P.Unchange, P.Decrease, P.Unchange, P.Unchange, P.Unchange, P.Increase ]

hikaemeCSGekkouga = 
    P.Pokemon
        658 "ゲッコウガ" "" [ P.Water, P.Dark ]
        [ 72, 95, 67, 103, 71, 122 ]
        [ 31, 10, 31, 31, 31, 31 ]
        [ 0, 0, 0, 252, 4, 252 ]
        [ P.Unchange, P.Decrease, P.Unchange, P.Increase, P.Unchange, P.Unchange ]

ijiASMimikyu =
    P.Pokemon
        778 "ミミッキュ" "" [ P.Ghost, P.Fairy ]
        [ 55, 90, 80, 50, 105, 96 ]
        [ 31, 31, 31, 0, 31, 31 ]
        [ 4, 252, 0, 0, 0, 252 ]
        [ P.Unchange, P.Increase, P.Unchange, P.Decrease, P.Unchange, P.Unchange ]

zubutoiHBPikushi =
    P.Pokemon
        36 "ピクシー" "" [ P.Fairy ]
        [ 95, 70, 73, 95, 90, 60 ]
        [ 31, 31, 31, 31, 31, 31 ]
        [ 252, 0, 252, 0, 0, 4 ]
        [ P.Unchange, P.Decrease, P.Increase, P.Unchange, P.Unchange, P.Unchange ]

pokeJpList =
    [ P.PokeJp 1 "ピカチュウ"
    , P.PokeJp 2 "コダック"
    , P.PokeJp 100 "ウソッキー"
    , P.PokeJp 20 "ライボルト"
    ]

pokeJpDict = P.getPokemonNameDict pokeJpList


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
            , describe "要素の取得"
                [ testEqual "2つめを取得" (Just 3) (getFromList [1,2,3,4,5] 2)
                , testEqual "3つめを取得" (Just "d") (getFromList ["a", "b", "c", "d"] 3)
                , testEqual "配列外を取得するとNothing" Nothing (getFromList ["a", "b", "c", "d"] 4)
                , testEqual "負の数で配列外を取得するとNothing" Nothing (getFromList [1,2,3,4,5] -1)
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
        , describe "実数値計算"
            [ testEqual "臆病CSゲッコウガ" [ 147, 94, 87, 155, 92, 191 ] ( P.calcAllStatus okubyouCSGekkouga )
            , testEqual "控えめCSゲッコウガ" [ 147, 94, 87, 170, 92, 174 ] ( P.calcAllStatus hikaemeCSGekkouga )
            , testEqual "いじっぱりASミミッキュ" [ 131, 156, 100, 49, 125, 148 ] ( P.calcAllStatus ijiASMimikyu )
            , testEqual "ずぶといHBピクシー" [ 202, 81, 137, 115, 110, 81 ] ( P.calcAllStatus zubutoiHBPikushi )

            , testEqual "ゲッコウガのDに4振り" 92 ( P.calcStatus 50 4 71 31 4 P.Unchange )
            , testEqual "ゲッコウガのDに3振り（意味なし）" 91 ( P.calcStatus 50 4 71 31 3 P.Unchange )
            , testEqual "HPは上昇補正されない" ( P.calcStatus 50 0 72 31 252 P.Unchange ) ( P.calcStatus 50 0 72 31 252 P.Increase )
            , testEqual "HPは下降補正もされない" ( P.calcStatus 50 0 72 31 252 P.Unchange ) ( P.calcStatus 50 0 72 31 252 P.Decrease )
            ]
        , describe "日本語の名前と番号の辞書"
            [ testEqual "ピカチュウは1" (Dict.get "ピカチュウ" pokeJpDict) (Just 1)
            , testEqual "コダックは2" (Dict.get "コダック" pokeJpDict) (Just 2)
            , testEqual "ライボルトは20" (Dict.get "ライボルト" pokeJpDict) (Just 20)
            , testEqual "ウソッキーは100" (Dict.get "ウソッキー" pokeJpDict) (Just 100)
            , testEqual "ギルガルドはいない" (Dict.get "ギルガルド" pokeJpDict) Nothing
            ]
        ]