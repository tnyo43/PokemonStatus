module Pokemon exposing (..)

import Json.Decode as Decode exposing (Decoder)


-- POKEMON
type Type =
    Normal | Fire | Water | Grass | Electric
    | Ice | Fighting | Poison | Ground | Flying
    | Psychic | Bug | Rock | Ghost | Dragon
    | Dark | Steel | Fairy

type alias PokeJp =
    { no : Int
    , name : String
    }

type alias Pokemon =
    { no : Int
    , name : String
    , imgUrl : String
    , types : List Type
    , stats : List Int
    , indiv : List Int
    , effort : List Int
    }

urlStats = "https://pokeapi.co/api/v2/pokemon/"
urlJapanese = "https://raw.githubusercontent.com/kotofurumiya/pokemon_data/master/data/pokemon_data.json"


typeToString : Type -> String
typeToString type_ =
    case type_ of
        Normal -> "ノーマル"
        Fire -> "ほのお"
        Water -> "みず"
        Grass -> "くさ"
        Electric-> "でんき"
        Ice -> "こおり"
        Fighting -> "かくとう"
        Poison -> "どく"
        Ground -> "じめん"
        Flying-> "ひこう"
        Psychic -> "エスパー"
        Bug -> "むし"
        Rock -> "いわ"
        Ghost -> "ゴースト"
        Dragon-> "ドラゴン"
        Dark -> "あく"
        Steel -> "はがね"
        Fairy-> "フェアリー"


typeOfString : String -> Type
typeOfString t = case t of
    "normal" -> Normal
    "fire" -> Fire 
    "water" -> Water 
    "grass" -> Grass 
    "electric" -> Electric
    "ice" -> Ice 
    "fighting" -> Fighting 
    "poison" -> Poison 
    "ground" -> Ground 
    "flying" -> Flying
    "psychic" -> Psychic 
    "bug" -> Bug 
    "rock" -> Rock 
    "ghost" -> Ghost 
    "dragon" -> Dragon
    "dark" -> Dark 
    "steel" -> Steel 
    "fairy" -> Fairy
    _ -> Normal


statParams : List String
statParams =
        [ "HP"
        , "攻撃"
        , "防御"
        , "特攻"
        , "特防"
        , "素早さ"
        ]

calcStatus : Int -> Int -> Int -> Int -> Int -> Float -> Int
calcStatus level idx stat ind eff cmc =
    if idx == 0 then
        (stat * 2 + ind + eff // 4) * level // 100 + level + 10
    else
        (stat * 2 + ind + eff // 4) * level // 100 + 5
        |> toFloat
        |> (*) cmc
        |> floor

typeDecoder : Decoder Type
typeDecoder =
    Decode.field
        "name"
        ( Decode.string
        |> Decode.andThen
            (typeOfString >> Decode.succeed)
        )

pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    Decode.map7 Pokemon
        (Decode.field "id" Decode.int )
        (Decode.field "name" Decode.string )
        (Decode.field "sprites" (Decode.field "front_default" Decode.string))
        (Decode.field "types" (Decode.list (Decode.field "type" typeDecoder)))
        (Decode.field "stats" (Decode.list (Decode.field "base_stat" Decode.int)))
        (List.repeat 6 0 |> Decode.succeed)
        (List.repeat 6 0 |> Decode.succeed)


pokeJpDecoder : Decoder PokeJp
pokeJpDecoder =
    Decode.map2 PokeJp
        (Decode.field "no" Decode.int)
        (Decode.field "name" Decode.string)

pokeJpsDecoder : Decoder (List PokeJp)
pokeJpsDecoder = Decode.list pokeJpDecoder