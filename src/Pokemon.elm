module Pokemon exposing (..)

import Json.Decode as Decode exposing (Decoder)


-- POKEMON
type Type =
    Normal | Fire | Water | Grass | Electric
    | Ice | Fighting | Poison | Ground | Flying
    | Psychic | Bug | Rock | Ghost | Dragon
    | Dark | Steel | Fairy

type alias Pokemon =
    { no : Int
    , name : String
    , types : List Type
    }

urlStats = "https://pokeapi.co/api/v2/pokemon/"


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
    Decode.map3 Pokemon
        (Decode.field "id" Decode.int )
        (Decode.field "name" Decode.string )
        (Decode.field "types" (Decode.list (Decode.field "type" typeDecoder)))
