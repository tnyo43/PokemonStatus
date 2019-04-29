port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Pokemon exposing (..)
import Task exposing (Task)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "ポケモンステータス"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }


-- MODEL
type alias Model =
    { input : String
    , pokemon : Maybe Pokemon
    , data : Dict String Pokemon
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Nothing Dict.empty , Cmd.none )


-- UPDATE

type Msg
    = UpdateInput String
    | GetData
    | NewData (Result Http.Error Pokemon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput txt ->
            ( { model | input = txt }, Cmd.none )

        GetData ->
            ( model
            , Http.get
                { url = urlStats ++ model.input
                , expect = Http.expectJson NewData pokemonDecoder })

        NewData res ->
            case res of
                Ok poke -> ( { model | pokemon = Just poke }, Cmd.none )
                Err e -> ( { model | pokemon = Nothing }, Cmd.none )

-- VIEW

showName : Pokemon -> Html msg
showName pokemon = text (String.fromInt pokemon.no ++ " : " ++ pokemon.name)

showTypes : List Type -> Html msg
showTypes types =
        ul
            []
            ( List.map
                ( \t -> li [] [ text ( typeToString t ) ] ) 
                types
            )

showStats : List Int -> Html msg
showStats stats =
        table
            []

            ( List.map2
                ( \p -> \s ->
                        tr
                            []
                            [ td [] [ text p ]
                            , td [] [ text "：" ]
                            , td [] [ text ( String.fromInt s ) ]] )
                Pokemon.statParams (List.reverse stats)
            )

showPokemon : Maybe Pokemon -> List (Html msg)
showPokemon p =
    case p of
        Just pokemon ->
            [ showName pokemon
            , showTypes pokemon.types
            , showStats pokemon.stats
            ]
        Nothing ->
            [ text "no data" ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "ポケモンステータス"]
        , div 
            []
            [ text "図鑑番号"
            , input [ placeholder "1", onInput UpdateInput ] []
            , button [ onClick GetData ] [ text "検索" ]
            ]
        , div [] (showPokemon model.pokemon)
        ]

