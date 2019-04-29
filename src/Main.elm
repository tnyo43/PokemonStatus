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

showName : Maybe Pokemon -> Html msg
showName p =
    case p of
        Just pokemon -> text (String.fromInt pokemon.no ++ " : " ++ pokemon.name)
        Nothing -> text "no data"

showTypes : Maybe Pokemon -> Html msg
showTypes p =
    case p of
        Just pokemon ->
            ul
                []
                ( List.map
                    ( \t -> li [] [ text ( typeToString t ) ] ) 
                    pokemon.types
                )
        Nothing -> text ""

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
        , div
            []
            [ showName model.pokemon
            , showTypes model.pokemon ]
        ]

