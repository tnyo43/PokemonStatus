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
    { inputNo : String
    , inputName : String
    , pokemon : Maybe Pokemon
    , data : Dict String PokeJp
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" Nothing Dict.empty
    , Http.get
        { url = urlJapanese
        , expect = Http.expectJson JapaneseData pokeJpsDecoder } )


-- UPDATE

type Msg
    = UpdateNameInput String
    | GetNameData
    | NewData (Result Http.Error Pokemon)
    | JapaneseData (Result Http.Error (List PokeJp))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewData res ->
            case res of
                Ok poke -> ( { model | pokemon = Just poke }, Cmd.none )
                Err e -> ( { model | pokemon = Nothing }, Cmd.none )

        GetNameData ->
            case Dict.get model.inputName model.data of
                Just poke ->
                    ( model
                    , Http.get
                            { url = urlStats ++ (String.fromInt poke.no)
                            , expect = Http.expectJson NewData pokemonDecoder })

                Nothing -> ( { model | pokemon = Nothing }, Cmd.none )

        UpdateNameInput txt ->
            ( { model | inputName = txt }, Cmd.none )

        JapaneseData res ->
            case res of
                Ok lst -> 
                    let
                        dict = List.foldl
                                (\p -> \acc -> Dict.insert p.name p acc)
                                Dict.empty
                                lst
                    in
                    ( { model | data = dict }, Cmd.none )
                Err e -> ( { model | data = Dict.empty }, Cmd.none )


-- VIEW

showName : Pokemon -> Html msg
showName pokemon = text (String.fromInt pokemon.no ++ " : " ++ pokemon.name)

showImage : String -> Html msg
showImage url = img [ src url, width 200 ] []

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
            , showImage pokemon.imgUrl
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
            [ text "名前"
            , input [ placeholder "フシギダネ", onInput UpdateNameInput ] []
            , button [ onClick GetNameData ] [ text "検索" ]
            ]
        , div [] (showPokemon model.pokemon)
        ]

