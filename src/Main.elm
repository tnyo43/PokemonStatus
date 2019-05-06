port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Pokemon exposing (..)
import Task exposing (Task)
import Util exposing (..)


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
    { inputName : String
    , pokemon : Maybe Pokemon
    , data : Dict String Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Nothing Dict.empty
    , Http.get
        { url = urlJapanese
        , expect = Http.expectJson JapaneseData pokeJpsDecoder } )


-- UPDATE

type Msg
    = UpdateNameInput String
    | GetNameData
    | NewData (Result Http.Error Pokemon)
    | JapaneseData (Result Http.Error (List PokeJp))
    | UpdateIndiv Int String
    | UpdateEff Int String
    | UpdateNatureCorrect Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewData res ->
            case res of
                Ok poke -> ( { model | pokemon = Just poke }, Cmd.none )
                Err e -> ( { model | pokemon = Nothing }, Cmd.none )

        GetNameData ->
            case Dict.get model.inputName model.data of
                Just idx ->
                    ( model
                    , Http.get
                            { url = urlStats ++ (String.fromInt idx)
                            , expect = Http.expectJson NewData pokemonDecoder })

                Nothing -> ( { model | pokemon = Nothing }, Cmd.none )

        UpdateNameInput txt ->
            ( { model | inputName = txt }, Cmd.none )

        JapaneseData res ->
            case res of
                Ok lst ->
                    ( { model | data = Pokemon.getPokemonNameDict lst }, Cmd.none )
                Err e -> ( { model | data = Dict.empty }, Cmd.none )

        UpdateIndiv n txt ->
            case model.pokemon of
                Just poke ->
                    let
                        indiv_ =
                            case String.toInt txt of
                                Just x ->
                                    if x >= 0 && x <= 31 then
                                        case Util.updateList poke.indiv n x of
                                            Just lst -> lst
                                            Nothing -> poke.indiv
                                    else poke.indiv
                                Nothing -> poke.indiv
                        poke_ = Just { poke | indiv = indiv_ }
                    in
                    ( { model | pokemon = poke_ }, Cmd.none )
                Nothing -> ( model, Cmd.none )

        UpdateEff n txt ->
            case model.pokemon of
                Just poke ->
                    let
                        eff_ =
                            case String.toInt txt of
                                Just x ->
                                    if x >= 0 && x <= 252 then
                                        case Util.updateList poke.effort n x of
                                            Just lst -> lst
                                            Nothing -> poke.effort
                                    else poke.effort
                                Nothing -> poke.effort
                        poke_ = Just { poke | effort = eff_ }
                    in
                    ( { model | pokemon = poke_ }, Cmd.none )
                Nothing -> ( model, Cmd.none )

        UpdateNatureCorrect n ->
            case model.pokemon of
                Just poke ->
                    let
                        ncs = 
                            case Util.getFromList poke.ncs n of
                                Just nc ->
                                    nc
                                    |> Pokemon.nextNatureCorrect
                                    |> Util.updateList poke.ncs n
                                Nothing -> Nothing
                        poke_ =
                            case ncs of
                                Just ncs_ -> Just { poke | ncs = ncs_ }
                                Nothing -> Just poke
                    in
                    ( { model | pokemon = poke_ }, Cmd.none )
                Nothing -> ( model, Cmd.none )
-- VIEW

showName : Pokemon -> Html Msg
showName pokemon = text (String.fromInt pokemon.no ++ " : " ++ pokemon.name)

showImage : String -> Html Msg
showImage url = img [ src url, width 200 ] []

showTypes : List Type -> Html Msg
showTypes types =
        ul
            []
            ( List.map
                ( \t -> li [] [ text ( typeToString t ) ] ) 
                types
            )

showStats : Pokemon -> Html Msg
showStats pokemon = 

        table
            []
            (
                tr
                    []
                    [ td [] []
                    , td [] []
                    , td [] [ text "種族値" ]
                    , td [] [ text "個体値" ]
                    , td [] [ text "努力値" ]
                    , td [] [ text "性格補正" ]
                    , td [] [ text "実数値" ]]
                ::
                List.map5
                    ( \(i, p) s ind eff nc ->
                            tr
                                []
                                [ td [] [ text p ]
                                , td [] [ text "：" ]
                                , td [] [ text ( String.fromInt s ) ]
                                , td [] [ input
                                            [ onInput (UpdateIndiv i)
                                            , type_ "number"
                                            , String.fromInt ind |> value 
                                            ]
                                            []
                                        ]
                                , td [] [ input
                                            [ onInput (UpdateEff i)
                                            , type_ "number"
                                            , String.fromInt eff |> value 
                                            ]
                                            []
                                        ]
                                , if i == 0 then td [] []
                                  else
                                    td [] [ button
                                            [ onClick (UpdateNatureCorrect i)
                                            , style "color" (buttonColorNC nc)]
                                            [ Pokemon.stringOfNatureCorrect nc |> text ]
                                        ]
                                , td [] [ text (Pokemon.calcStatus 50 i s ind eff nc |> String.fromInt )]
                                ]
                    )
                    (List.indexedMap Tuple.pair Pokemon.statParams)
                    pokemon.stats
                    pokemon.indiv
                    pokemon.effort
                    pokemon.ncs
                ++
                [tr
                    []
                    [ td [] []
                    , td [] []
                    , td [] []
                    , td [] []
                    , if isValidEffort pokemon then td [] []
                      else td [ style "color" "red" ] [ text "努力値の合計は510まで" ]
                    , if isValidNatureCorrect pokemon then td [] []
                      else td [ style "color" "red" ] [ text "不正な補正の割り当て" ]
                    , td [] []
                    ]]
            )

buttonColorNC : NatureCorrect -> String
buttonColorNC nc =
    case nc of
        Increase -> "green"
        Decrease -> "red"
        Unchange -> ""


showPokemon : Maybe Pokemon -> List (Html Msg)
showPokemon p =
    case p of
        Just pokemon ->
            [ showName pokemon
            , showImage pokemon.imgUrl
            , showTypes pokemon.types
            , showStats pokemon
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

