module Main exposing (main)

import Browser
import Data exposing (dataAimlab)
import Dict.Extra as Dict
import Html exposing (Html, button, div, input, p, table, td, text, th, tr)
import Html.Attributes exposing (cols, rows, style, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Error, decodeString)
import Json.Decode.Pipeline as Decode
import List
import List.Extra as List





stats : List AimLab -> Maybe (AimLab -> String) -> Html Msg
stats result sort =
    div [ style "display" "flex" ]
        [ [ result |> List.length |> String.fromInt |> (++) "Number of entries "
          , result |> List.uniqueBy (\s -> s.taskName) |> List.length |> String.fromInt |> (++) "Different Tasks "
          , result |> lowestScore
          , result |> higestScore
          , result |> mostPlayed
          , result |> average
          , result |> averageX 10
          ]
            |> List.map (\s -> p [] [ text s ])
            |> div []
        , result
            |> (\ls ->
                    case sort of
                        Just s ->
                            List.sortBy s ls

                        Nothing ->
                            ls
               )
            |> List.reverse
            |> List.indexedMap line
            |> List.append
                [ tr []
                    [ th [ style "padding-left" "20px" ] [ button [] [ text "Number" ] ]
                    , th [ style "padding-left" "20px" ] [ button [] [ text "performanceClass" ] ]
                    , th [ style "padding-left" "20px" ] [ button [] [ text "taskName" ] ]
                    , th [ style "padding-left" "20px" ] [ button [] [ text "score" ] ]
                    , th [ style "padding-left" "20px" ] [ button [] [ text "Gun" ] ]
                    , th [ style "padding-left" "20px" ] [ button [ onClick (SortBy (Just (\s -> s.create_date))) ] [ text "create_date" ] ]
                    ]
                ]
            |> table []
        ]


line : Int -> AimLab -> Html Msg
line index aimLab =
    tr []
        [ td [ style "padding-left" "20px" ] [ text <| String.fromInt index ]
        , td [ style "padding-left" "20px" ] [ text aimLab.performanceClass ]
        , td [ style "padding-left" "20px" ] [ text aimLab.taskName ]
        , td [ style "padding-left" "20px" ] [ text <| String.fromInt aimLab.score ]
        , td [ style "padding-left" "20px" ] [ text aimLab.weaponName ]
        , td [ style "padding-left" "20px" ] [ text aimLab.create_date ]
        ]


filterAimLabs : List (Maybe (AimLab -> Bool)) -> List AimLab -> List AimLab
filterAimLabs filters aimlabs =
    filters |> List.foldl (\fn -> \result -> filterAimLab fn result) aimlabs


filterAimLab : Maybe (AimLab -> Bool) -> List AimLab -> List AimLab
filterAimLab filter aimlabs =
    case filter of
        Just fn ->
            List.filter fn aimlabs

        Nothing ->
            aimlabs


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ input [ onInput Convert ] []
        , p []
            [ case model.data of
                Ok result ->
                    case result of
                        [] ->
                            text "Waiting for your data"

                        ls ->
                            div []
                                [ toMenuButtons .performanceClass SetFilter ls
                                , toMenuButtons .taskName SetModeFilter (filterAimLab model.filterBy ls)
                                , toMenuButtons .weaponName SetGunFilter (filterAimLabs [ model.filterBy, model.modeFilterBy ] ls)
                                , stats (filterAimLabs [ model.filterBy, model.modeFilterBy, model.gunFilterBy ] ls |> List.sortBy .create_date) model.sortBy
                                ]

                Err err ->
                    text "something wrong, with your data"
            ]
        ]


type Msg
    = Convert String
    | SetFilter (Maybe String)
    | SetModeFilter (Maybe String)
    | SetGunFilter (Maybe String)
    | SortBy (Maybe (AimLab -> String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Convert data ->
            let
                _ = Debug.log "Data" data
            in
            ( { model | data = decodeString (Decode.list decoder) data }, Cmd.none )

        SetFilter query ->
            case query of
                Just q ->
                    ( { model | filterBy = Just (\s -> s.performanceClass == q), modeFilterBy = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | filterBy = Nothing, modeFilterBy = Nothing }, Cmd.none )

        SetModeFilter query ->
            case query of
                Just q ->
                    ( { model | modeFilterBy = Just (\s -> s.taskName == q) }, Cmd.none )

                Nothing ->
                    ( { model | modeFilterBy = Nothing }, Cmd.none )

        SetGunFilter query ->
            case query of
                Just q ->
                    ( { model | gunFilterBy = Just (\s -> s.taskName == q) }, Cmd.none )

                Nothing ->
                    ( { model | gunFilterBy = Nothing }, Cmd.none )

        SortBy fn ->
            ( { model | sortBy = fn }, Cmd.none )


type alias Model =
    { data : Result Error (List AimLab)
    , filterBy : Maybe (AimLab -> Bool)
    , modeFilterBy : Maybe (AimLab -> Bool)
    , gunFilterBy : Maybe (AimLab -> Bool)
    , sortBy : Maybe (AimLab -> String)
    }


initModel : Model
initModel =
    { data = decodeString (Decode.list decoder) dataAimlab
    , filterBy = Nothing
    , gunFilterBy = Nothing
    , modeFilterBy = Nothing
    , sortBy = Nothing
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias AimLab =
    { taskId : Int
    , klutch_id : String
    , create_date : String
    , taskName : String
    , score : Int
    , mode : Int
    , aimlab_map : Int
    , aimlab_version : String
    , weaponType : String
    , weaponName : String
    , performance : String
    , performanceClass : String
    }


decoder : Decoder AimLab
decoder =
    Decode.succeed AimLab
        |> Decode.required "taskId" Decode.int
        |> Decode.required "klutch_id" Decode.string
        |> Decode.required "create_date" Decode.string
        |> Decode.required "taskName" Decode.string
        |> Decode.required "score" Decode.int
        |> Decode.required "mode" Decode.int
        |> Decode.required "aimlab_map" Decode.int
        |> Decode.required "aimlab_version" Decode.string
        |> Decode.required "weaponType" Decode.string
        |> Decode.required "weaponName" Decode.string
        |> Decode.required "performance" Decode.string
        |> Decode.required "performanceClass" Decode.string


toMenuButtons : (AimLab -> String) -> (Maybe String -> Msg) -> List AimLab -> Html Msg
toMenuButtons fn action list =
    list
        |> List.map fn
        |> List.unique
        |> List.append [ "All" ]
        |> List.map (toMenuButton action)
        |> div []


toMenuButton : (Maybe String -> Msg) -> String -> Html Msg
toMenuButton action name =
    button
        [ onClick
            (action
                (if name == "All" then
                    Nothing

                 else
                    Just name
                )
            )
        ]
        [ text name ]

average : List AimLab -> String
average result =
    result
        |> List.map .score
        |> List.foldl (+) 0
        |> (\s -> s // List.length result)
        |> String.fromInt
        |> (++) "Your average score is "


averageX : Int -> List AimLab -> String
averageX x result =
    result
        |> List.sortBy .create_date
        |> List.take x
        |> List.map .score
        |> List.foldl (+) 0
        |> (\s -> s // (result |> List.take x |> List.length))
        |> String.fromInt
        |> (++) "Your average score for last 10 pratice "


mostPlayed : List AimLab -> String
mostPlayed result =
    result
        |> List.groupWhile (\s -> \s2 -> s.taskName == s2.taskName)
        |> List.sortBy (\( _, r ) -> List.length r)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( aim, _ ) -> aim.taskName)
        |> Maybe.withDefault ""
        |> (++) "Most Played "


higestScore : List AimLab -> String
higestScore result =
    result
        |> List.sortBy .score
        |> List.reverse
        |> List.head
        |> Maybe.map (\s -> "Highest Score " ++ s.taskName ++ " " ++ String.fromInt s.score)
        |> Maybe.withDefault ""


lowestScore : List AimLab -> String
lowestScore result =
    result |> List.sortBy .score |> List.head |> Maybe.map (\s -> "Lowest Score " ++ s.taskName ++ " " ++ String.fromInt s.score) |> Maybe.withDefault ""