-- CFB Watchability Scoreboard
--
-- Matthew Swanson
--

module Main exposing (..)

import Browser
import Model exposing (..)
import Debug exposing (toString)
import Html exposing (Html, blockquote, code, div, h1, i, img, math, pre, strong, table, td, text, th, tr)
import Html.Attributes exposing (height, src, style, width)
import Http
import List exposing (concat, drop, head)
import ProbabilityBar exposing (..)


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type Model
    = Failure String String
    | Loading
    | Success Response


-- MODEL/JSON

init : () -> (Model, Cmd Msg)
init _ =
    let
        testing = False
    in
        ( Loading
        , Http.get
            { url = getUrl testing
            , expect = Http.expectJson GotResponse Model.responseDecoder
            }
        )

getUrl : Bool -> String
getUrl testing =
    if testing
        then "./../assets/scoreboard.json"
        else "https://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?limit=100&dates=20211009"


-- UPDATE

type Msg = GotResponse (Result Http.Error Response)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotResponse result ->
            case result of
                Ok response ->
                    (Success response, Cmd.none)
                Err errorCode ->
                    (errorToFailure errorCode, Cmd.none)

errorToFailure : Http.Error -> Model
errorToFailure error =
    case error of
        Http.BadUrl _ ->
            Failure "BadUrl" ""
        Http.Timeout ->
            Failure "Timeout" ""
        Http.NetworkError ->
            Failure "NetworkError" ""
        Http.BadStatus _ ->
            Failure "BadStatus" ""
        Http.BadBody msg ->
            Failure "BadBody" msg


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Failure errorTypeString errorMsg ->
            errorPage errorTypeString errorMsg
        Loading ->
            div pageBackground [strong [foregroundColor, monospace] [text "Loading..."]]
        Success response ->
            successPage response


-- VIEW/FAILURE

errorPage : String -> String -> Html Msg
errorPage errorTypeString errorMsg =
    div
        (pageBackground ++ [padding 12])
        [ strong
            [foregroundColor, monospace]
            [text "Oops, I'm having trouble showing the scoreboard. Here is the error:"]
        , div
            errorDivStyle
            [blockquote errorTextStyle [code [] [text errorMsg]]]
        ]


-- VIEW/SUCCESS

successPage : Response -> Html Msg
successPage response =
    div
        pageBackground
        ([h1 [foregroundColor, monospace] [text "CFB Watchability Scoreboard"]]
            ++ (List.map makeTable (concat response)))

makeTable : Competition -> Html Msg
makeTable competition =
    div
        [padding 12]
        [
            table tableStyle ([
                tr []
                    [ td [] [makeTeamTable competition]
                    , td [] [makeScoreTable competition]
                    , td [] [makeProbabilityTable competition]
                    , td [] [makeWatchabilityTable competition]
                    ]
            ])
        ]

makeTeamTable : Competition -> Html Msg
makeTeamTable competition =
    table []
        [ makeTeamTableHeader competition
        , table []
            [ makeTeamTableData competition (getFirstCompetitor competition)
            , makeTeamTableData competition (getSecondCompetitor competition)
            ]
        ]

makeTeamTableHeader : Competition -> Html Msg
makeTeamTableHeader competition =
    tr [] [
        th [foregroundColor] [text competition.status.type_.shortDetail]
    ]

makeTeamTableData : Competition -> Competitor -> Html Msg
makeTeamTableData competition competitor =
    tr []
        [ td ([secondaryBackground] ++ backgroundColorBorder) [img [src competitor.team.logo, height 50, width 50] []]
        , td
            [foregroundColor]
            [text (getCuratedRankString competitor.curatedRank), text " ", strong [] [text competitor.team.abbreviation]]
        ]

getCompetitorPrimaryHexCode : Competitor -> String
getCompetitorPrimaryHexCode competitor = "#" ++ competitor.team.color

getCompetitorSecondaryHexCode : Competitor -> String
getCompetitorSecondaryHexCode competitor = "#" ++ competitor.team.alternateColor

getCuratedRankString : CuratedRank -> String
getCuratedRankString curatedRank =
    case curatedRank of
        Just rank ->
            Debug.toString rank
        Nothing ->
            ""

makeScoreTable : Competition -> Html Msg
makeScoreTable competition =
    table scoreTableStyle
        [ tr [] (makeScoreTableHeader competition)
        , tr [] (makeScoreTableData competition (getFirstCompetitor competition))
        , tr [] (makeScoreTableData competition (getSecondCompetitor competition))
        ]

makeScoreTableHeader : Competition -> List (Html Msg)
makeScoreTableHeader competition =
    let
        linescoreData = getLineScoreText (getFirstCompetitor competition)
        linescores =
            List.map
                (\header -> th scoreTableHeaderStyle [text header])
                linescoreData
    in
        if linescoreData == [] then [] else linescores ++ [th scoreTableHeaderStyle [text "T"]]

makeScoreTableData : Competition -> Competitor -> List (Html Msg)
makeScoreTableData competition competitor =
    let
        linescores =
            List.map
                (\score -> td scoreTableStyle [text (Debug.toString score)])
                (Maybe.withDefault [] competitor.linescores)  
    in
        if linescores == []
            then []
            else (padLineScoreData linescores) ++ [th scoreTotalStyle [text (String.fromInt (getTotalScore competitor))]]

padLineScoreData : List (Html Msg) -> List (Html Msg)
padLineScoreData linescores =
    if List.length linescores < 4
        then padLineScoreData (linescores ++ [(td scoreTableStyle [])])
        else linescores

getLineScoreText : Competitor -> List String
getLineScoreText competitor =
    let
        labels = ["1", "2", "3", "4", "OT"]
    in
        if List.length (getLineScores competitor) <= 4
            then List.take 4 labels
            else labels

getLineScores : Competitor -> LineScores
getLineScores competitor = Maybe.withDefault [] (competitor.linescores)

makeProbabilityTable : Competition -> Html Msg
makeProbabilityTable competition =
    table []
        [ tr [] [makeProbabilityTableHeader competition]
        , tr [] [makeProbabilityTableData competition]
        ]

makeProbabilityTableHeader : Competition -> Html Msg
makeProbabilityTableHeader competition =
    th [foregroundColor] [i [] [text "P(win)"]]

makeProbabilityTableData : Competition -> Html Msg
makeProbabilityTableData competition =
    let
        firstCompetitor = getFirstCompetitor competition
        secondCompetitor = getSecondCompetitor competition
        
        firstWinPercentage = getWinPercentage competition firstCompetitor
        secondWinPercentage = getWinPercentage competition secondCompetitor
        
        firstCompetitorHexColor = getCompetitorPrimaryHexCode firstCompetitor
        secondCompetitorHexColor = getCompetitorPrimaryHexCode secondCompetitor
        
        o = String.fromInt (0 + 2)
        w = 256
        h = 32
        r = 16
        
        barStyle =
            { origin = o
            , width = w
            , height = h
            , p_1 = firstWinPercentage
            , p_2 = secondWinPercentage
            , color_1 = firstCompetitorHexColor
            , color_2 = secondCompetitorHexColor
            , outlineColor = foregroundColorHexString
            }
    in
        probabilityBar barStyle

makeWatchabilityTable : Competition -> Html Msg
makeWatchabilityTable competition =
    table []
        [ tr [] [makeWatchabilityTableHeader competition]
        , tr [] [makeWatchabilityTableData competition]
        ]

makeWatchabilityTableHeader : Competition -> Html Msg
makeWatchabilityTableHeader competition =
    th [foregroundColor] [text "Watchability Score"]

makeWatchabilityTableData : Competition -> Html Msg
makeWatchabilityTableData competition =
    th [foregroundColor] [text (Debug.toString (getWatchabilityScore competition))]


-- VIEW/STYLE

tableStyle : List (Html.Attribute Msg)
tableStyle = 
    [ style "border" ("1px solid " ++ alternateForegroundColorHexString)
    , style "border-radius" "5px"
    , style "width" "800px"
    , monospace
    , backgroundColor
    , padding 5
    ]

monospace : Html.Attribute Msg
monospace = style "font-family" "Consolas, monaco, monospace"

padding : Int -> Html.Attribute Msg
padding px = style "padding" ((String.fromInt px) ++ "px")

foregroundColor : Html.Attribute Msg
foregroundColor = style "color" foregroundColorHexString

alternateForegroundColor : Html.Attribute Msg
alternateForegroundColor = style "color" alternateForegroundColorHexString

errorTextColor : Html.Attribute Msg
errorTextColor = style "color" errorTextColorHexString

secondaryBackground : Html.Attribute Msg
secondaryBackground = style "background-color" foregroundColorHexString

backgroundColor : Html.Attribute Msg
backgroundColor = style "background-color" backgroundColorHexString

alternateBackgroundColor : Html.Attribute Msg
alternateBackgroundColor = style "background-color" alternateBackgroundColorHexString

backgroundColorBorder : List (Html.Attribute Msg)
backgroundColorBorder =
    [ style "border" ("2px solid " ++ backgroundColorHexString)
    , style "border-radius" "5px"
    ]

pageBackground : List (Html.Attribute Msg)
pageBackground =
    [ style "padding" "0"
    , style "margin" "0"
    , style "top" "0"
    , style "left" "0"
    , backgroundColor
    ]

scoreTableHeaderStyle : List (Html.Attribute Msg)
scoreTableHeaderStyle =
    [ style "height" "25px"
    , padding 5
    ]

scoreTableStyle : List (Html.Attribute Msg)
scoreTableStyle =
    [ style "height" "50px"
    , foregroundColor
    , padding 5
    ]

scoreTotalStyle : List (Html.Attribute Msg)
scoreTotalStyle =
    [ foregroundColor
    ]

errorDivStyle : List (Html.Attribute Msg)
errorDivStyle =
    [ padding 12 ]

errorTextStyle : List (Html.Attribute Msg)
errorTextStyle =
    [errorTextColor]


-- VIEW/STYLE/COLOR

foregroundColorHexString : String
foregroundColorHexString = "#f0f0f0"

alternateForegroundColorHexString : String
alternateForegroundColorHexString = "#7f7f7f"

backgroundColorHexString : String
backgroundColorHexString = "#1f1f1f"

alternateBackgroundColorHexString : String
alternateBackgroundColorHexString = "#505050"

errorTextColorHexString : String
errorTextColorHexString = "#ff7f7f"
