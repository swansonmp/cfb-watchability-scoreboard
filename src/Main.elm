-- CFB Watchability Scoreboard
--
-- Matthew Swanson
-- 2021-10-03
--

module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, blockquote, code, div, h1, i, img, math, pre, strong, table, td, text, th, tr)
import Html.Attributes exposing (height, src, style, width)
import Http
import Json.Decode exposing (Decoder, field, string, int, float, bool, list, map, map2, map4, map7, map8, nullable, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List exposing (concat, drop, head)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (color, fill, rx, ry, viewBox, x, y)


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


-- MODEL/DATA

type alias Response = Events

type alias Events = List (List Competition)

type alias Competition =
    { date : String
    , neutralSite : Bool
    , conferenceCompetition : Bool
    , competitors : Competitors
    , situation : Maybe Situation
    , status : Status
    , broadcasts : Broadcasts
    , odds : Maybe Odds
    }

type alias Competitors = List Competitor

type alias Competitor =
    { id : String
    , order : Int
    , homeAway : String
    , team : Team
    , score : String
    , linescores : Maybe LineScores
    , curatedRank : CuratedRank
    , records : Records
    }

type alias Team =
    { abbreviation : String
    , color : String
    , alternateColor : String
    , logo : String
    }

type alias LineScores = List Int

type alias CuratedRank = Maybe Int

type alias Records = List Record

type alias Record =
    { type_ : String
    , summary : String
    }

type alias Situation =
    { lastPlay : LastPlay
    , down : Int
    , yardLine : Int
    , distance : Int
    , possessionText : Maybe String
    , isRedZone : Bool
    , homeTimeouts : Int
    , awayTimeouts : Int
    , possession : Maybe String
    }

type alias LastPlay =
    { text : String
    , probability : Probability
    }

type alias Probability =
    { tiePercentage : Float
    , homeWinPercentage : Float
    , awayWinPercentage : Float
    , secondsLeft : Int
    }

type alias Status =
    { clock : Int
    , displayClock : String
    , period : Int
    , type_ : StatusType
    }

type alias StatusType =
    { name : String
    , shortDetail : String
    }

type alias Broadcasts = List Broadcast

type alias Broadcast =
    { market : String
    , names : List String
    }

type alias Odds =
    { details : String
    , overUnder : Float
    }
    

-- MODEL/DATA/UTIL

getFirstCompetitor : Competition -> Competitor
getFirstCompetitor competition = Maybe.withDefault defaultCompetitor (head competition.competitors)

getSecondCompetitor : Competition -> Competitor
getSecondCompetitor competition = Maybe.withDefault defaultCompetitor (second competition.competitors)

-- Competitor to be returned when only zero or one is provided
defaultCompetitor : Competitor
defaultCompetitor =
    { id = "0"
    , order = 2
    , homeAway = "home"
    , team = defaultTeam
    , score = "0"
    , linescores = Nothing
    , curatedRank = Nothing
    , records = []
    }

-- Team to be returned with the default competitor
defaultTeam : Team
defaultTeam =
    { abbreviation = "ERR"
    , color = "ffffff"
    , alternateColor = "000000"
    , logo = ""
    }

getTotalScore : Competitor -> Int
getTotalScore competitor =
  case competitor.linescores of
    Just linescores ->
      List.sum linescores
    Nothing ->
      0

getWinPercentageString : Competition -> Competitor -> String
getWinPercentageString competition competitor = Debug.toString (getWinPercentage competition competitor)

getWinPercentage : Competition -> Competitor -> Float
getWinPercentage competition competitor =
    case competition.situation of
        Just situation ->
            if isHome competitor then situation.lastPlay.probability.homeWinPercentage 
                                 else situation.lastPlay.probability.awayWinPercentage
        Nothing ->
            0.0 -- TODO

-- Get the watchability score from the competition.
--
-- Factors in...
--   * Remaining clock
--   * Win percentage differential
--   * Whether competitors are ranked
--
-- watchabilityScore = TODO
getWatchabilityScore : Competition -> Int
getWatchabilityScore competition =
    truncate (1 * List.product
        [ getClockScore competition
        , getProbabilityScore competition
        , getRankedScore competition
        ])

-- TODO
getClockScore : Competition -> Float
getClockScore competition = 0.08 * (toFloat (3600 - competition.status.clock))

-- TODO
getProbabilityScore : Competition -> Float
getProbabilityScore competition =
    case competition.situation of
        Just situation ->
            1.0 * (1 - (abs (situation.lastPlay.probability.homeWinPercentage - situation.lastPlay.probability.awayWinPercentage)))
        Nothing ->
            1.0

-- TODO
getRankedScore : Competition -> Float
getRankedScore competition = 1.0

-- Returns True is competitor is the home team
isHome : Competitor -> Bool
isHome competitor = competitor.homeAway == "home"

-- Return the second item of a list
second : List a -> Maybe a
second xs = head (drop 1 xs)


-- MODEL/JSON

init : () -> (Model, Cmd Msg)
init _ =
    let
        url = "http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard"
        --url = "./broken-scoreboard.json"
    in
        ( Loading
        , Http.get
            { url = url
            , expect = Http.expectJson GotResponse responseDecoder
            }
        )

responseDecoder : Decoder Response
responseDecoder = field "events" eventsDecoder

eventsDecoder : Decoder Events
eventsDecoder = list (field "competitions" (list competitionDecoder))

competitionDecoder : Decoder Competition
competitionDecoder =
    succeed Competition
        |> required "date" string
        |> required "neutralSite" bool
        |> required "conferenceCompetition" bool
        |> required "competitors" competitorsDecoder
        |> optional "situation" (map Just situationDecoder) Nothing
        |> required "status" statusDecoder
        |> required "broadcasts" broadcastsDecoder
        |> optional "odds" oddsDecoder Nothing

competitorsDecoder : Decoder Competitors
competitorsDecoder = list competitorDecoder

competitorDecoder : Decoder Competitor
competitorDecoder =
    succeed Competitor
        |> required "id" string
        |> required "order" int
        |> required "homeAway" string
        |> required "team" teamDecoder
        |> required "score" string
        |> optional "linescores" (map Just lineScoresDecoder) Nothing
        |> required "curatedRank" curatedRankDecoder
        |> required "records" recordsDecoder
    
teamDecoder : Decoder Team
teamDecoder =
    map4 Team
        (field "abbreviation" string)
        (field "color" string)
        (field "alternateColor" string)
        (field "logo" string)

lineScoresDecoder : Decoder (List Int)
lineScoresDecoder = list (field "value" int)

curatedRankDecoder : Decoder CuratedRank
curatedRankDecoder =
    map (\rank -> if rank == 99 then Nothing else Just rank) (field "current" int)

recordsDecoder : Decoder Records
recordsDecoder = list recordDecoder

recordDecoder : Decoder Record
recordDecoder =
    map2 Record
        (field "type" string)
        (field "summary" string)

situationDecoder : Decoder Situation
situationDecoder =
    succeed Situation
        |> required "lastPlay" lastPlayDecoder
        |> required "down" int
        |> required "yardLine" int
        |> required "distance" int
        |> optional "possessionText" (map Just string) Nothing
        |> required "isRedZone" bool
        |> required "homeTimeouts" int
        |> required "awayTimeouts" int
        |> optional "possession" (map Just string) Nothing

lastPlayDecoder : Decoder LastPlay
lastPlayDecoder =
    map2 LastPlay
        (field "text" string)
        (field "probability" probabilityDecoder)

probabilityDecoder : Decoder Probability
probabilityDecoder =
    map4 Probability
        (field "tiePercentage" float)
        (field "homeWinPercentage" float)
        (field "awayWinPercentage" float)
        (field "secondsLeft" int)

statusDecoder : Decoder Status
statusDecoder =
    map4 Status
        (field "clock" int)
        (field "displayClock" string)
        (field "period" int)
        (field "type" statusTypeDecoder)

statusTypeDecoder : Decoder StatusType
statusTypeDecoder =
    map2 StatusType
        (field "name" string)
        (field "shortDetail" string)

broadcastsDecoder : Decoder Broadcasts
broadcastsDecoder = list broadcastDecoder

broadcastDecoder : Decoder Broadcast
broadcastDecoder =
    map2 Broadcast
        (field "market" string)
        (field "names" (list string))

oddsDecoder : Decoder (Maybe Odds)
oddsDecoder =
    map
        head
        (list
            (map2 Odds
                (field "details" string)
                (field "overUnder" float)
            ))

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
            div pageBackground (
                [h1 [foregroundColor, monospace] [text "CFB Watchability Scoreboard"]] ++
                (List.map makeTable (concat response))
            )


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
    List.map (\header -> th scoreTableStyle [text header]) (getLineScoreText (getFirstCompetitor competition))

makeScoreTableData : Competition -> Competitor -> List (Html Msg)
makeScoreTableData competition competitor =
    List.map (\score -> th scoreTableStyle [text (Debug.toString score)]) (Maybe.withDefault [] competitor.linescores)  

getLineScoreText : Competitor -> List String
getLineScoreText competitor =
    List.take (List.length (getLineScores competitor)) ["1", "2", "3", "4", "OT"]

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
        homeWinPercentage = Maybe.withDefault 50 Nothing -- TODO
        awayWinPercentage = Maybe.withDefault 50 Nothing -- TODO
        tiePercentage = Maybe.withDefault 0 Nothing      -- TODO
    in
        svg
            [viewBox "0 0 200 25", width 200, height 25]
            [ rect [x "0", y "0", width 200, height 25, fill foregroundColorHexString, rx "15", ry "15"] []
            , rect [x "0", y "0", width 200, height 25, fill foregroundColorHexString, rx "15", ry "15"] []
            ]

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
    [ style "border" ("1px solid " ++ foregroundColorHexString)
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

errorTextColor : Html.Attribute Msg
errorTextColor = style "color" errorTextColorHexString

secondaryBackground : Html.Attribute Msg
secondaryBackground = style "background-color" foregroundColorHexString

backgroundColor : Html.Attribute Msg
backgroundColor = style "background-color" backgroundColorHexString

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

scoreTableStyle : List (Html.Attribute Msg)
scoreTableStyle =
    [ style "width" "25px"
    , style "height" "25px"
    , style "border" ("1px solid " ++ foregroundColorHexString)
    , style "border-radius" "5px"
    , style "border-collapse" "collapse"
    , foregroundColor
    ]

errorDivStyle : List (Html.Attribute Msg)
errorDivStyle =
    [ padding 12 ]

errorTextStyle : List (Html.Attribute Msg)
errorTextStyle =
    [errorTextColor]

-- VIEW/STYLE/COLOR

foregroundColorHexString : String
foregroundColorHexString = "#f8f8f8"

backgroundColorHexString : String
backgroundColorHexString = "#2a2a2a"

errorTextColorHexString : String
errorTextColorHexString = "#ff7f7f"
