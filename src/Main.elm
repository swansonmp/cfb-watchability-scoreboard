-- CFB Watchability Scoreboard
--
-- Matthew Swanson
-- 2021-10-03
--

module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, div, h1, i, img, math, pre, table, td, text, th, tr)
import Html.Attributes exposing (height, src, style, width)
import Http
import Json.Decode exposing (Decoder, field, string, int, float, bool, list, map, map2, map4, map7, map8, nullable, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List exposing (concat, drop, head)


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
    }

type alias Competitors = List Competitor

type alias Competitor =
    { id : String
    , order : Int
    , homeAway : String
    , team : Team
    , score : String
    , linescores : LineScores
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
    , possessionText : String
    , isRedZone : Bool
    , homeTimeouts : Int
    , awayTimeouts : Int
    , possession : String
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

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard"
      , expect = Http.expectJson GotResponse responseDecoder
      }
  )

responseDecoder : Decoder Response
responseDecoder = field "events" eventsDecoder

eventsDecoder : Decoder Events
eventsDecoder = list (field "competitions" (list competitionDecoder))

competitionDecoder : Decoder Competition
competitionDecoder = succeed Competition
                         |> required "date" string
                         |> required "neutralSite" bool
                         |> required "conferenceCompetition" bool
                         |> required "competitors" competitorsDecoder
                         |> hardcoded Nothing --|> required "situation" (nullable situationDecoder)
                         |> required "status" statusDecoder
                         |> required "broadcasts" broadcastsDecoder

competitorsDecoder : Decoder Competitors
competitorsDecoder = list competitorDecoder

competitorDecoder : Decoder Competitor
competitorDecoder = map8 Competitor
                        (field "id" string)
                        (field "order" int)
                        (field "homeAway" string)
                        (field "team" teamDecoder)
                        (field "score" string)
                        (field "linescores" lineScoresDecoder)
                        (field "curatedRank" curatedRankDecoder)
                        (field "records" recordsDecoder)
    
teamDecoder : Decoder Team
teamDecoder = map4 Team
                  (field "abbreviation" string)
                  (field "color" string)
                  (field "alternateColor" string)
                  (field "logo" string)

lineScoresDecoder : Decoder (List Int)
lineScoresDecoder = list (field "value" int)

curatedRankDecoder : Decoder CuratedRank
curatedRankDecoder = map (\rank -> if rank == 99 then Nothing else Just rank) (field "current" int)

recordsDecoder : Decoder Records
recordsDecoder = list recordDecoder

recordDecoder : Decoder Record
recordDecoder = map2 Record
                    (field "type" string)
                    (field "summary" string)

situationDecoder : Decoder Situation
situationDecoder = succeed Situation
                       |> required "lastPlay" lastPlayDecoder
                       |> required "down" int
                       |> required "yardLine" int
                       |> required "distance" int
                       |> required "possessionText" string
                       |> required "isRedZone" bool
                       |> required "homeTimeouts" int
                       |> required "awayTimeouts" int
                       |> required "possession" string

lastPlayDecoder : Decoder LastPlay
lastPlayDecoder = map2 LastPlay
                      (field "text" string)
                      (field "probability" probabilityDecoder)

probabilityDecoder : Decoder Probability
probabilityDecoder = map4 Probability
                         (field "tiePercentage" float)
                         (field "homeWinPercentage" float)
                         (field "awayWinPercentage" float)
                         (field "secondsLeft" int)

statusDecoder : Decoder Status
statusDecoder = map4 Status
                    (field "clock" int)
                    (field "displayClock" string)
                    (field "period" int)
                    (field "type" statusTypeDecoder)

statusTypeDecoder : Decoder StatusType
statusTypeDecoder = map2 StatusType
                        (field "name" string)
                        (field "shortDetail" string)

broadcastsDecoder : Decoder Broadcasts
broadcastsDecoder = list broadcastDecoder

broadcastDecoder : Decoder Broadcast
broadcastDecoder = map2 Broadcast
                       (field "market" string)
                       (field "names" (list string))


-- UPDATE

type Msg
  = GotResponse (Result Http.Error Response)


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
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Failure errorTypeString errorMsg ->
      text errorMsg
    Loading ->
      text "Loading..."
    Success response ->
      div [] (
        [h1 [] [text "CFB Watchability Scoreboard"]] ++
        (List.map makeTable (concat response))
      )

makeTable : Competition -> Html Msg
makeTable competition =
  let
    firstCompetitor = getFirstCompetitor competition
    secondCompetitor = Maybe.withDefault defaultCompetitor (second competition.competitors)
  in
    div [] [
      table tableStyle ([
        makeHeaderRow competition,
        makeCompetitorRow competition firstCompetitor,
        makeCompetitorRow competition secondCompetitor
      ])
    ]

getFirstCompetitor : Competition -> Competitor
getFirstCompetitor competition = Maybe.withDefault defaultCompetitor (head competition.competitors)

tableStyle : List (Html.Attribute Msg)
tableStyle = 
  [ style "border" ("1px solid " ++ tableBorderColor)
  , style "width" "800px"
  ]

tableBorderColor : String
tableBorderColor = "#dddddd"

makeHeaderRow : Competition -> Html Msg
makeHeaderRow competition =
  tr [] (
    [th [] [text competition.status.type_.shortDetail],
    th [] []] ++
    (getLineScoreHeaders competition) ++
    [th [] [text "T"],
    th [] [i [] [text "P(win)"]],
    th [] [text "Watchability Score"]]
  )

getLineScoreHeaders : Competition -> List (Html Msg)
getLineScoreHeaders competition =
  List.map (\header -> th [] [text header]) (List.take (List.length ((getFirstCompetitor competition).linescores)) ["1", "2", "3", "4", "OT"])

makeCompetitorRow : Competition -> Competitor -> Html Msg
makeCompetitorRow competition competitor = 
  tr [] (
    [td [] [img [src competitor.team.logo, height 50, width 50] [], text competitor.team.abbreviation],
    td [] []] ++
    (getLineScoreData competitor) ++
    [td [] [text (Debug.toString (getTotalScore competitor))],
    td [] [text (getWinPercentageString competition competitor)],
    td [] [text (Debug.toString (getWatchabilityScore competition))]]
  )

getLineScoreData : Competitor -> List (Html Msg)
getLineScoreData competitor = List.map (\score -> th [] [text (Debug.toString score)]) (competitor.linescores)

getTotalScore : Competitor -> Int
getTotalScore competitor = List.sum (competitor.linescores)

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

-- TODO
getWatchabilityScore : Competition -> Int
getWatchabilityScore competition = 0

-- Returns True is competitor is the home team
isHome : Competitor -> Bool
isHome competitor = competitor.homeAway == "home"

-- Return the second item of a list
second : List a -> Maybe a
second xs = head (drop 1 xs)

-- Competitor to be returned when only zero or one is provided
defaultCompetitor : Competitor
defaultCompetitor =
  { id = "0"
  , order = 2
  , homeAway = "home"
  , team = defaultTeam
  , score = "0"
  , linescores = []
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
