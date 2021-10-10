
module Model exposing (..)

import Json.Decode exposing (Decoder, field, string, int, float, bool, list, map, map2, map4, map7, map8, nullable, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List exposing (concat, drop, head)


-- MODEL/TYPES/UTIL

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
    , probability : Maybe Probability
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
    

-- MODEL/TYPES/UTIL

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

defaultLastPlay : LastPlay
defaultLastPlay =
    { text = ""
    , probability = Nothing
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
            case situation.lastPlay.probability of
                Just probability ->
                    if isHome competitor
                        then probability.homeWinPercentage 
                        else probability.awayWinPercentage
                Nothing ->
                    0.5    
        Nothing ->
            0.5 -- TODO

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
            case situation.lastPlay.probability of
                Just probability ->
                    1.0 * (1 - (abs (probability.homeWinPercentage - probability.awayWinPercentage)))
                Nothing ->
                    0.5
        Nothing ->
            0.5

-- TODO
getRankedScore : Competition -> Float
getRankedScore competition = 1.0

-- Returns True is competitor is the home team
isHome : Competitor -> Bool
isHome competitor = competitor.homeAway == "home"

-- Return the second item of a list
second : List a -> Maybe a
second xs = head (drop 1 xs)


-- MODEL/DECODE

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
    succeed Team
        |> required "abbreviation" string
        |> required "color" string
        |> optional "alternateColor" string "000000"
        |> required "logo" string

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
        |> optional "lastPlay" lastPlayDecoder defaultLastPlay
        |> optional "down" int 0
        |> optional "yardLine" int 0
        |> optional "distance" int 0
        |> optional "possessionText" (map Just string) Nothing
        |> optional "isRedZone" bool False
        |> optional "homeTimeouts" int 0
        |> optional "awayTimeouts" int 0
        |> optional "possession" (map Just string) Nothing

lastPlayDecoder : Decoder LastPlay
lastPlayDecoder =
    succeed LastPlay
        |> required "text" string
        |> optional "probability" (map Just probabilityDecoder) Nothing

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
