module Data exposing (..)

import Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Dict exposing (Dict)


{-| Represents the statistics as they come from the wrecker-ui server
-}
type alias Stats =
    { meanTime : Float
    , successHits : Int
    , serverErrorHits : Int
    , maxTime : Float
    , minTime : Float
    , userErrorHits : Int
    , totalTime : Float
    , failedHits : Int
    , variance : Float
    , hits : Int
    , quantile95 : Float
    }


{-| A GroupSet is a groupping for RunGroup. They are commonly used to test a particular
test hypothesis, where many consecutive runs are needed for comparison.
-}
type alias GroupSet =
    { id : Int
    , name : String
    , description : String
    , created : Date.Date
    }


{-| Contains the info for a run group (a consecutive list of test runs using the same workflow)
-}
type alias RunGroup =
    { id : Int
    , groupSetId : Int
    , title : String
    , notes : String
    , concurrencyStart : Int
    , concurrencyTarget : Int
    , rampupStep : Int
    , runKeepTime : Int
    , created : Date.Date
    }


{-| Contains the basic info for a Run done using wrecker
-}
type alias RunInfo =
    { created : Date.Date
    , concurrency : Int
    , runGroupId : Int
    , id : Int
    , match : String
    }


{-| A run is a collection of pages that were tested, the load stats
and also the basic information related to the run itself.
-}
type alias Run =
    { stats : Stats
    , run : RunInfo
    }


{-| A page is the combination of a url and a run id. It contains the
statistics related to loading such url
-}
type alias Page =
    { url : String
    , runId : Int
    , stats : Stats
    }


type alias Results =
    { runs : List Run
    , pages : Dict String (List Int)
    , runGroups : List RunGroup
    , availableGroupSets : List GroupSet
    }



---------------------------------------------
--- JSON Decoding
---------------------------------------------


decodeGroupSet : Decode.Decoder GroupSet
decodeGroupSet =
    Pipeline.decode GroupSet
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "created" (Decode.string |> Decode.andThen decodeDateTime)


decodeRunGroup : Decode.Decoder RunGroup
decodeRunGroup =
    Pipeline.decode RunGroup
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "groupSetId" Decode.int
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "notes" Decode.string
        |> Pipeline.required "concurrencyStart" Decode.int
        |> Pipeline.required "concurrencyTarget" Decode.int
        |> Pipeline.required "rampupStep" Decode.int
        |> Pipeline.required "runKeepTime" Decode.int
        |> Pipeline.required "created" (Decode.string |> Decode.andThen decodeDateTime)


decodeRun : Decode.Decoder Run
decodeRun =
    Pipeline.decode Run
        |> Pipeline.required "stats" decodeStats
        |> Pipeline.required "run" decodeRunInfo


decodePage : Decode.Decoder Page
decodePage =
    Pipeline.decode Page
        |> Pipeline.required "url" Decode.string
        |> Pipeline.required "runId" Decode.int
        |> Pipeline.custom decodeStats


decodeStats : Decode.Decoder Stats
decodeStats =
    Pipeline.decode Stats
        |> Pipeline.required "meanTime" Decode.float
        |> Pipeline.required "successHits" Decode.int
        |> Pipeline.required "serverErrorHits" Decode.int
        |> Pipeline.required "maxTime" Decode.float
        |> Pipeline.required "minTime" Decode.float
        |> Pipeline.required "userErrorHits" Decode.int
        |> Pipeline.required "totalTime" Decode.float
        |> Pipeline.required "failedHits" Decode.int
        |> Pipeline.required "variance" Decode.float
        |> Pipeline.required "hits" Decode.int
        |> Pipeline.required "quantile95" Decode.float


decodeRunInfo : Decode.Decoder RunInfo
decodeRunInfo =
    Pipeline.decode RunInfo
        |> Pipeline.required "created" (Decode.string |> Decode.andThen decodeDateTime)
        |> Pipeline.required "concurrency" Decode.int
        |> Pipeline.required "runGroupId" Decode.int
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "match" Decode.string


decodeResults : Decode.Decoder Results
decodeResults =
    Pipeline.decode Results
        |> Pipeline.required "runs" (Decode.list decodeRun)
        |> Pipeline.required "pages" (Decode.dict (Decode.list Decode.int))
        |> Pipeline.required "runGroups" (Decode.list decodeRunGroup)
        |> Pipeline.required "availableGroupSets" (Decode.list decodeGroupSet)


decodeDateTime : String -> Decode.Decoder Date.Date
decodeDateTime datetimeString =
    case Date.fromString datetimeString of
        Err e ->
            Decode.fail e

        Ok date ->
            Decode.succeed date
