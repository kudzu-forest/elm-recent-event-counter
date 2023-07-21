module RecentEventCounter exposing
    ( RecentEventCounter
    , create
    , increment
    , subscribe
    , stop, start, toggle
    , resetCounter, resetWhole
    , count, isMoving, passedMillis
    )

{-| This module provides functionality of counting some events which happened in user-determined certain time range.
You can stop and restart the time-induced change as you like.
Useful for scorering typing-practice game etc.


# Types

@docs RecentEventCounter


# Creation

@docs create


# Modification


## Incrementing event count

@docs increment


## Change over time

@docs subscribe


## Stop & Restart

@docs stop, start, toggle


## Discarding the content

@docs resetCounter, resetWhole


# Query

@docs count, isMoving, passedMillis

-}

import Browser.Events
import Queue exposing (Queue)
import Time


type alias Model =
    { queue : Queue Float
    , duration : Float
    , count : Int
    , moving : Bool
    , passed : Float
    }


{-| An opaque type representating a counter for events having recently happend.
-}
type RecentEventCounter
    = RecentEventCounter Model


{-| Call this when the observed event happend.

    rec : RecentEventCounter
    rec = create { durationInMillis = 30000, isMoving = False}

    rec
        |> increment
        |> count
        --> 1

-}
increment : RecentEventCounter -> RecentEventCounter
increment (RecentEventCounter rec) =
    let
        newQueue =
            Queue.enqueue
                (rec.passed + rec.duration)
                rec.queue
    in
    RecentEventCounter
        { queue = newQueue
        , duration = rec.duration
        , count = rec.count + 1
        , moving = rec.moving
        , passed = rec.passed
        }


{-| Make subscription for the time-induced change of RecentEventCounter. If the timer is moving, the passed milliseconds is enlarged each frame. Otherwise nothing changes. If the oldest event memorized expires its lifetime then the event is forgotton and the event count decreases by one.

        subscribe toMsg rec

-}
subscribe : (RecentEventCounter -> msg) -> RecentEventCounter -> Sub msg
subscribe toMsg ((RecentEventCounter rec) as old) =
    Browser.Events.onAnimationFrameDelta
        (\delta ->
            case Queue.head rec.queue of
                Nothing ->
                    toMsg old

                Just headEventDeadTime ->
                    if rec.moving then
                        let
                            oldestEventIsDead =
                                rec.passed
                                    + delta
                                    > headEventDeadTime
                        in
                        if oldestEventIsDead then
                            { queue = Queue.dequeue rec.queue
                            , duration = rec.duration
                            , count = rec.count - 1
                            , moving = True
                            , passed = rec.passed + delta
                            }
                                |> RecentEventCounter
                                |> toMsg

                        else
                            { queue = rec.queue
                            , duration = rec.duration
                            , count = rec.count
                            , moving = True
                            , passed = rec.passed + delta
                            }
                                |> RecentEventCounter
                                |> toMsg

                    else
                        toMsg old
        )


{-| Make the timer moves whichever it has been originally moving or not.
-}
start : RecentEventCounter -> RecentEventCounter
start (RecentEventCounter rec) =
    RecentEventCounter
        { queue = rec.queue
        , duration = rec.duration
        , count = rec.count
        , moving = True
        , passed = rec.passed
        }


{-| Make the timer stops whichever it has been originally moving or not.
-}
stop : RecentEventCounter -> RecentEventCounter
stop (RecentEventCounter rec) =
    RecentEventCounter
        { queue = rec.queue
        , duration = rec.duration
        , count = rec.count
        , moving = False
        , passed = rec.passed
        }


{-| If the timer is moving, then stops it. Else, restart the timer.
-}
toggle : RecentEventCounter -> RecentEventCounter
toggle (RecentEventCounter rec) =
    RecentEventCounter
        { queue = rec.queue
        , duration = rec.duration
        , count = rec.count
        , moving = not rec.moving
        , passed = rec.passed
        }


{-| Return how many times `increment` is called during the user-defined length of time by now.
-}
count : RecentEventCounter -> Int
count (RecentEventCounter rec) =
    rec.count


{-| Return `RecentEventCounter` with the event count forgotten,
while the passed time from initialization of the application is remained.
-}
resetCounter : RecentEventCounter -> RecentEventCounter
resetCounter (RecentEventCounter rec) =
    RecentEventCounter
        { queue = Queue.empty
        , duration = rec.duration
        , count = 0
        , moving = True
        , passed = rec.passed
        }


{-| Return `RecentEventCounter` in completely seme state as it was created.
-}
resetWhole : RecentEventCounter -> RecentEventCounter
resetWhole (RecentEventCounter rec) =
    RecentEventCounter
        { queue = Queue.empty
        , duration = rec.duration
        , count = 0
        , moving = True
        , passed = 0
        }


{-| create a `RecentEventCounter` from some configuration. Recode is used in order to prevent user from misunderstanding the time unit.
-}
create : { durationInMillis : Float, moving : Bool } -> RecentEventCounter
create { durationInMillis, moving } =
    RecentEventCounter
        { queue = Queue.empty
        , duration = durationInMillis
        , count = 0
        , moving = moving
        , passed = 0
        }


{-| Returns time in milliseconds passed with the timer moving.
-}
passedMillis : RecentEventCounter -> Float
passedMillis (RecentEventCounter rec) =
    rec.passed


{-| Returns if the timer is currently moving.
-}
isMoving : RecentEventCounter -> Bool
isMoving (RecentEventCounter rec) =
    rec.moving
