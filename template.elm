module TimeZone exposing
    ( version
    , getZone, Error(..)
    , zones
    , ZONE_IDS
    )

{-| This library provides time zone data from the `VERSION` release of the IANA
Time Zone Database.

This edition of `timezone-data` includes _all_ time zones found in [eggert/tz](https://github.com/eggert/tz).
When duplicates are found between `Zone`s and `Link`s, the `Zone` is preferred.

This edition aims to be maximally compatible with time zones returned by
[Intl](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/resolvedOptions#timezone),
[which is how the Elm kernel retrieves the time zone](https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L44).

Unless you know you need full support for one of the deprecated or removed
zones from [eggert/tz](https://github.com/eggert/tz)'s `backward` or `backzone` files, you probably want
[justinmimbs/timezone-data](https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/) instead of this package.

@docs version


## Local zone

@docs getZone, Error


## Zones

@docs zones

---

Each unevaluated zone is named after its zone name (e.g.
`America/New_York`), where slashes are replaced by `__`, dashes are replaced
by `_`, and the name is lowercased. For example, `America/Port-au-Prince`
becomes `america__port_au_prince`.

@docs ZONE_IDS

-}

import Dict exposing (Dict)
import Task exposing (Task)
import Time exposing (Month(..), Weekday(..))
import TimeZone.Specification exposing (Clock(..), DateTime, DayOfMonth(..), Rule, Zone, ZoneRules(..), ZoneState)


{-| What release of the IANA Time Zone Database is this data from?
-}
version : String
version =
    "VERSION"


minYear : Int
minYear =
    MIN_YEAR


maxYear : Int
maxYear =
    MAX_YEAR


fromSpecification : Zone -> Time.Zone
fromSpecification zone =
    let
        ( descending, bottom ) =
            zone |> TimeZone.Specification.toOffsets minYear maxYear
    in
    Time.customZone bottom descending


{-| Represents an error that may occur when trying to get the local zone.
-}
type Error
    = NoZoneName
    | NoDataForZoneName String


{-| Try to get the local time zone. If the task succeeds, then you get the zone
name along with the `Time.Zone`.
-}
getZone : Task Error ( String, Time.Zone )
getZone =
    Time.getZoneName
        |> Task.andThen
            (\nameOrOffset ->
                case nameOrOffset of
                    Time.Name zoneName ->
                        case Dict.get zoneName zones of
                            Just zone ->
                                Task.succeed ( zoneName, zone () )

                            Nothing ->
                                Task.fail (NoDataForZoneName zoneName)

                    Time.Offset _ ->
                        Task.fail NoZoneName
            )


{-| You can look up an unevaluated zone by its zone name in the `zones` dictionary.

    import Dict
    import TimeZone exposing (zones, america__new_york)


    Dict.get "America/New_York" zones

    -- Just america__new_york

-}
zones : Dict String (() -> Time.Zone)
zones =
    [ ZONE_NAME_ID_PAIRS
    ]
        |> Dict.fromList
