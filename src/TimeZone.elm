module TimeZone exposing
    ( version
    , getZone, Error(..)
    , zones
    , africa__abidjan, africa__accra, africa__addis_ababa, africa__algiers, africa__asmara, africa__asmera, africa__bamako, africa__bangui, africa__banjul, africa__bissau, africa__blantyre, africa__brazzaville, africa__bujumbura, africa__cairo, africa__casablanca, africa__ceuta, africa__conakry, africa__dakar, africa__dar_es_salaam, africa__djibouti, africa__douala, africa__el_aaiun, africa__freetown, africa__gaborone, africa__harare, africa__johannesburg, africa__juba, africa__kampala, africa__khartoum, africa__kigali, africa__kinshasa, africa__lagos, africa__libreville, africa__lome, africa__luanda, africa__lubumbashi, africa__lusaka, africa__malabo, africa__maputo, africa__maseru, africa__mbabane, africa__mogadishu, africa__monrovia, africa__nairobi, africa__ndjamena, africa__niamey, africa__nouakchott, africa__ouagadougou, africa__porto_novo, africa__sao_tome, africa__timbuktu, africa__tripoli, africa__tunis, africa__windhoek, america__adak, america__anchorage, america__anguilla, america__antigua, america__araguaina, america__argentina__buenos_aires, america__argentina__catamarca, america__argentina__comodrivadavia, america__argentina__cordoba, america__argentina__jujuy, america__argentina__la_rioja, america__argentina__mendoza, america__argentina__rio_gallegos, america__argentina__salta, america__argentina__san_juan, america__argentina__san_luis, america__argentina__tucuman, america__argentina__ushuaia, america__aruba, america__asuncion, america__atikokan, america__atka, america__bahia, america__bahia_banderas, america__barbados, america__belem, america__belize, america__blanc_sablon, america__boa_vista, america__bogota, america__boise, america__buenos_aires, america__cambridge_bay, america__campo_grande, america__cancun, america__caracas, america__catamarca, america__cayenne, america__cayman, america__chicago, america__chihuahua, america__ciudad_juarez, america__coral_harbour, america__cordoba, america__costa_rica, america__coyhaique, america__creston, america__cuiaba, america__curacao, america__danmarkshavn, america__dawson, america__dawson_creek, america__denver, america__detroit, america__dominica, america__edmonton, america__eirunepe, america__el_salvador, america__ensenada, america__fort_nelson, america__fort_wayne, america__fortaleza, america__glace_bay, america__godthab, america__goose_bay, america__grand_turk, america__grenada, america__guadeloupe, america__guatemala, america__guayaquil, america__guyana, america__halifax, america__havana, america__hermosillo, america__indiana__indianapolis, america__indiana__knox, america__indiana__marengo, america__indiana__petersburg, america__indiana__tell_city, america__indiana__vevay, america__indiana__vincennes, america__indiana__winamac, america__indianapolis, america__inuvik, america__iqaluit, america__jamaica, america__jujuy, america__juneau, america__kentucky__louisville, america__kentucky__monticello, america__knox_in, america__kralendijk, america__la_paz, america__lima, america__los_angeles, america__louisville, america__lower_princes, america__maceio, america__managua, america__manaus, america__marigot, america__martinique, america__matamoros, america__mazatlan, america__mendoza, america__menominee, america__merida, america__metlakatla, america__mexico_city, america__miquelon, america__moncton, america__monterrey, america__montevideo, america__montreal, america__montserrat, america__nassau, america__new_york, america__nipigon, america__nome, america__noronha, america__north_dakota__beulah, america__north_dakota__center, america__north_dakota__new_salem, america__nuuk, america__ojinaga, america__panama, america__pangnirtung, america__paramaribo, america__phoenix, america__port_au_prince, america__port_of_spain, america__porto_acre, america__porto_velho, america__puerto_rico, america__punta_arenas, america__rainy_river, america__rankin_inlet, america__recife, america__regina, america__resolute, america__rio_branco, america__rosario, america__santa_isabel, america__santarem, america__santiago, america__santo_domingo, america__sao_paulo, america__scoresbysund, america__shiprock, america__sitka, america__st_barthelemy, america__st_johns, america__st_kitts, america__st_lucia, america__st_thomas, america__st_vincent, america__swift_current, america__tegucigalpa, america__thule, america__thunder_bay, america__tijuana, america__toronto, america__tortola, america__vancouver, america__virgin, america__whitehorse, america__winnipeg, america__yakutat, america__yellowknife, antarctica__casey, antarctica__davis, antarctica__dumontdurville, antarctica__macquarie, antarctica__mawson, antarctica__mcmurdo, antarctica__palmer, antarctica__rothera, antarctica__south_pole, antarctica__syowa, antarctica__troll, antarctica__vostok, arctic__longyearbyen, asia__aden, asia__almaty, asia__amman, asia__anadyr, asia__aqtau, asia__aqtobe, asia__ashgabat, asia__ashkhabad, asia__atyrau, asia__baghdad, asia__bahrain, asia__baku, asia__bangkok, asia__barnaul, asia__beirut, asia__bishkek, asia__brunei, asia__calcutta, asia__chita, asia__choibalsan, asia__chongqing, asia__chungking, asia__colombo, asia__dacca, asia__damascus, asia__dhaka, asia__dili, asia__dubai, asia__dushanbe, asia__famagusta, asia__gaza, asia__hanoi, asia__harbin, asia__hebron, asia__ho_chi_minh, asia__hong_kong, asia__hovd, asia__irkutsk, asia__istanbul, asia__jakarta, asia__jayapura, asia__jerusalem, asia__kabul, asia__kamchatka, asia__karachi, asia__kashgar, asia__kathmandu, asia__katmandu, asia__khandyga, asia__kolkata, asia__krasnoyarsk, asia__kuala_lumpur, asia__kuching, asia__kuwait, asia__macao, asia__macau, asia__magadan, asia__makassar, asia__manila, asia__muscat, asia__nicosia, asia__novokuznetsk, asia__novosibirsk, asia__omsk, asia__oral, asia__phnom_penh, asia__pontianak, asia__pyongyang, asia__qatar, asia__qostanay, asia__qyzylorda, asia__rangoon, asia__riyadh, asia__saigon, asia__sakhalin, asia__samarkand, asia__seoul, asia__shanghai, asia__singapore, asia__srednekolymsk, asia__taipei, asia__tashkent, asia__tbilisi, asia__tehran, asia__tel_aviv, asia__thimbu, asia__thimphu, asia__tokyo, asia__tomsk, asia__ujung_pandang, asia__ulaanbaatar, asia__ulan_bator, asia__urumqi, asia__ust_nera, asia__vientiane, asia__vladivostok, asia__yakutsk, asia__yangon, asia__yekaterinburg, asia__yerevan, atlantic__azores, atlantic__bermuda, atlantic__canary, atlantic__cape_verde, atlantic__faeroe, atlantic__faroe, atlantic__jan_mayen, atlantic__madeira, atlantic__reykjavik, atlantic__south_georgia, atlantic__st_helena, atlantic__stanley, australia__act, australia__adelaide, australia__brisbane, australia__broken_hill, australia__canberra, australia__currie, australia__darwin, australia__eucla, australia__hobart, australia__lhi, australia__lindeman, australia__lord_howe, australia__melbourne, australia__nsw, australia__north, australia__perth, australia__queensland, australia__south, australia__sydney, australia__tasmania, australia__victoria, australia__west, australia__yancowinna, brazil__acre, brazil__denoronha, brazil__east, brazil__west, cet, cst6cdt, canada__atlantic, canada__central, canada__eastern, canada__mountain, canada__newfoundland, canada__pacific, canada__saskatchewan, canada__yukon, chile__continental, chile__easterisland, cuba, eet, est, est5edt, egypt, eire, etc__gmt, etc__gmt_plus0, etc__gmt_plus1, etc__gmt_plus10, etc__gmt_plus11, etc__gmt_plus12, etc__gmt_plus2, etc__gmt_plus3, etc__gmt_plus4, etc__gmt_plus5, etc__gmt_plus6, etc__gmt_plus7, etc__gmt_plus8, etc__gmt_plus9, etc__gmt_0, etc__gmt_1, etc__gmt_10, etc__gmt_11, etc__gmt_12, etc__gmt_13, etc__gmt_14, etc__gmt_2, etc__gmt_3, etc__gmt_4, etc__gmt_5, etc__gmt_6, etc__gmt_7, etc__gmt_8, etc__gmt_9, etc__gmt0, etc__greenwich, etc__uct, etc__utc, etc__universal, etc__zulu, europe__amsterdam, europe__andorra, europe__astrakhan, europe__athens, europe__belfast, europe__belgrade, europe__berlin, europe__bratislava, europe__brussels, europe__bucharest, europe__budapest, europe__busingen, europe__chisinau, europe__copenhagen, europe__dublin, europe__gibraltar, europe__guernsey, europe__helsinki, europe__isle_of_man, europe__istanbul, europe__jersey, europe__kaliningrad, europe__kiev, europe__kirov, europe__kyiv, europe__lisbon, europe__ljubljana, europe__london, europe__luxembourg, europe__madrid, europe__malta, europe__mariehamn, europe__minsk, europe__monaco, europe__moscow, europe__nicosia, europe__oslo, europe__paris, europe__podgorica, europe__prague, europe__riga, europe__rome, europe__samara, europe__san_marino, europe__sarajevo, europe__saratov, europe__simferopol, europe__skopje, europe__sofia, europe__stockholm, europe__tallinn, europe__tirane, europe__tiraspol, europe__ulyanovsk, europe__uzhgorod, europe__vaduz, europe__vatican, europe__vienna, europe__vilnius, europe__volgograd, europe__warsaw, europe__zagreb, europe__zaporozhye, europe__zurich, gb, gb_eire, gmt, gmt_plus0, gmt_0, gmt0, greenwich, hst, hongkong, iceland, indian__antananarivo, indian__chagos, indian__christmas, indian__cocos, indian__comoro, indian__kerguelen, indian__mahe, indian__maldives, indian__mauritius, indian__mayotte, indian__reunion, iran, israel, jamaica, japan, kwajalein, libya, met, mst, mst7mdt, mexico__bajanorte, mexico__bajasur, mexico__general, nz, nz_chat, navajo, prc, pst8pdt, pacific__apia, pacific__auckland, pacific__bougainville, pacific__chatham, pacific__chuuk, pacific__easter, pacific__efate, pacific__enderbury, pacific__fakaofo, pacific__fiji, pacific__funafuti, pacific__galapagos, pacific__gambier, pacific__guadalcanal, pacific__guam, pacific__honolulu, pacific__johnston, pacific__kanton, pacific__kiritimati, pacific__kosrae, pacific__kwajalein, pacific__majuro, pacific__marquesas, pacific__midway, pacific__nauru, pacific__niue, pacific__norfolk, pacific__noumea, pacific__pago_pago, pacific__palau, pacific__pitcairn, pacific__pohnpei, pacific__ponape, pacific__port_moresby, pacific__rarotonga, pacific__saipan, pacific__samoa, pacific__tahiti, pacific__tarawa, pacific__tongatapu, pacific__truk, pacific__wake, pacific__wallis, pacific__yap, poland, portugal, roc, rok, singapore, turkey, uct, us__alaska, us__aleutian, us__arizona, us__central, us__east_indiana, us__eastern, us__hawaii, us__indiana_starke, us__michigan, us__mountain, us__pacific, us__samoa, utc, universal, w_su, wet, zulu
    )

{-| This library provides time zone data from the `2025b` release of the IANA
Time Zone Database.

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

@docs africa__abidjan, africa__accra, africa__addis_ababa, africa__algiers, africa__asmara, africa__asmera, africa__bamako, africa__bangui, africa__banjul, africa__bissau, africa__blantyre, africa__brazzaville, africa__bujumbura, africa__cairo, africa__casablanca, africa__ceuta, africa__conakry, africa__dakar, africa__dar_es_salaam, africa__djibouti, africa__douala, africa__el_aaiun, africa__freetown, africa__gaborone, africa__harare, africa__johannesburg, africa__juba, africa__kampala, africa__khartoum, africa__kigali, africa__kinshasa, africa__lagos, africa__libreville, africa__lome, africa__luanda, africa__lubumbashi, africa__lusaka, africa__malabo, africa__maputo, africa__maseru, africa__mbabane, africa__mogadishu, africa__monrovia, africa__nairobi, africa__ndjamena, africa__niamey, africa__nouakchott, africa__ouagadougou, africa__porto_novo, africa__sao_tome, africa__timbuktu, africa__tripoli, africa__tunis, africa__windhoek, america__adak, america__anchorage, america__anguilla, america__antigua, america__araguaina, america__argentina__buenos_aires, america__argentina__catamarca, america__argentina__comodrivadavia, america__argentina__cordoba, america__argentina__jujuy, america__argentina__la_rioja, america__argentina__mendoza, america__argentina__rio_gallegos, america__argentina__salta, america__argentina__san_juan, america__argentina__san_luis, america__argentina__tucuman, america__argentina__ushuaia, america__aruba, america__asuncion, america__atikokan, america__atka, america__bahia, america__bahia_banderas, america__barbados, america__belem, america__belize, america__blanc_sablon, america__boa_vista, america__bogota, america__boise, america__buenos_aires, america__cambridge_bay, america__campo_grande, america__cancun, america__caracas, america__catamarca, america__cayenne, america__cayman, america__chicago, america__chihuahua, america__ciudad_juarez, america__coral_harbour, america__cordoba, america__costa_rica, america__coyhaique, america__creston, america__cuiaba, america__curacao, america__danmarkshavn, america__dawson, america__dawson_creek, america__denver, america__detroit, america__dominica, america__edmonton, america__eirunepe, america__el_salvador, america__ensenada, america__fort_nelson, america__fort_wayne, america__fortaleza, america__glace_bay, america__godthab, america__goose_bay, america__grand_turk, america__grenada, america__guadeloupe, america__guatemala, america__guayaquil, america__guyana, america__halifax, america__havana, america__hermosillo, america__indiana__indianapolis, america__indiana__knox, america__indiana__marengo, america__indiana__petersburg, america__indiana__tell_city, america__indiana__vevay, america__indiana__vincennes, america__indiana__winamac, america__indianapolis, america__inuvik, america__iqaluit, america__jamaica, america__jujuy, america__juneau, america__kentucky__louisville, america__kentucky__monticello, america__knox_in, america__kralendijk, america__la_paz, america__lima, america__los_angeles, america__louisville, america__lower_princes, america__maceio, america__managua, america__manaus, america__marigot, america__martinique, america__matamoros, america__mazatlan, america__mendoza, america__menominee, america__merida, america__metlakatla, america__mexico_city, america__miquelon, america__moncton, america__monterrey, america__montevideo, america__montreal, america__montserrat, america__nassau, america__new_york, america__nipigon, america__nome, america__noronha, america__north_dakota__beulah, america__north_dakota__center, america__north_dakota__new_salem, america__nuuk, america__ojinaga, america__panama, america__pangnirtung, america__paramaribo, america__phoenix, america__port_au_prince, america__port_of_spain, america__porto_acre, america__porto_velho, america__puerto_rico, america__punta_arenas, america__rainy_river, america__rankin_inlet, america__recife, america__regina, america__resolute, america__rio_branco, america__rosario, america__santa_isabel, america__santarem, america__santiago, america__santo_domingo, america__sao_paulo, america__scoresbysund, america__shiprock, america__sitka, america__st_barthelemy, america__st_johns, america__st_kitts, america__st_lucia, america__st_thomas, america__st_vincent, america__swift_current, america__tegucigalpa, america__thule, america__thunder_bay, america__tijuana, america__toronto, america__tortola, america__vancouver, america__virgin, america__whitehorse, america__winnipeg, america__yakutat, america__yellowknife, antarctica__casey, antarctica__davis, antarctica__dumontdurville, antarctica__macquarie, antarctica__mawson, antarctica__mcmurdo, antarctica__palmer, antarctica__rothera, antarctica__south_pole, antarctica__syowa, antarctica__troll, antarctica__vostok, arctic__longyearbyen, asia__aden, asia__almaty, asia__amman, asia__anadyr, asia__aqtau, asia__aqtobe, asia__ashgabat, asia__ashkhabad, asia__atyrau, asia__baghdad, asia__bahrain, asia__baku, asia__bangkok, asia__barnaul, asia__beirut, asia__bishkek, asia__brunei, asia__calcutta, asia__chita, asia__choibalsan, asia__chongqing, asia__chungking, asia__colombo, asia__dacca, asia__damascus, asia__dhaka, asia__dili, asia__dubai, asia__dushanbe, asia__famagusta, asia__gaza, asia__hanoi, asia__harbin, asia__hebron, asia__ho_chi_minh, asia__hong_kong, asia__hovd, asia__irkutsk, asia__istanbul, asia__jakarta, asia__jayapura, asia__jerusalem, asia__kabul, asia__kamchatka, asia__karachi, asia__kashgar, asia__kathmandu, asia__katmandu, asia__khandyga, asia__kolkata, asia__krasnoyarsk, asia__kuala_lumpur, asia__kuching, asia__kuwait, asia__macao, asia__macau, asia__magadan, asia__makassar, asia__manila, asia__muscat, asia__nicosia, asia__novokuznetsk, asia__novosibirsk, asia__omsk, asia__oral, asia__phnom_penh, asia__pontianak, asia__pyongyang, asia__qatar, asia__qostanay, asia__qyzylorda, asia__rangoon, asia__riyadh, asia__saigon, asia__sakhalin, asia__samarkand, asia__seoul, asia__shanghai, asia__singapore, asia__srednekolymsk, asia__taipei, asia__tashkent, asia__tbilisi, asia__tehran, asia__tel_aviv, asia__thimbu, asia__thimphu, asia__tokyo, asia__tomsk, asia__ujung_pandang, asia__ulaanbaatar, asia__ulan_bator, asia__urumqi, asia__ust_nera, asia__vientiane, asia__vladivostok, asia__yakutsk, asia__yangon, asia__yekaterinburg, asia__yerevan, atlantic__azores, atlantic__bermuda, atlantic__canary, atlantic__cape_verde, atlantic__faeroe, atlantic__faroe, atlantic__jan_mayen, atlantic__madeira, atlantic__reykjavik, atlantic__south_georgia, atlantic__st_helena, atlantic__stanley, australia__act, australia__adelaide, australia__brisbane, australia__broken_hill, australia__canberra, australia__currie, australia__darwin, australia__eucla, australia__hobart, australia__lhi, australia__lindeman, australia__lord_howe, australia__melbourne, australia__nsw, australia__north, australia__perth, australia__queensland, australia__south, australia__sydney, australia__tasmania, australia__victoria, australia__west, australia__yancowinna, brazil__acre, brazil__denoronha, brazil__east, brazil__west, cet, cst6cdt, canada__atlantic, canada__central, canada__eastern, canada__mountain, canada__newfoundland, canada__pacific, canada__saskatchewan, canada__yukon, chile__continental, chile__easterisland, cuba, eet, est, est5edt, egypt, eire, etc__gmt, etc__gmt_plus0, etc__gmt_plus1, etc__gmt_plus10, etc__gmt_plus11, etc__gmt_plus12, etc__gmt_plus2, etc__gmt_plus3, etc__gmt_plus4, etc__gmt_plus5, etc__gmt_plus6, etc__gmt_plus7, etc__gmt_plus8, etc__gmt_plus9, etc__gmt_0, etc__gmt_1, etc__gmt_10, etc__gmt_11, etc__gmt_12, etc__gmt_13, etc__gmt_14, etc__gmt_2, etc__gmt_3, etc__gmt_4, etc__gmt_5, etc__gmt_6, etc__gmt_7, etc__gmt_8, etc__gmt_9, etc__gmt0, etc__greenwich, etc__uct, etc__utc, etc__universal, etc__zulu, europe__amsterdam, europe__andorra, europe__astrakhan, europe__athens, europe__belfast, europe__belgrade, europe__berlin, europe__bratislava, europe__brussels, europe__bucharest, europe__budapest, europe__busingen, europe__chisinau, europe__copenhagen, europe__dublin, europe__gibraltar, europe__guernsey, europe__helsinki, europe__isle_of_man, europe__istanbul, europe__jersey, europe__kaliningrad, europe__kiev, europe__kirov, europe__kyiv, europe__lisbon, europe__ljubljana, europe__london, europe__luxembourg, europe__madrid, europe__malta, europe__mariehamn, europe__minsk, europe__monaco, europe__moscow, europe__nicosia, europe__oslo, europe__paris, europe__podgorica, europe__prague, europe__riga, europe__rome, europe__samara, europe__san_marino, europe__sarajevo, europe__saratov, europe__simferopol, europe__skopje, europe__sofia, europe__stockholm, europe__tallinn, europe__tirane, europe__tiraspol, europe__ulyanovsk, europe__uzhgorod, europe__vaduz, europe__vatican, europe__vienna, europe__vilnius, europe__volgograd, europe__warsaw, europe__zagreb, europe__zaporozhye, europe__zurich, gb, gb_eire, gmt, gmt_plus0, gmt_0, gmt0, greenwich, hst, hongkong, iceland, indian__antananarivo, indian__chagos, indian__christmas, indian__cocos, indian__comoro, indian__kerguelen, indian__mahe, indian__maldives, indian__mauritius, indian__mayotte, indian__reunion, iran, israel, jamaica, japan, kwajalein, libya, met, mst, mst7mdt, mexico__bajanorte, mexico__bajasur, mexico__general, nz, nz_chat, navajo, prc, pst8pdt, pacific__apia, pacific__auckland, pacific__bougainville, pacific__chatham, pacific__chuuk, pacific__easter, pacific__efate, pacific__enderbury, pacific__fakaofo, pacific__fiji, pacific__funafuti, pacific__galapagos, pacific__gambier, pacific__guadalcanal, pacific__guam, pacific__honolulu, pacific__johnston, pacific__kanton, pacific__kiritimati, pacific__kosrae, pacific__kwajalein, pacific__majuro, pacific__marquesas, pacific__midway, pacific__nauru, pacific__niue, pacific__norfolk, pacific__noumea, pacific__pago_pago, pacific__palau, pacific__pitcairn, pacific__pohnpei, pacific__ponape, pacific__port_moresby, pacific__rarotonga, pacific__saipan, pacific__samoa, pacific__tahiti, pacific__tarawa, pacific__tongatapu, pacific__truk, pacific__wake, pacific__wallis, pacific__yap, poland, portugal, roc, rok, singapore, turkey, uct, us__alaska, us__aleutian, us__arizona, us__central, us__east_indiana, us__eastern, us__hawaii, us__indiana_starke, us__michigan, us__mountain, us__pacific, us__samoa, utc, universal, w_su, wet, zulu

-}

import Dict exposing (Dict)
import Task exposing (Task)
import Time exposing (Month(..), Weekday(..))
import TimeZone.Specification exposing (Clock(..), DateTime, DayOfMonth(..), Rule, Zone, ZoneRules(..), ZoneState)


{-| What release of the IANA Time Zone Database is this data from?
-}
version : String
version =
    "2025b"


minYear : Int
minYear =
    1970


maxYear : Int
maxYear =
    2037


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
    [ ( "Africa/Abidjan", africa__abidjan )
    , ( "Africa/Accra", africa__accra )
    , ( "Africa/Addis_Ababa", africa__addis_ababa )
    , ( "Africa/Algiers", africa__algiers )
    , ( "Africa/Asmara", africa__asmara )
    , ( "Africa/Asmera", africa__asmera )
    , ( "Africa/Bamako", africa__bamako )
    , ( "Africa/Bangui", africa__bangui )
    , ( "Africa/Banjul", africa__banjul )
    , ( "Africa/Bissau", africa__bissau )
    , ( "Africa/Blantyre", africa__blantyre )
    , ( "Africa/Brazzaville", africa__brazzaville )
    , ( "Africa/Bujumbura", africa__bujumbura )
    , ( "Africa/Cairo", africa__cairo )
    , ( "Africa/Casablanca", africa__casablanca )
    , ( "Africa/Ceuta", africa__ceuta )
    , ( "Africa/Conakry", africa__conakry )
    , ( "Africa/Dakar", africa__dakar )
    , ( "Africa/Dar_es_Salaam", africa__dar_es_salaam )
    , ( "Africa/Djibouti", africa__djibouti )
    , ( "Africa/Douala", africa__douala )
    , ( "Africa/El_Aaiun", africa__el_aaiun )
    , ( "Africa/Freetown", africa__freetown )
    , ( "Africa/Gaborone", africa__gaborone )
    , ( "Africa/Harare", africa__harare )
    , ( "Africa/Johannesburg", africa__johannesburg )
    , ( "Africa/Juba", africa__juba )
    , ( "Africa/Kampala", africa__kampala )
    , ( "Africa/Khartoum", africa__khartoum )
    , ( "Africa/Kigali", africa__kigali )
    , ( "Africa/Kinshasa", africa__kinshasa )
    , ( "Africa/Lagos", africa__lagos )
    , ( "Africa/Libreville", africa__libreville )
    , ( "Africa/Lome", africa__lome )
    , ( "Africa/Luanda", africa__luanda )
    , ( "Africa/Lubumbashi", africa__lubumbashi )
    , ( "Africa/Lusaka", africa__lusaka )
    , ( "Africa/Malabo", africa__malabo )
    , ( "Africa/Maputo", africa__maputo )
    , ( "Africa/Maseru", africa__maseru )
    , ( "Africa/Mbabane", africa__mbabane )
    , ( "Africa/Mogadishu", africa__mogadishu )
    , ( "Africa/Monrovia", africa__monrovia )
    , ( "Africa/Nairobi", africa__nairobi )
    , ( "Africa/Ndjamena", africa__ndjamena )
    , ( "Africa/Niamey", africa__niamey )
    , ( "Africa/Nouakchott", africa__nouakchott )
    , ( "Africa/Ouagadougou", africa__ouagadougou )
    , ( "Africa/Porto-Novo", africa__porto_novo )
    , ( "Africa/Sao_Tome", africa__sao_tome )
    , ( "Africa/Timbuktu", africa__timbuktu )
    , ( "Africa/Tripoli", africa__tripoli )
    , ( "Africa/Tunis", africa__tunis )
    , ( "Africa/Windhoek", africa__windhoek )
    , ( "America/Adak", america__adak )
    , ( "America/Anchorage", america__anchorage )
    , ( "America/Anguilla", america__anguilla )
    , ( "America/Antigua", america__antigua )
    , ( "America/Araguaina", america__araguaina )
    , ( "America/Argentina/Buenos_Aires", america__argentina__buenos_aires )
    , ( "America/Argentina/Catamarca", america__argentina__catamarca )
    , ( "America/Argentina/ComodRivadavia", america__argentina__comodrivadavia )
    , ( "America/Argentina/Cordoba", america__argentina__cordoba )
    , ( "America/Argentina/Jujuy", america__argentina__jujuy )
    , ( "America/Argentina/La_Rioja", america__argentina__la_rioja )
    , ( "America/Argentina/Mendoza", america__argentina__mendoza )
    , ( "America/Argentina/Rio_Gallegos", america__argentina__rio_gallegos )
    , ( "America/Argentina/Salta", america__argentina__salta )
    , ( "America/Argentina/San_Juan", america__argentina__san_juan )
    , ( "America/Argentina/San_Luis", america__argentina__san_luis )
    , ( "America/Argentina/Tucuman", america__argentina__tucuman )
    , ( "America/Argentina/Ushuaia", america__argentina__ushuaia )
    , ( "America/Aruba", america__aruba )
    , ( "America/Asuncion", america__asuncion )
    , ( "America/Atikokan", america__atikokan )
    , ( "America/Atka", america__atka )
    , ( "America/Bahia", america__bahia )
    , ( "America/Bahia_Banderas", america__bahia_banderas )
    , ( "America/Barbados", america__barbados )
    , ( "America/Belem", america__belem )
    , ( "America/Belize", america__belize )
    , ( "America/Blanc-Sablon", america__blanc_sablon )
    , ( "America/Boa_Vista", america__boa_vista )
    , ( "America/Bogota", america__bogota )
    , ( "America/Boise", america__boise )
    , ( "America/Buenos_Aires", america__buenos_aires )
    , ( "America/Cambridge_Bay", america__cambridge_bay )
    , ( "America/Campo_Grande", america__campo_grande )
    , ( "America/Cancun", america__cancun )
    , ( "America/Caracas", america__caracas )
    , ( "America/Catamarca", america__catamarca )
    , ( "America/Cayenne", america__cayenne )
    , ( "America/Cayman", america__cayman )
    , ( "America/Chicago", america__chicago )
    , ( "America/Chihuahua", america__chihuahua )
    , ( "America/Ciudad_Juarez", america__ciudad_juarez )
    , ( "America/Coral_Harbour", america__coral_harbour )
    , ( "America/Cordoba", america__cordoba )
    , ( "America/Costa_Rica", america__costa_rica )
    , ( "America/Coyhaique", america__coyhaique )
    , ( "America/Creston", america__creston )
    , ( "America/Cuiaba", america__cuiaba )
    , ( "America/Curacao", america__curacao )
    , ( "America/Danmarkshavn", america__danmarkshavn )
    , ( "America/Dawson", america__dawson )
    , ( "America/Dawson_Creek", america__dawson_creek )
    , ( "America/Denver", america__denver )
    , ( "America/Detroit", america__detroit )
    , ( "America/Dominica", america__dominica )
    , ( "America/Edmonton", america__edmonton )
    , ( "America/Eirunepe", america__eirunepe )
    , ( "America/El_Salvador", america__el_salvador )
    , ( "America/Ensenada", america__ensenada )
    , ( "America/Fort_Nelson", america__fort_nelson )
    , ( "America/Fort_Wayne", america__fort_wayne )
    , ( "America/Fortaleza", america__fortaleza )
    , ( "America/Glace_Bay", america__glace_bay )
    , ( "America/Godthab", america__godthab )
    , ( "America/Goose_Bay", america__goose_bay )
    , ( "America/Grand_Turk", america__grand_turk )
    , ( "America/Grenada", america__grenada )
    , ( "America/Guadeloupe", america__guadeloupe )
    , ( "America/Guatemala", america__guatemala )
    , ( "America/Guayaquil", america__guayaquil )
    , ( "America/Guyana", america__guyana )
    , ( "America/Halifax", america__halifax )
    , ( "America/Havana", america__havana )
    , ( "America/Hermosillo", america__hermosillo )
    , ( "America/Indiana/Indianapolis", america__indiana__indianapolis )
    , ( "America/Indiana/Knox", america__indiana__knox )
    , ( "America/Indiana/Marengo", america__indiana__marengo )
    , ( "America/Indiana/Petersburg", america__indiana__petersburg )
    , ( "America/Indiana/Tell_City", america__indiana__tell_city )
    , ( "America/Indiana/Vevay", america__indiana__vevay )
    , ( "America/Indiana/Vincennes", america__indiana__vincennes )
    , ( "America/Indiana/Winamac", america__indiana__winamac )
    , ( "America/Indianapolis", america__indianapolis )
    , ( "America/Inuvik", america__inuvik )
    , ( "America/Iqaluit", america__iqaluit )
    , ( "America/Jamaica", america__jamaica )
    , ( "America/Jujuy", america__jujuy )
    , ( "America/Juneau", america__juneau )
    , ( "America/Kentucky/Louisville", america__kentucky__louisville )
    , ( "America/Kentucky/Monticello", america__kentucky__monticello )
    , ( "America/Knox_IN", america__knox_in )
    , ( "America/Kralendijk", america__kralendijk )
    , ( "America/La_Paz", america__la_paz )
    , ( "America/Lima", america__lima )
    , ( "America/Los_Angeles", america__los_angeles )
    , ( "America/Louisville", america__louisville )
    , ( "America/Lower_Princes", america__lower_princes )
    , ( "America/Maceio", america__maceio )
    , ( "America/Managua", america__managua )
    , ( "America/Manaus", america__manaus )
    , ( "America/Marigot", america__marigot )
    , ( "America/Martinique", america__martinique )
    , ( "America/Matamoros", america__matamoros )
    , ( "America/Mazatlan", america__mazatlan )
    , ( "America/Mendoza", america__mendoza )
    , ( "America/Menominee", america__menominee )
    , ( "America/Merida", america__merida )
    , ( "America/Metlakatla", america__metlakatla )
    , ( "America/Mexico_City", america__mexico_city )
    , ( "America/Miquelon", america__miquelon )
    , ( "America/Moncton", america__moncton )
    , ( "America/Monterrey", america__monterrey )
    , ( "America/Montevideo", america__montevideo )
    , ( "America/Montreal", america__montreal )
    , ( "America/Montserrat", america__montserrat )
    , ( "America/Nassau", america__nassau )
    , ( "America/New_York", america__new_york )
    , ( "America/Nipigon", america__nipigon )
    , ( "America/Nome", america__nome )
    , ( "America/Noronha", america__noronha )
    , ( "America/North_Dakota/Beulah", america__north_dakota__beulah )
    , ( "America/North_Dakota/Center", america__north_dakota__center )
    , ( "America/North_Dakota/New_Salem", america__north_dakota__new_salem )
    , ( "America/Nuuk", america__nuuk )
    , ( "America/Ojinaga", america__ojinaga )
    , ( "America/Panama", america__panama )
    , ( "America/Pangnirtung", america__pangnirtung )
    , ( "America/Paramaribo", america__paramaribo )
    , ( "America/Phoenix", america__phoenix )
    , ( "America/Port-au-Prince", america__port_au_prince )
    , ( "America/Port_of_Spain", america__port_of_spain )
    , ( "America/Porto_Acre", america__porto_acre )
    , ( "America/Porto_Velho", america__porto_velho )
    , ( "America/Puerto_Rico", america__puerto_rico )
    , ( "America/Punta_Arenas", america__punta_arenas )
    , ( "America/Rainy_River", america__rainy_river )
    , ( "America/Rankin_Inlet", america__rankin_inlet )
    , ( "America/Recife", america__recife )
    , ( "America/Regina", america__regina )
    , ( "America/Resolute", america__resolute )
    , ( "America/Rio_Branco", america__rio_branco )
    , ( "America/Rosario", america__rosario )
    , ( "America/Santa_Isabel", america__santa_isabel )
    , ( "America/Santarem", america__santarem )
    , ( "America/Santiago", america__santiago )
    , ( "America/Santo_Domingo", america__santo_domingo )
    , ( "America/Sao_Paulo", america__sao_paulo )
    , ( "America/Scoresbysund", america__scoresbysund )
    , ( "America/Shiprock", america__shiprock )
    , ( "America/Sitka", america__sitka )
    , ( "America/St_Barthelemy", america__st_barthelemy )
    , ( "America/St_Johns", america__st_johns )
    , ( "America/St_Kitts", america__st_kitts )
    , ( "America/St_Lucia", america__st_lucia )
    , ( "America/St_Thomas", america__st_thomas )
    , ( "America/St_Vincent", america__st_vincent )
    , ( "America/Swift_Current", america__swift_current )
    , ( "America/Tegucigalpa", america__tegucigalpa )
    , ( "America/Thule", america__thule )
    , ( "America/Thunder_Bay", america__thunder_bay )
    , ( "America/Tijuana", america__tijuana )
    , ( "America/Toronto", america__toronto )
    , ( "America/Tortola", america__tortola )
    , ( "America/Vancouver", america__vancouver )
    , ( "America/Virgin", america__virgin )
    , ( "America/Whitehorse", america__whitehorse )
    , ( "America/Winnipeg", america__winnipeg )
    , ( "America/Yakutat", america__yakutat )
    , ( "America/Yellowknife", america__yellowknife )
    , ( "Antarctica/Casey", antarctica__casey )
    , ( "Antarctica/Davis", antarctica__davis )
    , ( "Antarctica/DumontDUrville", antarctica__dumontdurville )
    , ( "Antarctica/Macquarie", antarctica__macquarie )
    , ( "Antarctica/Mawson", antarctica__mawson )
    , ( "Antarctica/McMurdo", antarctica__mcmurdo )
    , ( "Antarctica/Palmer", antarctica__palmer )
    , ( "Antarctica/Rothera", antarctica__rothera )
    , ( "Antarctica/South_Pole", antarctica__south_pole )
    , ( "Antarctica/Syowa", antarctica__syowa )
    , ( "Antarctica/Troll", antarctica__troll )
    , ( "Antarctica/Vostok", antarctica__vostok )
    , ( "Arctic/Longyearbyen", arctic__longyearbyen )
    , ( "Asia/Aden", asia__aden )
    , ( "Asia/Almaty", asia__almaty )
    , ( "Asia/Amman", asia__amman )
    , ( "Asia/Anadyr", asia__anadyr )
    , ( "Asia/Aqtau", asia__aqtau )
    , ( "Asia/Aqtobe", asia__aqtobe )
    , ( "Asia/Ashgabat", asia__ashgabat )
    , ( "Asia/Ashkhabad", asia__ashkhabad )
    , ( "Asia/Atyrau", asia__atyrau )
    , ( "Asia/Baghdad", asia__baghdad )
    , ( "Asia/Bahrain", asia__bahrain )
    , ( "Asia/Baku", asia__baku )
    , ( "Asia/Bangkok", asia__bangkok )
    , ( "Asia/Barnaul", asia__barnaul )
    , ( "Asia/Beirut", asia__beirut )
    , ( "Asia/Bishkek", asia__bishkek )
    , ( "Asia/Brunei", asia__brunei )
    , ( "Asia/Calcutta", asia__calcutta )
    , ( "Asia/Chita", asia__chita )
    , ( "Asia/Choibalsan", asia__choibalsan )
    , ( "Asia/Chongqing", asia__chongqing )
    , ( "Asia/Chungking", asia__chungking )
    , ( "Asia/Colombo", asia__colombo )
    , ( "Asia/Dacca", asia__dacca )
    , ( "Asia/Damascus", asia__damascus )
    , ( "Asia/Dhaka", asia__dhaka )
    , ( "Asia/Dili", asia__dili )
    , ( "Asia/Dubai", asia__dubai )
    , ( "Asia/Dushanbe", asia__dushanbe )
    , ( "Asia/Famagusta", asia__famagusta )
    , ( "Asia/Gaza", asia__gaza )
    , ( "Asia/Hanoi", asia__hanoi )
    , ( "Asia/Harbin", asia__harbin )
    , ( "Asia/Hebron", asia__hebron )
    , ( "Asia/Ho_Chi_Minh", asia__ho_chi_minh )
    , ( "Asia/Hong_Kong", asia__hong_kong )
    , ( "Asia/Hovd", asia__hovd )
    , ( "Asia/Irkutsk", asia__irkutsk )
    , ( "Asia/Istanbul", asia__istanbul )
    , ( "Asia/Jakarta", asia__jakarta )
    , ( "Asia/Jayapura", asia__jayapura )
    , ( "Asia/Jerusalem", asia__jerusalem )
    , ( "Asia/Kabul", asia__kabul )
    , ( "Asia/Kamchatka", asia__kamchatka )
    , ( "Asia/Karachi", asia__karachi )
    , ( "Asia/Kashgar", asia__kashgar )
    , ( "Asia/Kathmandu", asia__kathmandu )
    , ( "Asia/Katmandu", asia__katmandu )
    , ( "Asia/Khandyga", asia__khandyga )
    , ( "Asia/Kolkata", asia__kolkata )
    , ( "Asia/Krasnoyarsk", asia__krasnoyarsk )
    , ( "Asia/Kuala_Lumpur", asia__kuala_lumpur )
    , ( "Asia/Kuching", asia__kuching )
    , ( "Asia/Kuwait", asia__kuwait )
    , ( "Asia/Macao", asia__macao )
    , ( "Asia/Macau", asia__macau )
    , ( "Asia/Magadan", asia__magadan )
    , ( "Asia/Makassar", asia__makassar )
    , ( "Asia/Manila", asia__manila )
    , ( "Asia/Muscat", asia__muscat )
    , ( "Asia/Nicosia", asia__nicosia )
    , ( "Asia/Novokuznetsk", asia__novokuznetsk )
    , ( "Asia/Novosibirsk", asia__novosibirsk )
    , ( "Asia/Omsk", asia__omsk )
    , ( "Asia/Oral", asia__oral )
    , ( "Asia/Phnom_Penh", asia__phnom_penh )
    , ( "Asia/Pontianak", asia__pontianak )
    , ( "Asia/Pyongyang", asia__pyongyang )
    , ( "Asia/Qatar", asia__qatar )
    , ( "Asia/Qostanay", asia__qostanay )
    , ( "Asia/Qyzylorda", asia__qyzylorda )
    , ( "Asia/Rangoon", asia__rangoon )
    , ( "Asia/Riyadh", asia__riyadh )
    , ( "Asia/Saigon", asia__saigon )
    , ( "Asia/Sakhalin", asia__sakhalin )
    , ( "Asia/Samarkand", asia__samarkand )
    , ( "Asia/Seoul", asia__seoul )
    , ( "Asia/Shanghai", asia__shanghai )
    , ( "Asia/Singapore", asia__singapore )
    , ( "Asia/Srednekolymsk", asia__srednekolymsk )
    , ( "Asia/Taipei", asia__taipei )
    , ( "Asia/Tashkent", asia__tashkent )
    , ( "Asia/Tbilisi", asia__tbilisi )
    , ( "Asia/Tehran", asia__tehran )
    , ( "Asia/Tel_Aviv", asia__tel_aviv )
    , ( "Asia/Thimbu", asia__thimbu )
    , ( "Asia/Thimphu", asia__thimphu )
    , ( "Asia/Tokyo", asia__tokyo )
    , ( "Asia/Tomsk", asia__tomsk )
    , ( "Asia/Ujung_Pandang", asia__ujung_pandang )
    , ( "Asia/Ulaanbaatar", asia__ulaanbaatar )
    , ( "Asia/Ulan_Bator", asia__ulan_bator )
    , ( "Asia/Urumqi", asia__urumqi )
    , ( "Asia/Ust-Nera", asia__ust_nera )
    , ( "Asia/Vientiane", asia__vientiane )
    , ( "Asia/Vladivostok", asia__vladivostok )
    , ( "Asia/Yakutsk", asia__yakutsk )
    , ( "Asia/Yangon", asia__yangon )
    , ( "Asia/Yekaterinburg", asia__yekaterinburg )
    , ( "Asia/Yerevan", asia__yerevan )
    , ( "Atlantic/Azores", atlantic__azores )
    , ( "Atlantic/Bermuda", atlantic__bermuda )
    , ( "Atlantic/Canary", atlantic__canary )
    , ( "Atlantic/Cape_Verde", atlantic__cape_verde )
    , ( "Atlantic/Faeroe", atlantic__faeroe )
    , ( "Atlantic/Faroe", atlantic__faroe )
    , ( "Atlantic/Jan_Mayen", atlantic__jan_mayen )
    , ( "Atlantic/Madeira", atlantic__madeira )
    , ( "Atlantic/Reykjavik", atlantic__reykjavik )
    , ( "Atlantic/South_Georgia", atlantic__south_georgia )
    , ( "Atlantic/St_Helena", atlantic__st_helena )
    , ( "Atlantic/Stanley", atlantic__stanley )
    , ( "Australia/ACT", australia__act )
    , ( "Australia/Adelaide", australia__adelaide )
    , ( "Australia/Brisbane", australia__brisbane )
    , ( "Australia/Broken_Hill", australia__broken_hill )
    , ( "Australia/Canberra", australia__canberra )
    , ( "Australia/Currie", australia__currie )
    , ( "Australia/Darwin", australia__darwin )
    , ( "Australia/Eucla", australia__eucla )
    , ( "Australia/Hobart", australia__hobart )
    , ( "Australia/LHI", australia__lhi )
    , ( "Australia/Lindeman", australia__lindeman )
    , ( "Australia/Lord_Howe", australia__lord_howe )
    , ( "Australia/Melbourne", australia__melbourne )
    , ( "Australia/NSW", australia__nsw )
    , ( "Australia/North", australia__north )
    , ( "Australia/Perth", australia__perth )
    , ( "Australia/Queensland", australia__queensland )
    , ( "Australia/South", australia__south )
    , ( "Australia/Sydney", australia__sydney )
    , ( "Australia/Tasmania", australia__tasmania )
    , ( "Australia/Victoria", australia__victoria )
    , ( "Australia/West", australia__west )
    , ( "Australia/Yancowinna", australia__yancowinna )
    , ( "Brazil/Acre", brazil__acre )
    , ( "Brazil/DeNoronha", brazil__denoronha )
    , ( "Brazil/East", brazil__east )
    , ( "Brazil/West", brazil__west )
    , ( "CET", cet )
    , ( "CST6CDT", cst6cdt )
    , ( "Canada/Atlantic", canada__atlantic )
    , ( "Canada/Central", canada__central )
    , ( "Canada/Eastern", canada__eastern )
    , ( "Canada/Mountain", canada__mountain )
    , ( "Canada/Newfoundland", canada__newfoundland )
    , ( "Canada/Pacific", canada__pacific )
    , ( "Canada/Saskatchewan", canada__saskatchewan )
    , ( "Canada/Yukon", canada__yukon )
    , ( "Chile/Continental", chile__continental )
    , ( "Chile/EasterIsland", chile__easterisland )
    , ( "Cuba", cuba )
    , ( "EET", eet )
    , ( "EST", est )
    , ( "EST5EDT", est5edt )
    , ( "Egypt", egypt )
    , ( "Eire", eire )
    , ( "Etc/GMT", etc__gmt )
    , ( "Etc/GMT+0", etc__gmt_plus0 )
    , ( "Etc/GMT+1", etc__gmt_plus1 )
    , ( "Etc/GMT+10", etc__gmt_plus10 )
    , ( "Etc/GMT+11", etc__gmt_plus11 )
    , ( "Etc/GMT+12", etc__gmt_plus12 )
    , ( "Etc/GMT+2", etc__gmt_plus2 )
    , ( "Etc/GMT+3", etc__gmt_plus3 )
    , ( "Etc/GMT+4", etc__gmt_plus4 )
    , ( "Etc/GMT+5", etc__gmt_plus5 )
    , ( "Etc/GMT+6", etc__gmt_plus6 )
    , ( "Etc/GMT+7", etc__gmt_plus7 )
    , ( "Etc/GMT+8", etc__gmt_plus8 )
    , ( "Etc/GMT+9", etc__gmt_plus9 )
    , ( "Etc/GMT-0", etc__gmt_0 )
    , ( "Etc/GMT-1", etc__gmt_1 )
    , ( "Etc/GMT-10", etc__gmt_10 )
    , ( "Etc/GMT-11", etc__gmt_11 )
    , ( "Etc/GMT-12", etc__gmt_12 )
    , ( "Etc/GMT-13", etc__gmt_13 )
    , ( "Etc/GMT-14", etc__gmt_14 )
    , ( "Etc/GMT-2", etc__gmt_2 )
    , ( "Etc/GMT-3", etc__gmt_3 )
    , ( "Etc/GMT-4", etc__gmt_4 )
    , ( "Etc/GMT-5", etc__gmt_5 )
    , ( "Etc/GMT-6", etc__gmt_6 )
    , ( "Etc/GMT-7", etc__gmt_7 )
    , ( "Etc/GMT-8", etc__gmt_8 )
    , ( "Etc/GMT-9", etc__gmt_9 )
    , ( "Etc/GMT0", etc__gmt0 )
    , ( "Etc/Greenwich", etc__greenwich )
    , ( "Etc/UCT", etc__uct )
    , ( "Etc/UTC", etc__utc )
    , ( "Etc/Universal", etc__universal )
    , ( "Etc/Zulu", etc__zulu )
    , ( "Europe/Amsterdam", europe__amsterdam )
    , ( "Europe/Andorra", europe__andorra )
    , ( "Europe/Astrakhan", europe__astrakhan )
    , ( "Europe/Athens", europe__athens )
    , ( "Europe/Belfast", europe__belfast )
    , ( "Europe/Belgrade", europe__belgrade )
    , ( "Europe/Berlin", europe__berlin )
    , ( "Europe/Bratislava", europe__bratislava )
    , ( "Europe/Brussels", europe__brussels )
    , ( "Europe/Bucharest", europe__bucharest )
    , ( "Europe/Budapest", europe__budapest )
    , ( "Europe/Busingen", europe__busingen )
    , ( "Europe/Chisinau", europe__chisinau )
    , ( "Europe/Copenhagen", europe__copenhagen )
    , ( "Europe/Dublin", europe__dublin )
    , ( "Europe/Gibraltar", europe__gibraltar )
    , ( "Europe/Guernsey", europe__guernsey )
    , ( "Europe/Helsinki", europe__helsinki )
    , ( "Europe/Isle_of_Man", europe__isle_of_man )
    , ( "Europe/Istanbul", europe__istanbul )
    , ( "Europe/Jersey", europe__jersey )
    , ( "Europe/Kaliningrad", europe__kaliningrad )
    , ( "Europe/Kiev", europe__kiev )
    , ( "Europe/Kirov", europe__kirov )
    , ( "Europe/Kyiv", europe__kyiv )
    , ( "Europe/Lisbon", europe__lisbon )
    , ( "Europe/Ljubljana", europe__ljubljana )
    , ( "Europe/London", europe__london )
    , ( "Europe/Luxembourg", europe__luxembourg )
    , ( "Europe/Madrid", europe__madrid )
    , ( "Europe/Malta", europe__malta )
    , ( "Europe/Mariehamn", europe__mariehamn )
    , ( "Europe/Minsk", europe__minsk )
    , ( "Europe/Monaco", europe__monaco )
    , ( "Europe/Moscow", europe__moscow )
    , ( "Europe/Nicosia", europe__nicosia )
    , ( "Europe/Oslo", europe__oslo )
    , ( "Europe/Paris", europe__paris )
    , ( "Europe/Podgorica", europe__podgorica )
    , ( "Europe/Prague", europe__prague )
    , ( "Europe/Riga", europe__riga )
    , ( "Europe/Rome", europe__rome )
    , ( "Europe/Samara", europe__samara )
    , ( "Europe/San_Marino", europe__san_marino )
    , ( "Europe/Sarajevo", europe__sarajevo )
    , ( "Europe/Saratov", europe__saratov )
    , ( "Europe/Simferopol", europe__simferopol )
    , ( "Europe/Skopje", europe__skopje )
    , ( "Europe/Sofia", europe__sofia )
    , ( "Europe/Stockholm", europe__stockholm )
    , ( "Europe/Tallinn", europe__tallinn )
    , ( "Europe/Tirane", europe__tirane )
    , ( "Europe/Tiraspol", europe__tiraspol )
    , ( "Europe/Ulyanovsk", europe__ulyanovsk )
    , ( "Europe/Uzhgorod", europe__uzhgorod )
    , ( "Europe/Vaduz", europe__vaduz )
    , ( "Europe/Vatican", europe__vatican )
    , ( "Europe/Vienna", europe__vienna )
    , ( "Europe/Vilnius", europe__vilnius )
    , ( "Europe/Volgograd", europe__volgograd )
    , ( "Europe/Warsaw", europe__warsaw )
    , ( "Europe/Zagreb", europe__zagreb )
    , ( "Europe/Zaporozhye", europe__zaporozhye )
    , ( "Europe/Zurich", europe__zurich )
    , ( "GB", gb )
    , ( "GB-Eire", gb_eire )
    , ( "GMT", gmt )
    , ( "GMT+0", gmt_plus0 )
    , ( "GMT-0", gmt_0 )
    , ( "GMT0", gmt0 )
    , ( "Greenwich", greenwich )
    , ( "HST", hst )
    , ( "Hongkong", hongkong )
    , ( "Iceland", iceland )
    , ( "Indian/Antananarivo", indian__antananarivo )
    , ( "Indian/Chagos", indian__chagos )
    , ( "Indian/Christmas", indian__christmas )
    , ( "Indian/Cocos", indian__cocos )
    , ( "Indian/Comoro", indian__comoro )
    , ( "Indian/Kerguelen", indian__kerguelen )
    , ( "Indian/Mahe", indian__mahe )
    , ( "Indian/Maldives", indian__maldives )
    , ( "Indian/Mauritius", indian__mauritius )
    , ( "Indian/Mayotte", indian__mayotte )
    , ( "Indian/Reunion", indian__reunion )
    , ( "Iran", iran )
    , ( "Israel", israel )
    , ( "Jamaica", jamaica )
    , ( "Japan", japan )
    , ( "Kwajalein", kwajalein )
    , ( "Libya", libya )
    , ( "MET", met )
    , ( "MST", mst )
    , ( "MST7MDT", mst7mdt )
    , ( "Mexico/BajaNorte", mexico__bajanorte )
    , ( "Mexico/BajaSur", mexico__bajasur )
    , ( "Mexico/General", mexico__general )
    , ( "NZ", nz )
    , ( "NZ-CHAT", nz_chat )
    , ( "Navajo", navajo )
    , ( "PRC", prc )
    , ( "PST8PDT", pst8pdt )
    , ( "Pacific/Apia", pacific__apia )
    , ( "Pacific/Auckland", pacific__auckland )
    , ( "Pacific/Bougainville", pacific__bougainville )
    , ( "Pacific/Chatham", pacific__chatham )
    , ( "Pacific/Chuuk", pacific__chuuk )
    , ( "Pacific/Easter", pacific__easter )
    , ( "Pacific/Efate", pacific__efate )
    , ( "Pacific/Enderbury", pacific__enderbury )
    , ( "Pacific/Fakaofo", pacific__fakaofo )
    , ( "Pacific/Fiji", pacific__fiji )
    , ( "Pacific/Funafuti", pacific__funafuti )
    , ( "Pacific/Galapagos", pacific__galapagos )
    , ( "Pacific/Gambier", pacific__gambier )
    , ( "Pacific/Guadalcanal", pacific__guadalcanal )
    , ( "Pacific/Guam", pacific__guam )
    , ( "Pacific/Honolulu", pacific__honolulu )
    , ( "Pacific/Johnston", pacific__johnston )
    , ( "Pacific/Kanton", pacific__kanton )
    , ( "Pacific/Kiritimati", pacific__kiritimati )
    , ( "Pacific/Kosrae", pacific__kosrae )
    , ( "Pacific/Kwajalein", pacific__kwajalein )
    , ( "Pacific/Majuro", pacific__majuro )
    , ( "Pacific/Marquesas", pacific__marquesas )
    , ( "Pacific/Midway", pacific__midway )
    , ( "Pacific/Nauru", pacific__nauru )
    , ( "Pacific/Niue", pacific__niue )
    , ( "Pacific/Norfolk", pacific__norfolk )
    , ( "Pacific/Noumea", pacific__noumea )
    , ( "Pacific/Pago_Pago", pacific__pago_pago )
    , ( "Pacific/Palau", pacific__palau )
    , ( "Pacific/Pitcairn", pacific__pitcairn )
    , ( "Pacific/Pohnpei", pacific__pohnpei )
    , ( "Pacific/Ponape", pacific__ponape )
    , ( "Pacific/Port_Moresby", pacific__port_moresby )
    , ( "Pacific/Rarotonga", pacific__rarotonga )
    , ( "Pacific/Saipan", pacific__saipan )
    , ( "Pacific/Samoa", pacific__samoa )
    , ( "Pacific/Tahiti", pacific__tahiti )
    , ( "Pacific/Tarawa", pacific__tarawa )
    , ( "Pacific/Tongatapu", pacific__tongatapu )
    , ( "Pacific/Truk", pacific__truk )
    , ( "Pacific/Wake", pacific__wake )
    , ( "Pacific/Wallis", pacific__wallis )
    , ( "Pacific/Yap", pacific__yap )
    , ( "Poland", poland )
    , ( "Portugal", portugal )
    , ( "ROC", roc )
    , ( "ROK", rok )
    , ( "Singapore", singapore )
    , ( "Turkey", turkey )
    , ( "UCT", uct )
    , ( "US/Alaska", us__alaska )
    , ( "US/Aleutian", us__aleutian )
    , ( "US/Arizona", us__arizona )
    , ( "US/Central", us__central )
    , ( "US/East-Indiana", us__east_indiana )
    , ( "US/Eastern", us__eastern )
    , ( "US/Hawaii", us__hawaii )
    , ( "US/Indiana-Starke", us__indiana_starke )
    , ( "US/Michigan", us__michigan )
    , ( "US/Mountain", us__mountain )
    , ( "US/Pacific", us__pacific )
    , ( "US/Samoa", us__samoa )
    , ( "UTC", utc )
    , ( "Universal", universal )
    , ( "W-SU", w_su )
    , ( "WET", wet )
    , ( "Zulu", zulu )
    ]
        |> Dict.fromList



-- Rules


rules_AN : List Rule
rules_AN =
    [ Rule 1971 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Day 27) 120 Standard 0
    , Rule 1973 1981 Mar (Next Sun 1) 120 Standard 0
    , Rule 1982 1982 Apr (Next Sun 1) 120 Standard 0
    , Rule 1983 1985 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1989 Mar (Next Sun 15) 120 Standard 0
    , Rule 1986 1986 Oct (Day 19) 120 Standard 60
    , Rule 1987 1999 Oct (Last Sun) 120 Standard 60
    , Rule 1990 1995 Mar (Next Sun 1) 120 Standard 0
    , Rule 1996 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2000 2000 Aug (Last Sun) 120 Standard 60
    , Rule 2001 2007 Oct (Last Sun) 120 Standard 60
    , Rule 2006 2006 Apr (Next Sun 1) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 Standard 60
    ]


rules_AQ : List Rule
rules_AQ =
    [ Rule 1971 1971 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Last Sun) 120 Standard 0
    , Rule 1989 1991 Oct (Last Sun) 120 Standard 60
    , Rule 1990 1992 Mar (Next Sun 1) 120 Standard 0
    ]


rules_AS : List Rule
rules_AS =
    [ Rule 1971 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1986 1986 Oct (Day 19) 120 Standard 60
    , Rule 1987 2007 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Day 27) 120 Standard 0
    , Rule 1973 1985 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1990 Mar (Next Sun 15) 120 Standard 0
    , Rule 1991 1991 Mar (Day 3) 120 Standard 0
    , Rule 1992 1992 Mar (Day 22) 120 Standard 0
    , Rule 1993 1993 Mar (Day 7) 120 Standard 0
    , Rule 1994 1994 Mar (Day 20) 120 Standard 0
    , Rule 1995 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2006 2006 Apr (Day 2) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 Standard 60
    ]


rules_AT : List Rule
rules_AT =
    [ Rule 1968 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1969 1971 Mar (Next Sun 8) 120 Standard 0
    , Rule 1972 1972 Feb (Last Sun) 120 Standard 0
    , Rule 1973 1981 Mar (Next Sun 1) 120 Standard 0
    , Rule 1982 1983 Mar (Last Sun) 120 Standard 0
    , Rule 1984 1986 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1986 Oct (Next Sun 15) 120 Standard 60
    , Rule 1987 1990 Mar (Next Sun 15) 120 Standard 0
    , Rule 1987 1987 Oct (Next Sun 22) 120 Standard 60
    , Rule 1988 1990 Oct (Last Sun) 120 Standard 60
    , Rule 1991 1999 Oct (Next Sun 1) 120 Standard 60
    , Rule 1991 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2000 2000 Aug (Last Sun) 120 Standard 60
    , Rule 2001 maxYear Oct (Next Sun 1) 120 Standard 60
    , Rule 2006 2006 Apr (Next Sun 1) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    ]


rules_AV : List Rule
rules_AV =
    [ Rule 1971 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Last Sun) 120 Standard 0
    , Rule 1973 1985 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1990 Mar (Next Sun 15) 120 Standard 0
    , Rule 1986 1987 Oct (Next Sun 15) 120 Standard 60
    , Rule 1988 1999 Oct (Last Sun) 120 Standard 60
    , Rule 1991 1994 Mar (Next Sun 1) 120 Standard 0
    , Rule 1995 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2000 2000 Aug (Last Sun) 120 Standard 60
    , Rule 2001 2007 Oct (Last Sun) 120 Standard 60
    , Rule 2006 2006 Apr (Next Sun 1) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 Standard 60
    ]


rules_AW : List Rule
rules_AW =
    [ Rule 1974 1974 Oct (Last Sun) 120 Standard 60
    , Rule 1975 1975 Mar (Next Sun 1) 120 Standard 0
    , Rule 1983 1983 Oct (Last Sun) 120 Standard 60
    , Rule 1984 1984 Mar (Next Sun 1) 120 Standard 0
    , Rule 1991 1991 Nov (Day 17) 120 Standard 60
    , Rule 1992 1992 Mar (Next Sun 1) 120 Standard 0
    , Rule 2006 2006 Dec (Day 3) 120 Standard 60
    , Rule 2007 2009 Mar (Last Sun) 120 Standard 0
    , Rule 2007 2008 Oct (Last Sun) 120 Standard 60
    ]


rules_Albania : List Rule
rules_Albania =
    [ Rule 1974 1974 May (Day 4) 0 WallClock 60
    , Rule 1974 1974 Oct (Day 2) 0 WallClock 0
    , Rule 1975 1975 May (Day 1) 0 WallClock 60
    , Rule 1975 1975 Oct (Day 2) 0 WallClock 0
    , Rule 1976 1976 May (Day 2) 0 WallClock 60
    , Rule 1976 1976 Oct (Day 3) 0 WallClock 0
    , Rule 1977 1977 May (Day 8) 0 WallClock 60
    , Rule 1977 1977 Oct (Day 2) 0 WallClock 0
    , Rule 1978 1978 May (Day 6) 0 WallClock 60
    , Rule 1978 1978 Oct (Day 1) 0 WallClock 0
    , Rule 1979 1979 May (Day 5) 0 WallClock 60
    , Rule 1979 1979 Sep (Day 30) 0 WallClock 0
    , Rule 1980 1980 May (Day 3) 0 WallClock 60
    , Rule 1980 1980 Oct (Day 4) 0 WallClock 0
    , Rule 1981 1981 Apr (Day 26) 0 WallClock 60
    , Rule 1981 1981 Sep (Day 27) 0 WallClock 0
    , Rule 1982 1982 May (Day 2) 0 WallClock 60
    , Rule 1982 1982 Oct (Day 3) 0 WallClock 0
    , Rule 1983 1983 Apr (Day 18) 0 WallClock 60
    , Rule 1983 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1984 1984 Apr (Day 1) 0 WallClock 60
    ]


rules_Algeria : List Rule
rules_Algeria =
    [ Rule 1971 1971 Apr (Day 25) 1380 Standard 60
    , Rule 1971 1971 Sep (Day 26) 1380 Standard 0
    , Rule 1977 1977 May (Day 6) 0 WallClock 60
    , Rule 1977 1977 Oct (Day 21) 0 WallClock 0
    , Rule 1978 1978 Mar (Day 24) 60 WallClock 60
    , Rule 1978 1978 Sep (Day 22) 180 WallClock 0
    , Rule 1980 1980 Apr (Day 25) 0 WallClock 60
    , Rule 1980 1980 Oct (Day 31) 120 WallClock 0
    ]


rules_Arg : List Rule
rules_Arg =
    [ Rule 1968 1969 Apr (Next Sun 1) 0 WallClock 0
    , Rule 1974 1974 Jan (Day 23) 0 WallClock 60
    , Rule 1974 1974 May (Day 1) 0 WallClock 0
    , Rule 1988 1988 Dec (Day 1) 0 WallClock 60
    , Rule 1989 1993 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1989 1992 Oct (Next Sun 15) 0 WallClock 60
    , Rule 1999 1999 Oct (Next Sun 1) 0 WallClock 60
    , Rule 2000 2000 Mar (Day 3) 0 WallClock 0
    , Rule 2007 2007 Dec (Day 30) 0 WallClock 60
    , Rule 2008 2009 Mar (Next Sun 15) 0 WallClock 0
    , Rule 2008 2008 Oct (Next Sun 15) 0 WallClock 60
    ]


rules_Armenia : List Rule
rules_Armenia =
    [ Rule 2011 2011 Mar (Last Sun) 120 Standard 60
    , Rule 2011 2011 Oct (Last Sun) 120 Standard 0
    ]


rules_Austria : List Rule
rules_Austria =
    [ Rule 1980 1980 Apr (Day 6) 0 WallClock 60
    , Rule 1980 1980 Sep (Day 28) 0 WallClock 0
    ]


rules_Azer : List Rule
rules_Azer =
    [ Rule 1997 2015 Mar (Last Sun) 240 WallClock 60
    , Rule 1997 2015 Oct (Last Sun) 300 WallClock 0
    ]


rules_Bahamas : List Rule
rules_Bahamas =
    [ Rule 1964 1975 Oct (Last Sun) 120 WallClock 0
    , Rule 1964 1975 Apr (Last Sun) 120 WallClock 60
    ]


rules_Barb : List Rule
rules_Barb =
    [ Rule 1977 1977 Jun (Day 12) 120 WallClock 60
    , Rule 1977 1978 Oct (Next Sun 1) 120 WallClock 0
    , Rule 1978 1980 Apr (Next Sun 15) 120 WallClock 60
    , Rule 1979 1979 Sep (Day 30) 120 WallClock 0
    , Rule 1980 1980 Sep (Day 25) 120 WallClock 0
    ]


rules_Belize : List Rule
rules_Belize =
    [ Rule 1973 1973 Dec (Day 5) 0 WallClock 60
    , Rule 1974 1974 Feb (Day 9) 0 WallClock 0
    , Rule 1982 1982 Dec (Day 18) 0 WallClock 60
    , Rule 1983 1983 Feb (Day 12) 0 WallClock 0
    ]


rules_Brazil : List Rule
rules_Brazil =
    [ Rule 1985 1985 Nov (Day 2) 0 WallClock 60
    , Rule 1986 1986 Mar (Day 15) 0 WallClock 0
    , Rule 1986 1986 Oct (Day 25) 0 WallClock 60
    , Rule 1987 1987 Feb (Day 14) 0 WallClock 0
    , Rule 1987 1987 Oct (Day 25) 0 WallClock 60
    , Rule 1988 1988 Feb (Day 7) 0 WallClock 0
    , Rule 1988 1988 Oct (Day 16) 0 WallClock 60
    , Rule 1989 1989 Jan (Day 29) 0 WallClock 0
    , Rule 1989 1989 Oct (Day 15) 0 WallClock 60
    , Rule 1990 1990 Feb (Day 11) 0 WallClock 0
    , Rule 1990 1990 Oct (Day 21) 0 WallClock 60
    , Rule 1991 1991 Feb (Day 17) 0 WallClock 0
    , Rule 1991 1991 Oct (Day 20) 0 WallClock 60
    , Rule 1992 1992 Feb (Day 9) 0 WallClock 0
    , Rule 1992 1992 Oct (Day 25) 0 WallClock 60
    , Rule 1993 1993 Jan (Day 31) 0 WallClock 0
    , Rule 1993 1995 Oct (Next Sun 11) 0 WallClock 60
    , Rule 1994 1995 Feb (Next Sun 15) 0 WallClock 0
    , Rule 1996 1996 Feb (Day 11) 0 WallClock 0
    , Rule 1996 1996 Oct (Day 6) 0 WallClock 60
    , Rule 1997 1997 Feb (Day 16) 0 WallClock 0
    , Rule 1997 1997 Oct (Day 6) 0 WallClock 60
    , Rule 1998 1998 Mar (Day 1) 0 WallClock 0
    , Rule 1998 1998 Oct (Day 11) 0 WallClock 60
    , Rule 1999 1999 Feb (Day 21) 0 WallClock 0
    , Rule 1999 1999 Oct (Day 3) 0 WallClock 60
    , Rule 2000 2000 Feb (Day 27) 0 WallClock 0
    , Rule 2000 2001 Oct (Next Sun 8) 0 WallClock 60
    , Rule 2001 2006 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2002 2002 Nov (Day 3) 0 WallClock 60
    , Rule 2003 2003 Oct (Day 19) 0 WallClock 60
    , Rule 2004 2004 Nov (Day 2) 0 WallClock 60
    , Rule 2005 2005 Oct (Day 16) 0 WallClock 60
    , Rule 2006 2006 Nov (Day 5) 0 WallClock 60
    , Rule 2007 2007 Feb (Day 25) 0 WallClock 0
    , Rule 2007 2007 Oct (Next Sun 8) 0 WallClock 60
    , Rule 2008 2017 Oct (Next Sun 15) 0 WallClock 60
    , Rule 2008 2011 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2012 2012 Feb (Next Sun 22) 0 WallClock 0
    , Rule 2013 2014 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2015 2015 Feb (Next Sun 22) 0 WallClock 0
    , Rule 2016 2019 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2018 2018 Nov (Next Sun 1) 0 WallClock 60
    ]


rules_Bulg : List Rule
rules_Bulg =
    [ Rule 1979 1979 Mar (Day 31) 1380 WallClock 60
    , Rule 1979 1979 Oct (Day 1) 60 WallClock 0
    , Rule 1980 1982 Apr (Next Sat 1) 1380 WallClock 60
    , Rule 1980 1980 Sep (Day 29) 60 WallClock 0
    , Rule 1981 1981 Sep (Day 27) 120 WallClock 0
    ]


rules_C_Eur : List Rule
rules_C_Eur =
    [ Rule 1977 1980 Apr (Next Sun 1) 120 Standard 60
    , Rule 1977 1977 Sep (Last Sun) 120 Standard 0
    , Rule 1978 1978 Oct (Day 1) 120 Standard 0
    , Rule 1979 1995 Sep (Last Sun) 120 Standard 0
    , Rule 1981 maxYear Mar (Last Sun) 120 Standard 60
    , Rule 1996 maxYear Oct (Last Sun) 120 Standard 0
    ]


rules_CO : List Rule
rules_CO =
    [ Rule 1992 1992 May (Day 3) 0 WallClock 60
    , Rule 1993 1993 Feb (Day 6) 1440 WallClock 0
    ]


rules_CR : List Rule
rules_CR =
    [ Rule 1979 1980 Feb (Last Sun) 0 WallClock 60
    , Rule 1979 1980 Jun (Next Sun 1) 0 WallClock 0
    , Rule 1991 1992 Jan (Next Sat 15) 0 WallClock 60
    , Rule 1991 1991 Jul (Day 1) 0 WallClock 0
    , Rule 1992 1992 Mar (Day 15) 0 WallClock 0
    ]


rules_Canada : List Rule
rules_Canada =
    [ Rule 1974 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1974 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 1987 2006 Apr (Next Sun 1) 120 WallClock 60
    , Rule 2007 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2007 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Chatham : List Rule
rules_Chatham =
    [ Rule 1974 1974 Nov (Next Sun 1) 165 Standard 60
    , Rule 1975 1975 Feb (Last Sun) 165 Standard 0
    , Rule 1975 1988 Oct (Last Sun) 165 Standard 60
    , Rule 1976 1989 Mar (Next Sun 1) 165 Standard 0
    , Rule 1989 1989 Oct (Next Sun 8) 165 Standard 60
    , Rule 1990 2006 Oct (Next Sun 1) 165 Standard 60
    , Rule 1990 2007 Mar (Next Sun 15) 165 Standard 0
    , Rule 2007 maxYear Sep (Last Sun) 165 Standard 60
    , Rule 2008 maxYear Apr (Next Sun 1) 165 Standard 0
    ]


rules_Chile : List Rule
rules_Chile =
    [ Rule 1969 1969 Mar (Day 30) 180 Universal 0
    , Rule 1969 1969 Nov (Day 23) 240 Universal 60
    , Rule 1970 1970 Mar (Day 29) 180 Universal 0
    , Rule 1971 1971 Mar (Day 14) 180 Universal 0
    , Rule 1970 1972 Oct (Next Sun 9) 240 Universal 60
    , Rule 1972 1986 Mar (Next Sun 9) 180 Universal 0
    , Rule 1973 1973 Sep (Day 30) 240 Universal 60
    , Rule 1974 1987 Oct (Next Sun 9) 240 Universal 60
    , Rule 1987 1987 Apr (Day 12) 180 Universal 0
    , Rule 1988 1990 Mar (Next Sun 9) 180 Universal 0
    , Rule 1988 1989 Oct (Next Sun 9) 240 Universal 60
    , Rule 1990 1990 Sep (Day 16) 240 Universal 60
    , Rule 1991 1996 Mar (Next Sun 9) 180 Universal 0
    , Rule 1991 1997 Oct (Next Sun 9) 240 Universal 60
    , Rule 1997 1997 Mar (Day 30) 180 Universal 0
    , Rule 1998 1998 Mar (Next Sun 9) 180 Universal 0
    , Rule 1998 1998 Sep (Day 27) 240 Universal 60
    , Rule 1999 1999 Apr (Day 4) 180 Universal 0
    , Rule 1999 2010 Oct (Next Sun 9) 240 Universal 60
    , Rule 2000 2007 Mar (Next Sun 9) 180 Universal 0
    , Rule 2008 2008 Mar (Day 30) 180 Universal 0
    , Rule 2009 2009 Mar (Next Sun 9) 180 Universal 0
    , Rule 2010 2010 Apr (Next Sun 1) 180 Universal 0
    , Rule 2011 2011 May (Next Sun 2) 180 Universal 0
    , Rule 2011 2011 Aug (Next Sun 16) 240 Universal 60
    , Rule 2012 2014 Apr (Next Sun 23) 180 Universal 0
    , Rule 2012 2014 Sep (Next Sun 2) 240 Universal 60
    , Rule 2016 2018 May (Next Sun 9) 180 Universal 0
    , Rule 2016 2018 Aug (Next Sun 9) 240 Universal 60
    , Rule 2019 maxYear Apr (Next Sun 2) 180 Universal 0
    , Rule 2019 2021 Sep (Next Sun 2) 240 Universal 60
    , Rule 2022 2022 Sep (Next Sun 9) 240 Universal 60
    , Rule 2023 maxYear Sep (Next Sun 2) 240 Universal 60
    ]


rules_Cook : List Rule
rules_Cook =
    [ Rule 1978 1978 Nov (Day 12) 0 WallClock 30
    , Rule 1979 1991 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1979 1990 Oct (Last Sun) 0 WallClock 30
    ]


rules_Cuba : List Rule
rules_Cuba =
    [ Rule 1969 1977 Apr (Last Sun) 0 WallClock 60
    , Rule 1969 1971 Oct (Last Sun) 0 WallClock 0
    , Rule 1972 1974 Oct (Day 8) 0 WallClock 0
    , Rule 1975 1977 Oct (Last Sun) 0 WallClock 0
    , Rule 1978 1978 May (Day 7) 0 WallClock 60
    , Rule 1978 1990 Oct (Next Sun 8) 0 WallClock 0
    , Rule 1979 1980 Mar (Next Sun 15) 0 WallClock 60
    , Rule 1981 1985 May (Next Sun 5) 0 WallClock 60
    , Rule 1986 1989 Mar (Next Sun 14) 0 WallClock 60
    , Rule 1990 1997 Apr (Next Sun 1) 0 WallClock 60
    , Rule 1991 1995 Oct (Next Sun 8) 0 Standard 0
    , Rule 1996 1996 Oct (Day 6) 0 Standard 0
    , Rule 1997 1997 Oct (Day 12) 0 Standard 0
    , Rule 1998 1999 Mar (Last Sun) 0 Standard 60
    , Rule 1998 2003 Oct (Last Sun) 0 Standard 0
    , Rule 2000 2003 Apr (Next Sun 1) 0 Standard 60
    , Rule 2004 2004 Mar (Last Sun) 0 Standard 60
    , Rule 2006 2010 Oct (Last Sun) 0 Standard 0
    , Rule 2007 2007 Mar (Next Sun 8) 0 Standard 60
    , Rule 2008 2008 Mar (Next Sun 15) 0 Standard 60
    , Rule 2009 2010 Mar (Next Sun 8) 0 Standard 60
    , Rule 2011 2011 Mar (Next Sun 15) 0 Standard 60
    , Rule 2011 2011 Nov (Day 13) 0 Standard 0
    , Rule 2012 2012 Apr (Day 1) 0 Standard 60
    , Rule 2012 maxYear Nov (Next Sun 1) 0 Standard 0
    , Rule 2013 maxYear Mar (Next Sun 8) 0 Standard 60
    ]


rules_Cyprus : List Rule
rules_Cyprus =
    [ Rule 1975 1975 Apr (Day 13) 0 WallClock 60
    , Rule 1975 1975 Oct (Day 12) 0 WallClock 0
    , Rule 1976 1976 May (Day 15) 0 WallClock 60
    , Rule 1976 1976 Oct (Day 11) 0 WallClock 0
    , Rule 1977 1980 Apr (Next Sun 1) 0 WallClock 60
    , Rule 1977 1977 Sep (Day 25) 0 WallClock 0
    , Rule 1978 1978 Oct (Day 2) 0 WallClock 0
    , Rule 1979 1997 Sep (Last Sun) 0 WallClock 0
    , Rule 1981 1998 Mar (Last Sun) 0 WallClock 60
    ]


rules_DR : List Rule
rules_DR =
    [ Rule 1969 1973 Oct (Last Sun) 0 WallClock 30
    , Rule 1970 1970 Feb (Day 21) 0 WallClock 0
    , Rule 1971 1971 Jan (Day 20) 0 WallClock 0
    , Rule 1972 1974 Jan (Day 21) 0 WallClock 0
    ]


rules_Dhaka : List Rule
rules_Dhaka =
    [ Rule 2009 2009 Jun (Day 19) 1380 WallClock 60
    , Rule 2009 2009 Dec (Day 31) 1440 WallClock 0
    ]


rules_E_Eur : List Rule
rules_E_Eur =
    [ Rule 1977 1980 Apr (Next Sun 1) 0 WallClock 60
    , Rule 1977 1977 Sep (Last Sun) 0 WallClock 0
    , Rule 1978 1978 Oct (Day 1) 0 WallClock 0
    , Rule 1979 1995 Sep (Last Sun) 0 WallClock 0
    , Rule 1981 maxYear Mar (Last Sun) 0 WallClock 60
    , Rule 1996 maxYear Oct (Last Sun) 0 WallClock 0
    ]


rules_E_EurAsia : List Rule
rules_E_EurAsia =
    [ Rule 1981 maxYear Mar (Last Sun) 0 WallClock 60
    , Rule 1979 1995 Sep (Last Sun) 0 WallClock 0
    , Rule 1996 maxYear Oct (Last Sun) 0 WallClock 0
    ]


rules_EU : List Rule
rules_EU =
    [ Rule 1977 1980 Apr (Next Sun 1) 60 Universal 60
    , Rule 1977 1977 Sep (Last Sun) 60 Universal 0
    , Rule 1978 1978 Oct (Day 1) 60 Universal 0
    , Rule 1979 1995 Sep (Last Sun) 60 Universal 0
    , Rule 1981 maxYear Mar (Last Sun) 60 Universal 60
    , Rule 1996 maxYear Oct (Last Sun) 60 Universal 0
    ]


rules_EUAsia : List Rule
rules_EUAsia =
    [ Rule 1981 maxYear Mar (Last Sun) 60 Universal 60
    , Rule 1979 1995 Sep (Last Sun) 60 Universal 0
    , Rule 1996 maxYear Oct (Last Sun) 60 Universal 0
    ]


rules_Ecuador : List Rule
rules_Ecuador =
    [ Rule 1992 1992 Nov (Day 28) 0 WallClock 60
    , Rule 1993 1993 Feb (Day 5) 0 WallClock 0
    ]


rules_Edm : List Rule
rules_Edm =
    [ Rule 1972 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1972 2006 Oct (Last Sun) 120 WallClock 0
    ]


rules_Egypt : List Rule
rules_Egypt =
    [ Rule 1959 1981 May (Day 1) 60 WallClock 60
    , Rule 1966 1994 Oct (Day 1) 180 WallClock 0
    , Rule 1982 1982 Jul (Day 25) 60 WallClock 60
    , Rule 1983 1983 Jul (Day 12) 60 WallClock 60
    , Rule 1984 1988 May (Day 1) 60 WallClock 60
    , Rule 1989 1989 May (Day 6) 60 WallClock 60
    , Rule 1990 1994 May (Day 1) 60 WallClock 60
    , Rule 1995 2010 Apr (Last Fri) 0 Standard 60
    , Rule 1995 2005 Sep (Last Thu) 1440 WallClock 0
    , Rule 2006 2006 Sep (Day 21) 1440 WallClock 0
    , Rule 2007 2007 Sep (Next Thu 1) 1440 WallClock 0
    , Rule 2008 2008 Aug (Last Thu) 1440 WallClock 0
    , Rule 2009 2009 Aug (Day 20) 1440 WallClock 0
    , Rule 2010 2010 Aug (Day 10) 1440 WallClock 0
    , Rule 2010 2010 Sep (Day 9) 1440 WallClock 60
    , Rule 2010 2010 Sep (Last Thu) 1440 WallClock 0
    , Rule 2014 2014 May (Day 15) 1440 WallClock 60
    , Rule 2014 2014 Jun (Day 26) 1440 WallClock 0
    , Rule 2014 2014 Jul (Day 31) 1440 WallClock 60
    , Rule 2014 2014 Sep (Last Thu) 1440 WallClock 0
    , Rule 2023 maxYear Apr (Last Fri) 0 WallClock 60
    , Rule 2023 maxYear Oct (Last Thu) 1440 WallClock 0
    ]


rules_Eire : List Rule
rules_Eire =
    [ Rule 1971 1971 Oct (Day 31) 120 Universal -60
    , Rule 1972 1980 Mar (Next Sun 16) 120 Universal 0
    , Rule 1972 1980 Oct (Next Sun 23) 120 Universal -60
    , Rule 1981 maxYear Mar (Last Sun) 60 Universal 0
    , Rule 1981 1989 Oct (Next Sun 23) 60 Universal -60
    , Rule 1990 1995 Oct (Next Sun 22) 60 Universal -60
    , Rule 1996 maxYear Oct (Last Sun) 60 Universal -60
    ]


rules_Falk : List Rule
rules_Falk =
    [ Rule 1983 1983 Sep (Last Sun) 0 WallClock 60
    , Rule 1984 1985 Apr (Last Sun) 0 WallClock 0
    , Rule 1984 1984 Sep (Day 16) 0 WallClock 60
    , Rule 1985 2000 Sep (Next Sun 9) 0 WallClock 60
    , Rule 1986 2000 Apr (Next Sun 16) 0 WallClock 0
    , Rule 2001 2010 Apr (Next Sun 15) 120 WallClock 0
    , Rule 2001 2010 Sep (Next Sun 1) 120 WallClock 60
    ]


rules_Fiji : List Rule
rules_Fiji =
    [ Rule 1998 1999 Nov (Next Sun 1) 120 WallClock 60
    , Rule 1999 2000 Feb (Last Sun) 180 WallClock 0
    , Rule 2009 2009 Nov (Day 29) 120 WallClock 60
    , Rule 2010 2010 Mar (Last Sun) 180 WallClock 0
    , Rule 2010 2013 Oct (Next Sun 21) 120 WallClock 60
    , Rule 2011 2011 Mar (Next Sun 1) 180 WallClock 0
    , Rule 2012 2013 Jan (Next Sun 18) 180 WallClock 0
    , Rule 2014 2014 Jan (Next Sun 18) 120 WallClock 0
    , Rule 2014 2018 Nov (Next Sun 1) 120 WallClock 60
    , Rule 2015 2021 Jan (Next Sun 12) 180 WallClock 0
    , Rule 2019 2019 Nov (Next Sun 8) 120 WallClock 60
    , Rule 2020 2020 Dec (Day 20) 120 WallClock 60
    ]


rules_Finland : List Rule
rules_Finland =
    [ Rule 1981 1982 Mar (Last Sun) 120 WallClock 60
    , Rule 1981 1982 Sep (Last Sun) 180 WallClock 0
    ]


rules_France : List Rule
rules_France =
    [ Rule 1976 1976 Mar (Day 28) 60 WallClock 60
    , Rule 1976 1976 Sep (Day 26) 60 WallClock 0
    ]


rules_GB_Eire : List Rule
rules_GB_Eire =
    [ Rule 1972 1980 Mar (Next Sun 16) 120 Standard 60
    , Rule 1972 1980 Oct (Next Sun 23) 120 Standard 0
    , Rule 1981 1995 Mar (Last Sun) 60 Universal 60
    , Rule 1981 1989 Oct (Next Sun 23) 60 Universal 0
    , Rule 1990 1995 Oct (Next Sun 22) 60 Universal 0
    ]


rules_Greece : List Rule
rules_Greece =
    [ Rule 1975 1975 Apr (Day 12) 0 Standard 60
    , Rule 1975 1975 Nov (Day 26) 0 Standard 0
    , Rule 1976 1976 Apr (Day 11) 120 Standard 60
    , Rule 1976 1976 Oct (Day 10) 120 Standard 0
    , Rule 1977 1978 Apr (Next Sun 1) 120 Standard 60
    , Rule 1977 1977 Sep (Day 26) 120 Standard 0
    , Rule 1978 1978 Sep (Day 24) 240 WallClock 0
    , Rule 1979 1979 Apr (Day 1) 540 WallClock 60
    , Rule 1979 1979 Sep (Day 29) 120 WallClock 0
    , Rule 1980 1980 Apr (Day 1) 0 WallClock 60
    , Rule 1980 1980 Sep (Day 28) 0 WallClock 0
    ]


rules_Guam : List Rule
rules_Guam =
    [ Rule 1969 1969 Jan (Day 26) 1 WallClock 0
    , Rule 1969 1969 Jun (Day 22) 120 WallClock 60
    , Rule 1969 1969 Aug (Day 31) 120 WallClock 0
    , Rule 1970 1971 Apr (Last Sun) 120 WallClock 60
    , Rule 1970 1971 Sep (Next Sun 1) 120 WallClock 0
    , Rule 1973 1973 Dec (Day 16) 120 WallClock 60
    , Rule 1974 1974 Feb (Day 24) 120 WallClock 0
    , Rule 1976 1976 May (Day 26) 120 WallClock 60
    , Rule 1976 1976 Aug (Day 22) 121 WallClock 0
    , Rule 1977 1977 Apr (Day 24) 120 WallClock 60
    , Rule 1977 1977 Aug (Day 28) 120 WallClock 0
    ]


rules_Guat : List Rule
rules_Guat =
    [ Rule 1973 1973 Nov (Day 25) 0 WallClock 60
    , Rule 1974 1974 Feb (Day 24) 0 WallClock 0
    , Rule 1983 1983 May (Day 21) 0 WallClock 60
    , Rule 1983 1983 Sep (Day 22) 0 WallClock 0
    , Rule 1991 1991 Mar (Day 23) 0 WallClock 60
    , Rule 1991 1991 Sep (Day 7) 0 WallClock 0
    , Rule 2006 2006 Apr (Day 30) 0 WallClock 60
    , Rule 2006 2006 Oct (Day 1) 0 WallClock 0
    ]


rules_HK : List Rule
rules_HK =
    [ Rule 1965 1976 Apr (Next Sun 16) 210 WallClock 60
    , Rule 1965 1976 Oct (Next Sun 16) 210 WallClock 0
    , Rule 1973 1973 Dec (Day 30) 210 WallClock 60
    , Rule 1979 1979 May (Day 13) 210 WallClock 60
    , Rule 1979 1979 Oct (Day 21) 210 WallClock 0
    ]


rules_Haiti : List Rule
rules_Haiti =
    [ Rule 1983 1983 May (Day 8) 0 WallClock 60
    , Rule 1984 1987 Apr (Last Sun) 0 WallClock 60
    , Rule 1983 1987 Oct (Last Sun) 0 WallClock 0
    , Rule 1988 1997 Apr (Next Sun 1) 60 Standard 60
    , Rule 1988 1997 Oct (Last Sun) 60 Standard 0
    , Rule 2005 2006 Apr (Next Sun 1) 0 WallClock 60
    , Rule 2005 2006 Oct (Last Sun) 0 WallClock 0
    , Rule 2012 2015 Mar (Next Sun 8) 120 WallClock 60
    , Rule 2012 2015 Nov (Next Sun 1) 120 WallClock 0
    , Rule 2017 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2017 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Halifax : List Rule
rules_Halifax =
    [ Rule 1962 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1962 1973 Oct (Last Sun) 120 WallClock 0
    ]


rules_Holiday : List Rule
rules_Holiday =
    [ Rule 1992 1993 Oct (Last Sun) 120 Standard 60
    , Rule 1993 1994 Mar (Next Sun 1) 120 Standard 0
    ]


rules_Hond : List Rule
rules_Hond =
    [ Rule 1987 1988 May (Next Sun 1) 0 WallClock 60
    , Rule 1987 1988 Sep (Last Sun) 0 WallClock 0
    , Rule 2006 2006 May (Next Sun 1) 0 WallClock 60
    , Rule 2006 2006 Aug (Next Mon 1) 0 WallClock 0
    ]


rules_Hungary : List Rule
rules_Hungary =
    [ Rule 1980 1980 Apr (Day 6) 0 WallClock 60
    , Rule 1980 1980 Sep (Day 28) 60 WallClock 0
    , Rule 1981 1983 Mar (Last Sun) 0 WallClock 60
    , Rule 1981 1983 Sep (Last Sun) 60 WallClock 0
    ]


rules_Iran : List Rule
rules_Iran =
    [ Rule 1977 1977 Mar (Day 21) 1380 WallClock 60
    , Rule 1977 1977 Oct (Day 20) 1440 WallClock 0
    , Rule 1978 1978 Mar (Day 24) 1440 WallClock 60
    , Rule 1978 1978 Aug (Day 5) 60 WallClock 0
    , Rule 1979 1979 May (Day 26) 1440 WallClock 60
    , Rule 1979 1979 Sep (Day 18) 1440 WallClock 0
    , Rule 1980 1980 Mar (Day 20) 1440 WallClock 60
    , Rule 1980 1980 Sep (Day 22) 1440 WallClock 0
    , Rule 1991 1991 May (Day 2) 1440 WallClock 60
    , Rule 1992 1995 Mar (Day 21) 1440 WallClock 60
    , Rule 1991 1995 Sep (Day 21) 1440 WallClock 0
    , Rule 1996 1996 Mar (Day 20) 1440 WallClock 60
    , Rule 1996 1996 Sep (Day 20) 1440 WallClock 0
    , Rule 1997 1999 Mar (Day 21) 1440 WallClock 60
    , Rule 1997 1999 Sep (Day 21) 1440 WallClock 0
    , Rule 2000 2000 Mar (Day 20) 1440 WallClock 60
    , Rule 2000 2000 Sep (Day 20) 1440 WallClock 0
    , Rule 2001 2003 Mar (Day 21) 1440 WallClock 60
    , Rule 2001 2003 Sep (Day 21) 1440 WallClock 0
    , Rule 2004 2004 Mar (Day 20) 1440 WallClock 60
    , Rule 2004 2004 Sep (Day 20) 1440 WallClock 0
    , Rule 2005 2005 Mar (Day 21) 1440 WallClock 60
    , Rule 2005 2005 Sep (Day 21) 1440 WallClock 0
    , Rule 2008 2008 Mar (Day 20) 1440 WallClock 60
    , Rule 2008 2008 Sep (Day 20) 1440 WallClock 0
    , Rule 2009 2011 Mar (Day 21) 1440 WallClock 60
    , Rule 2009 2011 Sep (Day 21) 1440 WallClock 0
    , Rule 2012 2012 Mar (Day 20) 1440 WallClock 60
    , Rule 2012 2012 Sep (Day 20) 1440 WallClock 0
    , Rule 2013 2015 Mar (Day 21) 1440 WallClock 60
    , Rule 2013 2015 Sep (Day 21) 1440 WallClock 0
    , Rule 2016 2016 Mar (Day 20) 1440 WallClock 60
    , Rule 2016 2016 Sep (Day 20) 1440 WallClock 0
    , Rule 2017 2019 Mar (Day 21) 1440 WallClock 60
    , Rule 2017 2019 Sep (Day 21) 1440 WallClock 0
    , Rule 2020 2020 Mar (Day 20) 1440 WallClock 60
    , Rule 2020 2020 Sep (Day 20) 1440 WallClock 0
    , Rule 2021 2022 Mar (Day 21) 1440 WallClock 60
    , Rule 2021 2022 Sep (Day 21) 1440 WallClock 0
    ]


rules_Iraq : List Rule
rules_Iraq =
    [ Rule 1982 1982 May (Day 1) 0 WallClock 60
    , Rule 1982 1984 Oct (Day 1) 0 WallClock 0
    , Rule 1983 1983 Mar (Day 31) 0 WallClock 60
    , Rule 1984 1985 Apr (Day 1) 0 WallClock 60
    , Rule 1985 1990 Sep (Last Sun) 60 Standard 0
    , Rule 1986 1990 Mar (Last Sun) 60 Standard 60
    , Rule 1991 2007 Apr (Day 1) 180 Standard 60
    , Rule 1991 2007 Oct (Day 1) 180 Standard 0
    ]


rules_Italy : List Rule
rules_Italy =
    [ Rule 1967 1969 Sep (Next Sun 22) 0 Standard 0
    , Rule 1969 1969 Jun (Day 1) 0 Standard 60
    , Rule 1970 1970 May (Day 31) 0 Standard 60
    , Rule 1970 1970 Sep (Last Sun) 0 Standard 0
    , Rule 1971 1972 May (Next Sun 22) 0 Standard 60
    , Rule 1971 1971 Sep (Last Sun) 0 Standard 0
    , Rule 1972 1972 Oct (Day 1) 0 Standard 0
    , Rule 1973 1973 Jun (Day 3) 0 Standard 60
    , Rule 1973 1974 Sep (Last Sun) 0 Standard 0
    , Rule 1974 1974 May (Day 26) 0 Standard 60
    , Rule 1975 1975 Jun (Day 1) 0 Standard 60
    , Rule 1975 1977 Sep (Last Sun) 0 Standard 0
    , Rule 1976 1976 May (Day 30) 0 Standard 60
    , Rule 1977 1979 May (Next Sun 22) 0 Standard 60
    , Rule 1978 1978 Oct (Day 1) 0 Standard 0
    , Rule 1979 1979 Sep (Day 30) 0 Standard 0
    ]


rules_Jordan : List Rule
rules_Jordan =
    [ Rule 1973 1973 Jun (Day 6) 0 WallClock 60
    , Rule 1973 1975 Oct (Day 1) 0 WallClock 0
    , Rule 1974 1977 May (Day 1) 0 WallClock 60
    , Rule 1976 1976 Nov (Day 1) 0 WallClock 0
    , Rule 1977 1977 Oct (Day 1) 0 WallClock 0
    , Rule 1978 1978 Apr (Day 30) 0 WallClock 60
    , Rule 1978 1978 Sep (Day 30) 0 WallClock 0
    , Rule 1985 1985 Apr (Day 1) 0 WallClock 60
    , Rule 1985 1985 Oct (Day 1) 0 WallClock 0
    , Rule 1986 1988 Apr (Next Fri 1) 0 WallClock 60
    , Rule 1986 1990 Oct (Next Fri 1) 0 WallClock 0
    , Rule 1989 1989 May (Day 8) 0 WallClock 60
    , Rule 1990 1990 Apr (Day 27) 0 WallClock 60
    , Rule 1991 1991 Apr (Day 17) 0 WallClock 60
    , Rule 1991 1991 Sep (Day 27) 0 WallClock 0
    , Rule 1992 1992 Apr (Day 10) 0 WallClock 60
    , Rule 1992 1993 Oct (Next Fri 1) 0 WallClock 0
    , Rule 1993 1998 Apr (Next Fri 1) 0 WallClock 60
    , Rule 1994 1994 Sep (Next Fri 15) 0 WallClock 0
    , Rule 1995 1998 Sep (Next Fri 15) 0 Standard 0
    , Rule 1999 1999 Jul (Day 1) 0 Standard 60
    , Rule 1999 2002 Sep (Last Fri) 0 Standard 0
    , Rule 2000 2001 Mar (Last Thu) 0 Standard 60
    , Rule 2002 2012 Mar (Last Thu) 1440 WallClock 60
    , Rule 2003 2003 Oct (Day 24) 0 Standard 0
    , Rule 2004 2004 Oct (Day 15) 0 Standard 0
    , Rule 2005 2005 Sep (Last Fri) 0 Standard 0
    , Rule 2006 2011 Oct (Last Fri) 0 Standard 0
    , Rule 2013 2013 Dec (Day 20) 0 WallClock 0
    , Rule 2014 2021 Mar (Last Thu) 1440 WallClock 60
    , Rule 2014 2022 Oct (Last Fri) 0 Standard 0
    , Rule 2022 2022 Feb (Last Thu) 1440 WallClock 60
    ]


rules_Kyrgyz : List Rule
rules_Kyrgyz =
    [ Rule 1992 1996 Apr (Next Sun 7) 0 Standard 60
    , Rule 1992 1996 Sep (Last Sun) 0 WallClock 0
    , Rule 1997 2005 Mar (Last Sun) 150 WallClock 60
    , Rule 1997 2004 Oct (Last Sun) 150 WallClock 0
    ]


rules_LH : List Rule
rules_LH =
    [ Rule 1981 1984 Oct (Last Sun) 120 WallClock 60
    , Rule 1982 1985 Mar (Next Sun 1) 120 WallClock 0
    , Rule 1985 1985 Oct (Last Sun) 120 WallClock 30
    , Rule 1986 1989 Mar (Next Sun 15) 120 WallClock 0
    , Rule 1986 1986 Oct (Day 19) 120 WallClock 30
    , Rule 1987 1999 Oct (Last Sun) 120 WallClock 30
    , Rule 1990 1995 Mar (Next Sun 1) 120 WallClock 0
    , Rule 1996 2005 Mar (Last Sun) 120 WallClock 0
    , Rule 2000 2000 Aug (Last Sun) 120 WallClock 30
    , Rule 2001 2007 Oct (Last Sun) 120 WallClock 30
    , Rule 2006 2006 Apr (Next Sun 1) 120 WallClock 0
    , Rule 2007 2007 Mar (Last Sun) 120 WallClock 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 WallClock 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 WallClock 30
    ]


rules_Latvia : List Rule
rules_Latvia =
    [ Rule 1989 1996 Mar (Last Sun) 120 Standard 60
    , Rule 1989 1996 Sep (Last Sun) 120 Standard 0
    ]


rules_Lebanon : List Rule
rules_Lebanon =
    [ Rule 1972 1972 Jun (Day 22) 0 WallClock 60
    , Rule 1972 1977 Oct (Day 1) 0 WallClock 0
    , Rule 1973 1977 May (Day 1) 0 WallClock 60
    , Rule 1978 1978 Apr (Day 30) 0 WallClock 60
    , Rule 1978 1978 Sep (Day 30) 0 WallClock 0
    , Rule 1984 1987 May (Day 1) 0 WallClock 60
    , Rule 1984 1991 Oct (Day 16) 0 WallClock 0
    , Rule 1988 1988 Jun (Day 1) 0 WallClock 60
    , Rule 1989 1989 May (Day 10) 0 WallClock 60
    , Rule 1990 1992 May (Day 1) 0 WallClock 60
    , Rule 1992 1992 Oct (Day 4) 0 WallClock 0
    , Rule 1993 maxYear Mar (Last Sun) 0 WallClock 60
    , Rule 1993 1998 Sep (Last Sun) 0 WallClock 0
    , Rule 1999 maxYear Oct (Last Sun) 0 WallClock 0
    ]


rules_Libya : List Rule
rules_Libya =
    [ Rule 1982 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1982 1985 Oct (Day 1) 0 WallClock 0
    , Rule 1985 1985 Apr (Day 6) 0 WallClock 60
    , Rule 1986 1986 Apr (Day 4) 0 WallClock 60
    , Rule 1986 1986 Oct (Day 3) 0 WallClock 0
    , Rule 1987 1989 Apr (Day 1) 0 WallClock 60
    , Rule 1987 1989 Oct (Day 1) 0 WallClock 0
    , Rule 1997 1997 Apr (Day 4) 0 WallClock 60
    , Rule 1997 1997 Oct (Day 4) 0 WallClock 0
    , Rule 2013 2013 Mar (Last Fri) 60 WallClock 60
    , Rule 2013 2013 Oct (Last Fri) 120 WallClock 0
    ]


rules_Macau : List Rule
rules_Macau =
    [ Rule 1965 1973 Apr (Next Sun 16) 210 WallClock 60
    , Rule 1967 1976 Oct (Next Sun 16) 210 WallClock 0
    , Rule 1973 1973 Dec (Day 30) 210 WallClock 60
    , Rule 1975 1976 Apr (Next Sun 16) 210 WallClock 60
    , Rule 1979 1979 May (Day 13) 210 WallClock 60
    , Rule 1979 1979 Oct (Next Sun 16) 210 WallClock 0
    ]


rules_Malta : List Rule
rules_Malta =
    [ Rule 1973 1973 Mar (Day 31) 0 Standard 60
    , Rule 1973 1973 Sep (Day 29) 0 Standard 0
    , Rule 1974 1974 Apr (Day 21) 0 Standard 60
    , Rule 1974 1974 Sep (Day 16) 0 Standard 0
    , Rule 1975 1979 Apr (Next Sun 15) 120 WallClock 60
    , Rule 1975 1980 Sep (Next Sun 15) 120 WallClock 0
    , Rule 1980 1980 Mar (Day 31) 120 WallClock 60
    ]


rules_Mauritius : List Rule
rules_Mauritius =
    [ Rule 1982 1982 Oct (Day 10) 0 WallClock 60
    , Rule 1983 1983 Mar (Day 21) 0 WallClock 0
    , Rule 2008 2008 Oct (Last Sun) 120 WallClock 60
    , Rule 2009 2009 Mar (Last Sun) 120 WallClock 0
    ]


rules_Mexico : List Rule
rules_Mexico =
    [ Rule 1996 2000 Apr (Next Sun 1) 120 WallClock 60
    , Rule 1996 2000 Oct (Last Sun) 120 WallClock 0
    , Rule 2001 2001 May (Next Sun 1) 120 WallClock 60
    , Rule 2001 2001 Sep (Last Sun) 120 WallClock 0
    , Rule 2002 2022 Apr (Next Sun 1) 120 WallClock 60
    , Rule 2002 2022 Oct (Last Sun) 120 WallClock 0
    ]


rules_Moldova : List Rule
rules_Moldova =
    [ Rule 1997 maxYear Mar (Last Sun) 120 WallClock 60
    , Rule 1997 maxYear Oct (Last Sun) 180 WallClock 0
    ]


rules_Moncton : List Rule
rules_Moncton =
    [ Rule 1946 1972 Apr (Last Sun) 120 WallClock 60
    , Rule 1957 1972 Oct (Last Sun) 120 WallClock 0
    , Rule 1993 2006 Apr (Next Sun 1) 1 WallClock 60
    , Rule 1993 2006 Oct (Last Sun) 1 WallClock 0
    ]


rules_Mongol : List Rule
rules_Mongol =
    [ Rule 1983 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1983 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1985 1998 Mar (Last Sun) 0 WallClock 60
    , Rule 1984 1998 Sep (Last Sun) 0 WallClock 0
    , Rule 2001 2001 Apr (Last Sat) 120 WallClock 60
    , Rule 2001 2006 Sep (Last Sat) 120 WallClock 0
    , Rule 2002 2006 Mar (Last Sat) 120 WallClock 60
    , Rule 2015 2016 Mar (Last Sat) 120 WallClock 60
    , Rule 2015 2016 Sep (Last Sat) 0 WallClock 0
    ]


rules_Mont : List Rule
rules_Mont =
    [ Rule 1946 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1957 1973 Oct (Last Sun) 120 WallClock 0
    ]


rules_Morocco : List Rule
rules_Morocco =
    [ Rule 1974 1974 Jun (Day 24) 0 WallClock 60
    , Rule 1974 1974 Sep (Day 1) 0 WallClock 0
    , Rule 1976 1977 May (Day 1) 0 WallClock 60
    , Rule 1976 1976 Aug (Day 1) 0 WallClock 0
    , Rule 1977 1977 Sep (Day 28) 0 WallClock 0
    , Rule 1978 1978 Jun (Day 1) 0 WallClock 60
    , Rule 1978 1978 Aug (Day 4) 0 WallClock 0
    , Rule 2008 2008 Jun (Day 1) 0 WallClock 60
    , Rule 2008 2008 Sep (Day 1) 0 WallClock 0
    , Rule 2009 2009 Jun (Day 1) 0 WallClock 60
    , Rule 2009 2009 Aug (Day 21) 0 WallClock 0
    , Rule 2010 2010 May (Day 2) 0 WallClock 60
    , Rule 2010 2010 Aug (Day 8) 0 WallClock 0
    , Rule 2011 2011 Apr (Day 3) 0 WallClock 60
    , Rule 2011 2011 Jul (Day 31) 0 WallClock 0
    , Rule 2012 2013 Apr (Last Sun) 120 WallClock 60
    , Rule 2012 2012 Jul (Day 20) 180 WallClock 0
    , Rule 2012 2012 Aug (Day 20) 120 WallClock 60
    , Rule 2012 2012 Sep (Day 30) 180 WallClock 0
    , Rule 2013 2013 Jul (Day 7) 180 WallClock 0
    , Rule 2013 2013 Aug (Day 10) 120 WallClock 60
    , Rule 2013 2018 Oct (Last Sun) 180 WallClock 0
    , Rule 2014 2018 Mar (Last Sun) 120 WallClock 60
    , Rule 2014 2014 Jun (Day 28) 180 WallClock 0
    , Rule 2014 2014 Aug (Day 2) 120 WallClock 60
    , Rule 2015 2015 Jun (Day 14) 180 WallClock 0
    , Rule 2015 2015 Jul (Day 19) 120 WallClock 60
    , Rule 2016 2016 Jun (Day 5) 180 WallClock 0
    , Rule 2016 2016 Jul (Day 10) 120 WallClock 60
    , Rule 2017 2017 May (Day 21) 180 WallClock 0
    , Rule 2017 2017 Jul (Day 2) 120 WallClock 60
    , Rule 2018 2018 May (Day 13) 180 WallClock 0
    , Rule 2018 2018 Jun (Day 17) 120 WallClock 60
    , Rule 2019 2019 May (Day 5) 180 WallClock -60
    , Rule 2019 2019 Jun (Day 9) 120 WallClock 0
    , Rule 2020 2020 Apr (Day 19) 180 WallClock -60
    , Rule 2020 2020 May (Day 31) 120 WallClock 0
    , Rule 2021 2021 Apr (Day 11) 180 WallClock -60
    , Rule 2021 2021 May (Day 16) 120 WallClock 0
    , Rule 2022 2022 Mar (Day 27) 180 WallClock -60
    , Rule 2022 2022 May (Day 8) 120 WallClock 0
    , Rule 2023 2023 Mar (Day 19) 180 WallClock -60
    , Rule 2023 2023 Apr (Day 23) 120 WallClock 0
    , Rule 2024 2024 Mar (Day 10) 180 WallClock -60
    , Rule 2024 2024 Apr (Day 14) 120 WallClock 0
    , Rule 2025 2025 Feb (Day 23) 180 WallClock -60
    , Rule 2025 2025 Apr (Day 6) 120 WallClock 0
    , Rule 2026 2026 Feb (Day 15) 180 WallClock -60
    , Rule 2026 2026 Mar (Day 22) 120 WallClock 0
    , Rule 2027 2027 Feb (Day 7) 180 WallClock -60
    , Rule 2027 2027 Mar (Day 14) 120 WallClock 0
    , Rule 2028 2028 Jan (Day 23) 180 WallClock -60
    , Rule 2028 2028 Mar (Day 5) 120 WallClock 0
    , Rule 2029 2029 Jan (Day 14) 180 WallClock -60
    , Rule 2029 2029 Feb (Day 18) 120 WallClock 0
    , Rule 2029 2029 Dec (Day 30) 180 WallClock -60
    , Rule 2030 2030 Feb (Day 10) 120 WallClock 0
    , Rule 2030 2030 Dec (Day 22) 180 WallClock -60
    , Rule 2031 2031 Jan (Day 26) 120 WallClock 0
    , Rule 2031 2031 Dec (Day 14) 180 WallClock -60
    , Rule 2032 2032 Jan (Day 18) 120 WallClock 0
    , Rule 2032 2032 Nov (Day 28) 180 WallClock -60
    , Rule 2033 2033 Jan (Day 9) 120 WallClock 0
    , Rule 2033 2033 Nov (Day 20) 180 WallClock -60
    , Rule 2033 2033 Dec (Day 25) 120 WallClock 0
    , Rule 2034 2034 Nov (Day 5) 180 WallClock -60
    , Rule 2034 2034 Dec (Day 17) 120 WallClock 0
    , Rule 2035 2035 Oct (Day 28) 180 WallClock -60
    , Rule 2035 2035 Dec (Day 9) 120 WallClock 0
    , Rule 2036 2036 Oct (Day 19) 180 WallClock -60
    , Rule 2036 2036 Nov (Day 23) 120 WallClock 0
    , Rule 2037 2037 Oct (Day 4) 180 WallClock -60
    , Rule 2037 2037 Nov (Day 15) 120 WallClock 0
    ]


rules_NC : List Rule
rules_NC =
    [ Rule 1977 1978 Dec (Next Sun 1) 0 WallClock 60
    , Rule 1978 1979 Feb (Day 27) 0 WallClock 0
    , Rule 1996 1996 Dec (Day 1) 120 Standard 60
    , Rule 1997 1997 Mar (Day 2) 120 Standard 0
    ]


rules_NT_YK : List Rule
rules_NT_YK =
    [ Rule 1972 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1972 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 1987 2006 Apr (Next Sun 1) 120 WallClock 60
    ]


rules_NZ : List Rule
rules_NZ =
    [ Rule 1974 1974 Nov (Next Sun 1) 120 Standard 60
    , Rule 1975 1975 Feb (Last Sun) 120 Standard 0
    , Rule 1975 1988 Oct (Last Sun) 120 Standard 60
    , Rule 1976 1989 Mar (Next Sun 1) 120 Standard 0
    , Rule 1989 1989 Oct (Next Sun 8) 120 Standard 60
    , Rule 1990 2006 Oct (Next Sun 1) 120 Standard 60
    , Rule 1990 2007 Mar (Next Sun 15) 120 Standard 0
    , Rule 2007 maxYear Sep (Last Sun) 120 Standard 60
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    ]


rules_Namibia : List Rule
rules_Namibia =
    [ Rule 1994 1994 Mar (Day 21) 0 WallClock -60
    , Rule 1994 2017 Sep (Next Sun 1) 120 WallClock 0
    , Rule 1995 2017 Apr (Next Sun 1) 120 WallClock -60
    ]


rules_Nic : List Rule
rules_Nic =
    [ Rule 1979 1980 Mar (Next Sun 16) 0 WallClock 60
    , Rule 1979 1980 Jun (Next Mon 23) 0 WallClock 0
    , Rule 2005 2005 Apr (Day 10) 0 WallClock 60
    , Rule 2005 2005 Oct (Next Sun 1) 0 WallClock 0
    , Rule 2006 2006 Apr (Day 30) 120 WallClock 60
    , Rule 2006 2006 Oct (Next Sun 1) 60 WallClock 0
    ]


rules_PRC : List Rule
rules_PRC =
    [ Rule 1986 1986 May (Day 4) 120 WallClock 60
    , Rule 1986 1991 Sep (Next Sun 11) 120 WallClock 0
    , Rule 1987 1991 Apr (Next Sun 11) 120 WallClock 60
    ]


rules_Pakistan : List Rule
rules_Pakistan =
    [ Rule 2002 2002 Apr (Next Sun 2) 0 WallClock 60
    , Rule 2002 2002 Oct (Next Sun 2) 0 WallClock 0
    , Rule 2008 2008 Jun (Day 1) 0 WallClock 60
    , Rule 2008 2009 Nov (Day 1) 0 WallClock 0
    , Rule 2009 2009 Apr (Day 15) 0 WallClock 60
    ]


rules_Palestine : List Rule
rules_Palestine =
    [ Rule 1999 2005 Apr (Next Fri 15) 0 WallClock 60
    , Rule 1999 2003 Oct (Next Fri 15) 0 WallClock 0
    , Rule 2004 2004 Oct (Day 1) 60 WallClock 0
    , Rule 2005 2005 Oct (Day 4) 120 WallClock 0
    , Rule 2006 2007 Apr (Day 1) 0 WallClock 60
    , Rule 2006 2006 Sep (Day 22) 0 WallClock 0
    , Rule 2007 2007 Sep (Day 13) 120 WallClock 0
    , Rule 2008 2009 Mar (Last Fri) 0 WallClock 60
    , Rule 2008 2008 Sep (Day 1) 0 WallClock 0
    , Rule 2009 2009 Sep (Day 4) 60 WallClock 0
    , Rule 2010 2010 Mar (Day 26) 0 WallClock 60
    , Rule 2010 2010 Aug (Day 11) 0 WallClock 0
    , Rule 2011 2011 Apr (Day 1) 1 WallClock 60
    , Rule 2011 2011 Aug (Day 1) 0 WallClock 0
    , Rule 2011 2011 Aug (Day 30) 0 WallClock 60
    , Rule 2011 2011 Sep (Day 30) 0 WallClock 0
    , Rule 2012 2014 Mar (Last Thu) 1440 WallClock 60
    , Rule 2012 2012 Sep (Day 21) 60 WallClock 0
    , Rule 2013 2013 Sep (Day 27) 0 WallClock 0
    , Rule 2014 2014 Oct (Day 24) 0 WallClock 0
    , Rule 2015 2015 Mar (Day 28) 0 WallClock 60
    , Rule 2015 2015 Oct (Day 23) 60 WallClock 0
    , Rule 2016 2018 Mar (Prev Sat 30) 60 WallClock 60
    , Rule 2016 2018 Oct (Prev Sat 30) 60 WallClock 0
    , Rule 2019 2019 Mar (Day 29) 0 WallClock 60
    , Rule 2019 2019 Oct (Prev Sat 30) 0 WallClock 0
    , Rule 2020 2021 Mar (Prev Sat 30) 0 WallClock 60
    , Rule 2020 2020 Oct (Day 24) 60 WallClock 0
    , Rule 2021 2021 Oct (Day 29) 60 WallClock 0
    , Rule 2022 2022 Mar (Day 27) 0 WallClock 60
    , Rule 2022 2035 Oct (Prev Sat 30) 120 WallClock 0
    , Rule 2023 2023 Apr (Day 29) 120 WallClock 60
    , Rule 2024 2024 Apr (Day 20) 120 WallClock 60
    , Rule 2025 2025 Apr (Day 12) 120 WallClock 60
    , Rule 2026 2054 Mar (Prev Sat 30) 120 WallClock 60
    , Rule 2036 2036 Oct (Day 18) 120 WallClock 0
    , Rule 2037 2037 Oct (Day 10) 120 WallClock 0
    ]


rules_Para : List Rule
rules_Para =
    [ Rule 1975 1988 Oct (Day 1) 0 WallClock 60
    , Rule 1975 1978 Mar (Day 1) 0 WallClock 0
    , Rule 1979 1991 Apr (Day 1) 0 WallClock 0
    , Rule 1989 1989 Oct (Day 22) 0 WallClock 60
    , Rule 1990 1990 Oct (Day 1) 0 WallClock 60
    , Rule 1991 1991 Oct (Day 6) 0 WallClock 60
    , Rule 1992 1992 Mar (Day 1) 0 WallClock 0
    , Rule 1992 1992 Oct (Day 5) 0 WallClock 60
    , Rule 1993 1993 Mar (Day 31) 0 WallClock 0
    , Rule 1993 1995 Oct (Day 1) 0 WallClock 60
    , Rule 1994 1995 Feb (Last Sun) 0 WallClock 0
    , Rule 1996 1996 Mar (Day 1) 0 WallClock 0
    , Rule 1996 2001 Oct (Next Sun 1) 0 WallClock 60
    , Rule 1997 1997 Feb (Last Sun) 0 WallClock 0
    , Rule 1998 2001 Mar (Next Sun 1) 0 WallClock 0
    , Rule 2002 2004 Apr (Next Sun 1) 0 WallClock 0
    , Rule 2002 2003 Sep (Next Sun 1) 0 WallClock 60
    , Rule 2004 2009 Oct (Next Sun 15) 0 WallClock 60
    , Rule 2005 2009 Mar (Next Sun 8) 0 WallClock 0
    , Rule 2010 2024 Oct (Next Sun 1) 0 WallClock 60
    , Rule 2010 2012 Apr (Next Sun 8) 0 WallClock 0
    , Rule 2013 2024 Mar (Next Sun 22) 0 WallClock 0
    ]


rules_Peru : List Rule
rules_Peru =
    [ Rule 1986 1987 Jan (Day 1) 0 WallClock 60
    , Rule 1986 1987 Apr (Day 1) 0 WallClock 0
    , Rule 1990 1990 Jan (Day 1) 0 WallClock 60
    , Rule 1990 1990 Apr (Day 1) 0 WallClock 0
    , Rule 1994 1994 Jan (Day 1) 0 WallClock 60
    , Rule 1994 1994 Apr (Day 1) 0 WallClock 0
    ]


rules_Phil : List Rule
rules_Phil =
    [ Rule 1977 1977 Mar (Day 27) 1440 WallClock 60
    , Rule 1977 1977 Sep (Day 21) 1440 WallClock 0
    , Rule 1990 1990 May (Day 21) 0 WallClock 60
    , Rule 1990 1990 Jul (Day 28) 1440 WallClock 0
    ]


rules_Port : List Rule
rules_Port =
    [ Rule 1976 1976 Sep (Last Sun) 60 WallClock 0
    , Rule 1977 1977 Mar (Last Sun) 0 Standard 60
    , Rule 1977 1977 Sep (Last Sun) 0 Standard 0
    , Rule 1978 1980 Apr (Next Sun 1) 60 Standard 60
    , Rule 1978 1978 Oct (Day 1) 60 Standard 0
    , Rule 1979 1980 Sep (Last Sun) 60 Standard 0
    , Rule 1981 1986 Mar (Last Sun) 0 Standard 60
    , Rule 1981 1985 Sep (Last Sun) 0 Standard 0
    ]


rules_ROK : List Rule
rules_ROK =
    [ Rule 1987 1988 May (Next Sun 8) 120 WallClock 60
    , Rule 1987 1988 Oct (Next Sun 8) 180 WallClock 0
    ]


rules_Romania : List Rule
rules_Romania =
    [ Rule 1979 1979 May (Day 27) 0 WallClock 60
    , Rule 1979 1979 Sep (Last Sun) 0 WallClock 0
    , Rule 1980 1980 Apr (Day 5) 1380 WallClock 60
    , Rule 1980 1980 Sep (Last Sun) 60 WallClock 0
    , Rule 1991 1993 Mar (Last Sun) 0 Standard 60
    , Rule 1991 1993 Sep (Last Sun) 0 Standard 0
    ]


rules_Russia : List Rule
rules_Russia =
    [ Rule 1981 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1981 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1984 1995 Sep (Last Sun) 120 Standard 0
    , Rule 1985 2010 Mar (Last Sun) 120 Standard 60
    , Rule 1996 2010 Oct (Last Sun) 120 Standard 0
    ]


rules_RussiaAsia : List Rule
rules_RussiaAsia =
    [ Rule 1981 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1981 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1984 1995 Sep (Last Sun) 120 Standard 0
    , Rule 1985 2010 Mar (Last Sun) 120 Standard 60
    , Rule 1996 2010 Oct (Last Sun) 120 Standard 0
    ]


rules_Salv : List Rule
rules_Salv =
    [ Rule 1987 1988 May (Next Sun 1) 0 WallClock 60
    , Rule 1987 1988 Sep (Last Sun) 0 WallClock 0
    ]


rules_SanLuis : List Rule
rules_SanLuis =
    [ Rule 2008 2009 Mar (Next Sun 8) 0 WallClock 0
    , Rule 2007 2008 Oct (Next Sun 8) 0 WallClock 60
    ]


rules_Spain : List Rule
rules_Spain =
    [ Rule 1974 1975 Apr (Next Sat 12) 1380 WallClock 60
    , Rule 1974 1975 Oct (Next Sun 1) 60 WallClock 0
    , Rule 1976 1976 Mar (Day 27) 1380 WallClock 60
    , Rule 1976 1977 Sep (Last Sun) 60 WallClock 0
    , Rule 1977 1977 Apr (Day 2) 1380 WallClock 60
    , Rule 1978 1978 Apr (Day 2) 120 Standard 60
    , Rule 1978 1978 Oct (Day 1) 120 Standard 0
    ]


rules_SpainAfrica : List Rule
rules_SpainAfrica =
    [ Rule 1974 1974 Jun (Day 24) 0 WallClock 60
    , Rule 1974 1974 Sep (Day 1) 0 WallClock 0
    , Rule 1976 1977 May (Day 1) 0 WallClock 60
    , Rule 1976 1976 Aug (Day 1) 0 WallClock 0
    , Rule 1977 1977 Sep (Day 28) 0 WallClock 0
    , Rule 1978 1978 Jun (Day 1) 0 WallClock 60
    , Rule 1978 1978 Aug (Day 4) 0 WallClock 0
    ]


rules_StJohns : List Rule
rules_StJohns =
    [ Rule 1951 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1960 1986 Oct (Last Sun) 120 WallClock 0
    , Rule 1987 1987 Apr (Next Sun 1) 1 WallClock 60
    , Rule 1987 2006 Oct (Last Sun) 1 WallClock 0
    , Rule 1988 1988 Apr (Next Sun 1) 1 WallClock 120
    , Rule 1989 2006 Apr (Next Sun 1) 1 WallClock 60
    , Rule 2007 2011 Mar (Next Sun 8) 1 WallClock 60
    , Rule 2007 2010 Nov (Next Sun 1) 1 WallClock 0
    ]


rules_Sudan : List Rule
rules_Sudan =
    [ Rule 1970 1970 May (Day 1) 0 WallClock 60
    , Rule 1970 1985 Oct (Day 15) 0 WallClock 0
    , Rule 1971 1971 Apr (Day 30) 0 WallClock 60
    , Rule 1972 1985 Apr (Last Sun) 0 WallClock 60
    ]


rules_Syria : List Rule
rules_Syria =
    [ Rule 1966 1976 Oct (Day 1) 120 WallClock 0
    , Rule 1967 1978 May (Day 1) 120 WallClock 60
    , Rule 1977 1978 Sep (Day 1) 120 WallClock 0
    , Rule 1983 1984 Apr (Day 9) 120 WallClock 60
    , Rule 1983 1984 Oct (Day 1) 120 WallClock 0
    , Rule 1986 1986 Feb (Day 16) 120 WallClock 60
    , Rule 1986 1986 Oct (Day 9) 120 WallClock 0
    , Rule 1987 1987 Mar (Day 1) 120 WallClock 60
    , Rule 1987 1988 Oct (Day 31) 120 WallClock 0
    , Rule 1988 1988 Mar (Day 15) 120 WallClock 60
    , Rule 1989 1989 Mar (Day 31) 120 WallClock 60
    , Rule 1989 1989 Oct (Day 1) 120 WallClock 0
    , Rule 1990 1990 Apr (Day 1) 120 WallClock 60
    , Rule 1990 1990 Sep (Day 30) 120 WallClock 0
    , Rule 1991 1991 Apr (Day 1) 0 WallClock 60
    , Rule 1991 1992 Oct (Day 1) 0 WallClock 0
    , Rule 1992 1992 Apr (Day 8) 0 WallClock 60
    , Rule 1993 1993 Mar (Day 26) 0 WallClock 60
    , Rule 1993 1993 Sep (Day 25) 0 WallClock 0
    , Rule 1994 1996 Apr (Day 1) 0 WallClock 60
    , Rule 1994 2005 Oct (Day 1) 0 WallClock 0
    , Rule 1997 1998 Mar (Last Mon) 0 WallClock 60
    , Rule 1999 2006 Apr (Day 1) 0 WallClock 60
    , Rule 2006 2006 Sep (Day 22) 0 WallClock 0
    , Rule 2007 2007 Mar (Last Fri) 0 WallClock 60
    , Rule 2007 2007 Nov (Next Fri 1) 0 WallClock 0
    , Rule 2008 2008 Apr (Next Fri 1) 0 WallClock 60
    , Rule 2008 2008 Nov (Day 1) 0 WallClock 0
    , Rule 2009 2009 Mar (Last Fri) 0 WallClock 60
    , Rule 2010 2011 Apr (Next Fri 1) 0 WallClock 60
    , Rule 2012 2022 Mar (Last Fri) 0 WallClock 60
    , Rule 2009 2022 Oct (Last Fri) 0 WallClock 0
    ]


rules_Taiwan : List Rule
rules_Taiwan =
    [ Rule 1974 1975 Apr (Day 1) 0 WallClock 60
    , Rule 1974 1975 Oct (Day 1) 0 WallClock 0
    , Rule 1979 1979 Jul (Day 1) 0 WallClock 60
    , Rule 1979 1979 Oct (Day 1) 0 WallClock 0
    ]


rules_Thule : List Rule
rules_Thule =
    [ Rule 1991 1992 Mar (Last Sun) 120 WallClock 60
    , Rule 1991 1992 Sep (Last Sun) 120 WallClock 0
    , Rule 1993 2006 Apr (Next Sun 1) 120 WallClock 60
    , Rule 1993 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 2007 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2007 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Tonga : List Rule
rules_Tonga =
    [ Rule 1999 1999 Oct (Day 7) 120 Standard 60
    , Rule 2000 2000 Mar (Day 19) 120 Standard 0
    , Rule 2000 2001 Nov (Next Sun 1) 120 WallClock 60
    , Rule 2001 2002 Jan (Last Sun) 120 WallClock 0
    , Rule 2016 2016 Nov (Next Sun 1) 120 WallClock 60
    , Rule 2017 2017 Jan (Next Sun 15) 180 WallClock 0
    ]


rules_Toronto : List Rule
rules_Toronto =
    [ Rule 1946 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1957 1973 Oct (Last Sun) 120 WallClock 0
    ]


rules_Troll : List Rule
rules_Troll =
    [ Rule 2005 maxYear Mar (Last Sun) 60 Universal 120
    , Rule 2004 maxYear Oct (Last Sun) 60 Universal 0
    ]


rules_Tunisia : List Rule
rules_Tunisia =
    [ Rule 1977 1977 Apr (Day 30) 0 Standard 60
    , Rule 1977 1977 Sep (Day 24) 0 Standard 0
    , Rule 1978 1978 May (Day 1) 0 Standard 60
    , Rule 1978 1978 Oct (Day 1) 0 Standard 0
    , Rule 1988 1988 Jun (Day 1) 0 Standard 60
    , Rule 1988 1990 Sep (Last Sun) 0 Standard 0
    , Rule 1989 1989 Mar (Day 26) 0 Standard 60
    , Rule 1990 1990 May (Day 1) 0 Standard 60
    , Rule 2005 2005 May (Day 1) 0 Standard 60
    , Rule 2005 2005 Sep (Day 30) 60 Standard 0
    , Rule 2006 2008 Mar (Last Sun) 120 Standard 60
    , Rule 2006 2008 Oct (Last Sun) 120 Standard 0
    ]


rules_Turkey : List Rule
rules_Turkey =
    [ Rule 1973 1973 Jun (Day 3) 60 WallClock 60
    , Rule 1973 1976 Oct (Next Sun 31) 120 WallClock 0
    , Rule 1974 1974 Mar (Day 31) 120 WallClock 60
    , Rule 1975 1975 Mar (Day 22) 120 WallClock 60
    , Rule 1976 1976 Mar (Day 21) 120 WallClock 60
    , Rule 1977 1978 Apr (Next Sun 1) 120 WallClock 60
    , Rule 1977 1978 Oct (Next Sun 15) 120 WallClock 0
    , Rule 1978 1978 Jun (Day 29) 0 WallClock 0
    , Rule 1983 1983 Jul (Day 31) 120 WallClock 60
    , Rule 1983 1983 Oct (Day 2) 120 WallClock 0
    , Rule 1985 1985 Apr (Day 20) 60 Standard 60
    , Rule 1985 1985 Sep (Day 28) 60 Standard 0
    , Rule 1986 1993 Mar (Last Sun) 60 Standard 60
    , Rule 1986 1995 Sep (Last Sun) 60 Standard 0
    , Rule 1994 1994 Mar (Day 20) 60 Standard 60
    , Rule 1995 2006 Mar (Last Sun) 60 Standard 60
    , Rule 1996 2006 Oct (Last Sun) 60 Standard 0
    ]


rules_US : List Rule
rules_US =
    [ Rule 1967 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 1967 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1974 1974 Jan (Day 6) 120 WallClock 60
    , Rule 1975 1975 Feb (Last Sun) 120 WallClock 60
    , Rule 1976 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1987 2006 Apr (Next Sun 1) 120 WallClock 60
    , Rule 2007 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2007 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Uruguay : List Rule
rules_Uruguay =
    [ Rule 1970 1970 Apr (Day 25) 0 WallClock 60
    , Rule 1970 1970 Jun (Day 14) 0 WallClock 0
    , Rule 1972 1972 Apr (Day 23) 0 WallClock 60
    , Rule 1972 1972 Jul (Day 16) 0 WallClock 0
    , Rule 1974 1974 Jan (Day 13) 0 WallClock 90
    , Rule 1974 1974 Mar (Day 10) 0 WallClock 30
    , Rule 1974 1974 Sep (Day 1) 0 WallClock 0
    , Rule 1974 1974 Dec (Day 22) 0 WallClock 60
    , Rule 1975 1975 Mar (Day 30) 0 WallClock 0
    , Rule 1976 1976 Dec (Day 19) 0 WallClock 60
    , Rule 1977 1977 Mar (Day 6) 0 WallClock 0
    , Rule 1977 1977 Dec (Day 4) 0 WallClock 60
    , Rule 1978 1979 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1978 1978 Dec (Day 17) 0 WallClock 60
    , Rule 1979 1979 Apr (Day 29) 0 WallClock 60
    , Rule 1980 1980 Mar (Day 16) 0 WallClock 0
    , Rule 1987 1987 Dec (Day 14) 0 WallClock 60
    , Rule 1988 1988 Feb (Day 28) 0 WallClock 0
    , Rule 1988 1988 Dec (Day 11) 0 WallClock 60
    , Rule 1989 1989 Mar (Day 5) 0 WallClock 0
    , Rule 1989 1989 Oct (Day 29) 0 WallClock 60
    , Rule 1990 1990 Feb (Day 25) 0 WallClock 0
    , Rule 1990 1991 Oct (Next Sun 21) 0 WallClock 60
    , Rule 1991 1992 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1992 1992 Oct (Day 18) 0 WallClock 60
    , Rule 1993 1993 Feb (Day 28) 0 WallClock 0
    , Rule 2004 2004 Sep (Day 19) 0 WallClock 60
    , Rule 2005 2005 Mar (Day 27) 120 WallClock 0
    , Rule 2005 2005 Oct (Day 9) 120 WallClock 60
    , Rule 2006 2015 Mar (Next Sun 8) 120 WallClock 0
    , Rule 2006 2014 Oct (Next Sun 1) 120 WallClock 60
    ]


rules_Vanc : List Rule
rules_Vanc =
    [ Rule 1946 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1962 2006 Oct (Last Sun) 120 WallClock 0
    ]


rules_Vanuatu : List Rule
rules_Vanuatu =
    [ Rule 1973 1973 Dec (Day 22) 720 Universal 60
    , Rule 1974 1974 Mar (Day 30) 720 Universal 0
    , Rule 1983 1991 Sep (Next Sat 22) 1440 WallClock 60
    , Rule 1984 1991 Mar (Next Sat 22) 1440 WallClock 0
    , Rule 1992 1993 Jan (Next Sat 22) 1440 WallClock 0
    , Rule 1992 1992 Oct (Next Sat 22) 1440 WallClock 60
    ]


rules_W_Eur : List Rule
rules_W_Eur =
    [ Rule 1977 1980 Apr (Next Sun 1) 60 Standard 60
    , Rule 1977 1977 Sep (Last Sun) 60 Standard 0
    , Rule 1978 1978 Oct (Day 1) 60 Standard 0
    , Rule 1979 1995 Sep (Last Sun) 60 Standard 0
    , Rule 1981 maxYear Mar (Last Sun) 60 Standard 60
    , Rule 1996 maxYear Oct (Last Sun) 60 Standard 0
    ]


rules_WS : List Rule
rules_WS =
    [ Rule 2010 2010 Sep (Last Sun) 0 WallClock 60
    , Rule 2011 2011 Apr (Next Sat 1) 240 WallClock 0
    , Rule 2011 2011 Sep (Last Sat) 180 WallClock 60
    , Rule 2012 2021 Apr (Next Sun 1) 240 WallClock 0
    , Rule 2012 2020 Sep (Last Sun) 180 WallClock 60
    ]


rules_Winn : List Rule
rules_Winn =
    [ Rule 1966 1986 Apr (Last Sun) 120 Standard 60
    , Rule 1966 2005 Oct (Last Sun) 120 Standard 0
    , Rule 1987 2005 Apr (Next Sun 1) 120 Standard 60
    ]


rules_Zion : List Rule
rules_Zion =
    [ Rule 1974 1974 Jul (Day 6) 1440 WallClock 60
    , Rule 1974 1974 Oct (Day 12) 1440 WallClock 0
    , Rule 1975 1975 Apr (Day 19) 1440 WallClock 60
    , Rule 1975 1975 Aug (Day 30) 1440 WallClock 0
    , Rule 1980 1980 Aug (Day 2) 1440 Standard 60
    , Rule 1980 1980 Sep (Day 13) 1440 Standard 0
    , Rule 1984 1984 May (Day 5) 1440 Standard 60
    , Rule 1984 1984 Aug (Day 25) 1440 Standard 0
    , Rule 1985 1985 Apr (Day 13) 1440 WallClock 60
    , Rule 1985 1985 Aug (Day 31) 1440 WallClock 0
    , Rule 1986 1986 May (Day 17) 1440 WallClock 60
    , Rule 1986 1986 Sep (Day 6) 1440 WallClock 0
    , Rule 1987 1987 Apr (Day 14) 1440 WallClock 60
    , Rule 1987 1987 Sep (Day 12) 1440 WallClock 0
    , Rule 1988 1988 Apr (Day 9) 1440 WallClock 60
    , Rule 1988 1988 Sep (Day 3) 1440 WallClock 0
    , Rule 1989 1989 Apr (Day 29) 1440 WallClock 60
    , Rule 1989 1989 Sep (Day 2) 1440 WallClock 0
    , Rule 1990 1990 Mar (Day 24) 1440 WallClock 60
    , Rule 1990 1990 Aug (Day 25) 1440 WallClock 0
    , Rule 1991 1991 Mar (Day 23) 1440 WallClock 60
    , Rule 1991 1991 Aug (Day 31) 1440 WallClock 0
    , Rule 1992 1992 Mar (Day 28) 1440 WallClock 60
    , Rule 1992 1992 Sep (Day 5) 1440 WallClock 0
    , Rule 1993 1993 Apr (Day 2) 0 WallClock 60
    , Rule 1993 1993 Sep (Day 5) 0 WallClock 0
    , Rule 1994 1994 Apr (Day 1) 0 WallClock 60
    , Rule 1994 1994 Aug (Day 28) 0 WallClock 0
    , Rule 1995 1995 Mar (Day 31) 0 WallClock 60
    , Rule 1995 1995 Sep (Day 3) 0 WallClock 0
    , Rule 1996 1996 Mar (Day 14) 1440 WallClock 60
    , Rule 1996 1996 Sep (Day 15) 1440 WallClock 0
    , Rule 1997 1997 Mar (Day 20) 1440 WallClock 60
    , Rule 1997 1997 Sep (Day 13) 1440 WallClock 0
    , Rule 1998 1998 Mar (Day 20) 0 WallClock 60
    , Rule 1998 1998 Sep (Day 6) 0 WallClock 0
    , Rule 1999 1999 Apr (Day 2) 120 WallClock 60
    , Rule 1999 1999 Sep (Day 3) 120 WallClock 0
    , Rule 2000 2000 Apr (Day 14) 120 WallClock 60
    , Rule 2000 2000 Oct (Day 6) 60 WallClock 0
    , Rule 2001 2001 Apr (Day 9) 60 WallClock 60
    , Rule 2001 2001 Sep (Day 24) 60 WallClock 0
    , Rule 2002 2002 Mar (Day 29) 60 WallClock 60
    , Rule 2002 2002 Oct (Day 7) 60 WallClock 0
    , Rule 2003 2003 Mar (Day 28) 60 WallClock 60
    , Rule 2003 2003 Oct (Day 3) 60 WallClock 0
    , Rule 2004 2004 Apr (Day 7) 60 WallClock 60
    , Rule 2004 2004 Sep (Day 22) 60 WallClock 0
    , Rule 2005 2012 Apr (Prev Fri 1) 120 WallClock 60
    , Rule 2005 2005 Oct (Day 9) 120 WallClock 0
    , Rule 2006 2006 Oct (Day 1) 120 WallClock 0
    , Rule 2007 2007 Sep (Day 16) 120 WallClock 0
    , Rule 2008 2008 Oct (Day 5) 120 WallClock 0
    , Rule 2009 2009 Sep (Day 27) 120 WallClock 0
    , Rule 2010 2010 Sep (Day 12) 120 WallClock 0
    , Rule 2011 2011 Oct (Day 2) 120 WallClock 0
    , Rule 2012 2012 Sep (Day 23) 120 WallClock 0
    , Rule 2013 maxYear Mar (Next Fri 23) 120 WallClock 60
    , Rule 2013 maxYear Oct (Last Sun) 120 WallClock 0
    ]



-- Zones


{-| `Africa/Abidjan`
-}
africa__abidjan : () -> Time.Zone
africa__abidjan _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Accra`
-}
africa__accra : () -> Time.Zone
africa__accra _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Addis_Ababa`
-}
africa__addis_ababa : () -> Time.Zone
africa__addis_ababa _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Algiers`
-}
africa__algiers : () -> Time.Zone
africa__algiers _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Rules rules_Algeria), DateTime 1977 Oct 21 0 WallClock )
            , ( ZoneState 60 (Rules rules_Algeria), DateTime 1979 Oct 26 0 WallClock )
            , ( ZoneState 0 (Rules rules_Algeria), DateTime 1981 May 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Africa/Asmara`
-}
africa__asmara : () -> Time.Zone
africa__asmara _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Bamako`
-}
africa__bamako : () -> Time.Zone
africa__bamako _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Bangui`
-}
africa__bangui : () -> Time.Zone
africa__bangui _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Banjul`
-}
africa__banjul : () -> Time.Zone
africa__banjul _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Bissau`
-}
africa__bissau : () -> Time.Zone
africa__bissau _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -60 (Save 0), DateTime 1975 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Africa/Blantyre`
-}
africa__blantyre : () -> Time.Zone
africa__blantyre _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Brazzaville`
-}
africa__brazzaville : () -> Time.Zone
africa__brazzaville _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Bujumbura`
-}
africa__bujumbura : () -> Time.Zone
africa__bujumbura _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Cairo`
-}
africa__cairo : () -> Time.Zone
africa__cairo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Rules rules_Egypt))


{-| `Africa/Casablanca`
-}
africa__casablanca : () -> Time.Zone
africa__casablanca _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Rules rules_Morocco), DateTime 1984 Mar 16 0 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            , ( ZoneState 0 (Rules rules_Morocco), DateTime 2018 Oct 28 180 WallClock )
            ]
            (ZoneState 60 (Rules rules_Morocco))


{-| `Africa/Ceuta`
-}
africa__ceuta : () -> Time.Zone
africa__ceuta _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Rules rules_SpainAfrica), DateTime 1984 Mar 16 0 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Africa/Conakry`
-}
africa__conakry : () -> Time.Zone
africa__conakry _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Dakar`
-}
africa__dakar : () -> Time.Zone
africa__dakar _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Dar_es_Salaam`
-}
africa__dar_es_salaam : () -> Time.Zone
africa__dar_es_salaam _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Djibouti`
-}
africa__djibouti : () -> Time.Zone
africa__djibouti _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Douala`
-}
africa__douala : () -> Time.Zone
africa__douala _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/El_Aaiun`
-}
africa__el_aaiun : () -> Time.Zone
africa__el_aaiun _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -60 (Save 0), DateTime 1976 Apr 14 0 WallClock )
            , ( ZoneState 0 (Rules rules_Morocco), DateTime 2018 Oct 28 180 WallClock )
            ]
            (ZoneState 60 (Rules rules_Morocco))


{-| `Africa/Freetown`
-}
africa__freetown : () -> Time.Zone
africa__freetown _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Gaborone`
-}
africa__gaborone : () -> Time.Zone
africa__gaborone _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Harare`
-}
africa__harare : () -> Time.Zone
africa__harare _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Johannesburg`
-}
africa__johannesburg : () -> Time.Zone
africa__johannesburg _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Juba`
-}
africa__juba : () -> Time.Zone
africa__juba _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Sudan), DateTime 2000 Jan 15 720 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 2021 Feb 1 0 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Africa/Kampala`
-}
africa__kampala : () -> Time.Zone
africa__kampala _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Khartoum`
-}
africa__khartoum : () -> Time.Zone
africa__khartoum _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Sudan), DateTime 2000 Jan 15 720 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 2017 Nov 1 0 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Africa/Kigali`
-}
africa__kigali : () -> Time.Zone
africa__kigali _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Kinshasa`
-}
africa__kinshasa : () -> Time.Zone
africa__kinshasa _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Lagos`
-}
africa__lagos : () -> Time.Zone
africa__lagos _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Libreville`
-}
africa__libreville : () -> Time.Zone
africa__libreville _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Lome`
-}
africa__lome : () -> Time.Zone
africa__lome _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Luanda`
-}
africa__luanda : () -> Time.Zone
africa__luanda _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Lubumbashi`
-}
africa__lubumbashi : () -> Time.Zone
africa__lubumbashi _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Lusaka`
-}
africa__lusaka : () -> Time.Zone
africa__lusaka _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Malabo`
-}
africa__malabo : () -> Time.Zone
africa__malabo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Maputo`
-}
africa__maputo : () -> Time.Zone
africa__maputo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Maseru`
-}
africa__maseru : () -> Time.Zone
africa__maseru _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Mbabane`
-}
africa__mbabane : () -> Time.Zone
africa__mbabane _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Mogadishu`
-}
africa__mogadishu : () -> Time.Zone
africa__mogadishu _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Monrovia`
-}
africa__monrovia : () -> Time.Zone
africa__monrovia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -44 (Save 0), DateTime 1972 Jan 7 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Africa/Nairobi`
-}
africa__nairobi : () -> Time.Zone
africa__nairobi _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Ndjamena`
-}
africa__ndjamena : () -> Time.Zone
africa__ndjamena _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1979 Oct 14 0 WallClock )
            , ( ZoneState 60 (Save 60), DateTime 1980 Mar 8 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Africa/Niamey`
-}
africa__niamey : () -> Time.Zone
africa__niamey _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Nouakchott`
-}
africa__nouakchott : () -> Time.Zone
africa__nouakchott _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Ouagadougou`
-}
africa__ouagadougou : () -> Time.Zone
africa__ouagadougou _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Porto-Novo`
-}
africa__porto_novo : () -> Time.Zone
africa__porto_novo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Sao_Tome`
-}
africa__sao_tome : () -> Time.Zone
africa__sao_tome _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 2018 Jan 1 60 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 2019 Jan 1 120 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Africa/Timbuktu`
-}
africa__timbuktu : () -> Time.Zone
africa__timbuktu _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Tripoli`
-}
africa__tripoli : () -> Time.Zone
africa__tripoli _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Save 0), DateTime 1982 Jan 1 0 WallClock )
            , ( ZoneState 60 (Rules rules_Libya), DateTime 1990 May 4 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1996 Sep 30 0 WallClock )
            , ( ZoneState 60 (Rules rules_Libya), DateTime 1997 Oct 4 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2012 Nov 10 120 WallClock )
            , ( ZoneState 60 (Rules rules_Libya), DateTime 2013 Oct 25 120 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Africa/Tunis`
-}
africa__tunis : () -> Time.Zone
africa__tunis _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Rules rules_Tunisia))


{-| `Africa/Windhoek`
-}
africa__windhoek : () -> Time.Zone
africa__windhoek _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Save 0), DateTime 1990 Mar 21 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Namibia))


{-| `America/Adak`
-}
america__adak : () -> Time.Zone
america__adak _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -660 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -600 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -600 (Rules rules_US))


{-| `America/Anchorage`
-}
america__anchorage : () -> Time.Zone
america__anchorage _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -600 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Anguilla`
-}
america__anguilla : () -> Time.Zone
america__anguilla _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Antigua`
-}
america__antigua : () -> Time.Zone
america__antigua _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Araguaina`
-}
america__araguaina : () -> Time.Zone
america__araguaina _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1995 Sep 14 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2003 Sep 24 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2012 Oct 21 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2013 Sep 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Buenos_Aires`
-}
america__argentina__buenos_aires : () -> Time.Zone
america__argentina__buenos_aires _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Arg))


{-| `America/Argentina/Catamarca`
-}
america__argentina__catamarca : () -> Time.Zone
america__argentina__catamarca _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/ComodRivadavia`
-}
america__argentina__comodrivadavia : () -> Time.Zone
america__argentina__comodrivadavia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Save 0), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Cordoba`
-}
america__argentina__cordoba : () -> Time.Zone
america__argentina__cordoba _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Arg))


{-| `America/Argentina/Jujuy`
-}
america__argentina__jujuy : () -> Time.Zone
america__argentina__jujuy _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1990 Mar 4 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1990 Oct 28 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1991 Mar 17 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 6 0 WallClock )
            , ( ZoneState -180 (Save 60), DateTime 1992 Jan 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/La_Rioja`
-}
america__argentina__la_rioja : () -> Time.Zone
america__argentina__la_rioja _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 May 7 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Mendoza`
-}
america__argentina__mendoza : () -> Time.Zone
america__argentina__mendoza _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1990 Mar 4 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1990 Oct 15 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 15 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1992 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1992 Oct 18 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 23 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Sep 26 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Rio_Gallegos`
-}
america__argentina__rio_gallegos : () -> Time.Zone
america__argentina__rio_gallegos _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Salta`
-}
america__argentina__salta : () -> Time.Zone
america__argentina__salta _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/San_Juan`
-}
america__argentina__san_juan : () -> Time.Zone
america__argentina__san_juan _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 May 7 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 31 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jul 25 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/San_Luis`
-}
america__argentina__san_luis : () -> Time.Zone
america__argentina__san_luis _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState -180 (Save 60), DateTime 1990 Mar 14 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1990 Oct 15 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Jun 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 31 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jul 25 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Jan 21 0 WallClock )
            , ( ZoneState -240 (Rules rules_SanLuis), DateTime 2009 Oct 11 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Tucuman`
-}
america__argentina__tucuman : () -> Time.Zone
america__argentina__tucuman _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 13 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Arg))


{-| `America/Argentina/Ushuaia`
-}
america__argentina__ushuaia : () -> Time.Zone
america__argentina__ushuaia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 30 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Aruba`
-}
america__aruba : () -> Time.Zone
america__aruba _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Asuncion`
-}
america__asuncion : () -> Time.Zone
america__asuncion _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1972 Oct 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1974 Apr 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Para), DateTime 2024 Oct 15 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Atikokan`
-}
america__atikokan : () -> Time.Zone
america__atikokan _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `America/Bahia`
-}
america__bahia : () -> Time.Zone
america__bahia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 2003 Sep 24 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2011 Oct 16 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2012 Oct 21 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Bahia_Banderas`
-}
america__bahia_banderas : () -> Time.Zone
america__bahia_banderas _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 2010 Apr 4 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Barbados`
-}
america__barbados : () -> Time.Zone
america__barbados _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Rules rules_Barb))


{-| `America/Belem`
-}
america__belem : () -> Time.Zone
america__belem _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Belize`
-}
america__belize : () -> Time.Zone
america__belize _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Rules rules_Belize))


{-| `America/Blanc-Sablon`
-}
america__blanc_sablon : () -> Time.Zone
america__blanc_sablon _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Boa_Vista`
-}
america__boa_vista : () -> Time.Zone
america__boa_vista _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -240 (Rules rules_Brazil), DateTime 2000 Oct 15 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Bogota`
-}
america__bogota : () -> Time.Zone
america__bogota _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Rules rules_CO))


{-| `America/Boise`
-}
america__boise : () -> Time.Zone
america__boise _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 1974 Jan 1 0 WallClock )
            , ( ZoneState -420 (Save 0), DateTime 1974 Feb 3 120 WallClock )
            ]
            (ZoneState -420 (Rules rules_US))


{-| `America/Cambridge_Bay`
-}
america__cambridge_bay : () -> Time.Zone
america__cambridge_bay _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_NT_YK), DateTime 1999 Oct 31 120 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2000 Nov 5 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2001 Apr 1 180 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `America/Campo_Grande`
-}
america__campo_grande : () -> Time.Zone
america__campo_grande _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Rules rules_Brazil))


{-| `America/Cancun`
-}
america__cancun : () -> Time.Zone
america__cancun _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1981 Dec 26 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1983 Jan 4 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 1997 Oct 26 120 WallClock )
            , ( ZoneState -300 (Rules rules_Mexico), DateTime 1998 Aug 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 2015 Feb 1 120 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Caracas`
-}
america__caracas : () -> Time.Zone
america__caracas _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 2007 Dec 9 180 WallClock )
            , ( ZoneState -270 (Save 0), DateTime 2016 May 1 150 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Cayenne`
-}
america__cayenne : () -> Time.Zone
america__cayenne _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -180 (Save 0))


{-| `America/Cayman`
-}
america__cayman : () -> Time.Zone
america__cayman _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `America/Chicago`
-}
america__chicago : () -> Time.Zone
america__chicago _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Rules rules_US))


{-| `America/Chihuahua`
-}
america__chihuahua : () -> Time.Zone
america__chihuahua _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1998 Apr 5 180 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 2022 Oct 30 120 WallClock )
            ]
            (ZoneState -360 (Save 0))


{-| `America/Ciudad_Juarez`
-}
america__ciudad_juarez : () -> Time.Zone
america__ciudad_juarez _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1998 Apr 5 180 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            , ( ZoneState -420 (Rules rules_US), DateTime 2022 Oct 30 120 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2022 Nov 30 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_US))


{-| `America/Coral_Harbour`
-}
america__coral_harbour : () -> Time.Zone
america__coral_harbour _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `America/Costa_Rica`
-}
america__costa_rica : () -> Time.Zone
america__costa_rica _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Rules rules_CR))


{-| `America/Coyhaique`
-}
america__coyhaique : () -> Time.Zone
america__coyhaique _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Chile), DateTime 2025 Mar 20 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Creston`
-}
america__creston : () -> Time.Zone
america__creston _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -420 (Save 0))


{-| `America/Cuiaba`
-}
america__cuiaba : () -> Time.Zone
america__cuiaba _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 2003 Sep 24 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Oct 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Brazil))


{-| `America/Curacao`
-}
america__curacao : () -> Time.Zone
america__curacao _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Danmarkshavn`
-}
america__danmarkshavn : () -> Time.Zone
america__danmarkshavn _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Save 0), DateTime 1980 Apr 6 120 WallClock )
            , ( ZoneState -180 (Rules rules_EU), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `America/Dawson`
-}
america__dawson : () -> Time.Zone
america__dawson _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -540 (Save 0), DateTime 1973 Oct 28 0 WallClock )
            , ( ZoneState -480 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Canada), DateTime 2020 Nov 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Dawson_Creek`
-}
america__dawson_creek : () -> Time.Zone
america__dawson_creek _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_Vanc), DateTime 1972 Aug 30 120 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Denver`
-}
america__denver : () -> Time.Zone
america__denver _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -420 (Rules rules_US))


{-| `America/Detroit`
-}
america__detroit : () -> Time.Zone
america__detroit _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 1975 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1975 Apr 27 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Dominica`
-}
america__dominica : () -> Time.Zone
america__dominica _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Edmonton`
-}
america__edmonton : () -> Time.Zone
america__edmonton _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_Edm), DateTime 1987 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `America/Eirunepe`
-}
america__eirunepe : () -> Time.Zone
america__eirunepe _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1993 Sep 28 0 WallClock )
            , ( ZoneState -300 (Rules rules_Brazil), DateTime 1994 Sep 22 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2008 Jun 24 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2013 Nov 10 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/El_Salvador`
-}
america__el_salvador : () -> Time.Zone
america__el_salvador _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Rules rules_Salv))


{-| `America/Ensenada`
-}
america__ensenada : () -> Time.Zone
america__ensenada _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState -480 (Save 0))


{-| `America/Fort_Nelson`
-}
america__fort_nelson : () -> Time.Zone
america__fort_nelson _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_Vanc), DateTime 1987 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Canada), DateTime 2015 Mar 8 120 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Fortaleza`
-}
america__fortaleza : () -> Time.Zone
america__fortaleza _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2000 Oct 22 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Glace_Bay`
-}
america__glace_bay : () -> Time.Zone
america__glace_bay _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1972 Jan 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Halifax), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Goose_Bay`
-}
america__goose_bay : () -> Time.Zone
america__goose_bay _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_StJohns), DateTime 2011 Nov 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Grand_Turk`
-}
america__grand_turk : () -> Time.Zone
america__grand_turk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1979 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 2015 Mar 8 120 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2018 Mar 11 180 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Grenada`
-}
america__grenada : () -> Time.Zone
america__grenada _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Guadeloupe`
-}
america__guadeloupe : () -> Time.Zone
america__guadeloupe _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Guatemala`
-}
america__guatemala : () -> Time.Zone
america__guatemala _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Rules rules_Guat))


{-| `America/Guayaquil`
-}
america__guayaquil : () -> Time.Zone
america__guayaquil _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Rules rules_Ecuador))


{-| `America/Guyana`
-}
america__guyana : () -> Time.Zone
america__guyana _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -225 (Save 0), DateTime 1975 Aug 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1992 Mar 29 60 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Halifax`
-}
america__halifax : () -> Time.Zone
america__halifax _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Halifax), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Havana`
-}
america__havana : () -> Time.Zone
america__havana _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Rules rules_Cuba))


{-| `America/Hermosillo`
-}
america__hermosillo : () -> Time.Zone
america__hermosillo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Indiana/Indianapolis`
-}
america__indiana__indianapolis : () -> Time.Zone
america__indiana__indianapolis _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Knox`
-}
america__indiana__knox : () -> Time.Zone
america__indiana__knox _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_US), DateTime 1991 Oct 27 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Indiana/Marengo`
-}
america__indiana__marengo : () -> Time.Zone
america__indiana__marengo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1974 Jan 6 120 WallClock )
            , ( ZoneState -360 (Save 60), DateTime 1974 Oct 27 120 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 1976 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Petersburg`
-}
america__indiana__petersburg : () -> Time.Zone
america__indiana__petersburg _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_US), DateTime 1977 Oct 30 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 2007 Nov 4 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Tell_City`
-}
america__indiana__tell_city : () -> Time.Zone
america__indiana__tell_city _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Indiana/Vevay`
-}
america__indiana__vevay : () -> Time.Zone
america__indiana__vevay _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Vincennes`
-}
america__indiana__vincennes : () -> Time.Zone
america__indiana__vincennes _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 2007 Nov 4 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Winamac`
-}
america__indiana__winamac : () -> Time.Zone
america__indiana__winamac _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 2007 Mar 11 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Inuvik`
-}
america__inuvik : () -> Time.Zone
america__inuvik _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_NT_YK), DateTime 1979 Apr 29 120 WallClock )
            , ( ZoneState -420 (Rules rules_NT_YK), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `America/Iqaluit`
-}
america__iqaluit : () -> Time.Zone
america__iqaluit _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_NT_YK), DateTime 1999 Oct 31 120 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2000 Oct 29 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Jamaica`
-}
america__jamaica : () -> Time.Zone
america__jamaica _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1974 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 1984 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Juneau`
-}
america__juneau : () -> Time.Zone
america__juneau _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1980 Apr 27 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1980 Oct 26 120 WallClock )
            , ( ZoneState -480 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Kentucky/Louisville`
-}
america__kentucky__louisville : () -> Time.Zone
america__kentucky__louisville _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1974 Jan 6 120 WallClock )
            , ( ZoneState -360 (Save 60), DateTime 1974 Oct 27 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Kentucky/Monticello`
-}
america__kentucky__monticello : () -> Time.Zone
america__kentucky__monticello _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_US), DateTime 2000 Oct 29 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/La_Paz`
-}
america__la_paz : () -> Time.Zone
america__la_paz _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Lima`
-}
america__lima : () -> Time.Zone
america__lima _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Rules rules_Peru))


{-| `America/Los_Angeles`
-}
america__los_angeles : () -> Time.Zone
america__los_angeles _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -480 (Rules rules_US))


{-| `America/Maceio`
-}
america__maceio : () -> Time.Zone
america__maceio _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1995 Oct 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 1996 Sep 4 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2000 Oct 22 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Managua`
-}
america__managua : () -> Time.Zone
america__managua _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1973 May 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1975 Feb 16 0 WallClock )
            , ( ZoneState -360 (Rules rules_Nic), DateTime 1992 Jan 1 240 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1992 Sep 24 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1993 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Nic))


{-| `America/Manaus`
-}
america__manaus : () -> Time.Zone
america__manaus _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1993 Sep 28 0 WallClock )
            , ( ZoneState -240 (Rules rules_Brazil), DateTime 1994 Sep 22 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Martinique`
-}
america__martinique : () -> Time.Zone
america__martinique _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1980 Apr 6 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1980 Sep 28 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Matamoros`
-}
america__matamoros : () -> Time.Zone
america__matamoros _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1988 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 1989 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Mazatlan`
-}
america__mazatlan : () -> Time.Zone
america__mazatlan _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Mexico))


{-| `America/Menominee`
-}
america__menominee : () -> Time.Zone
america__menominee _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1973 Apr 29 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Merida`
-}
america__merida : () -> Time.Zone
america__merida _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1981 Dec 26 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1982 Nov 2 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Metlakatla`
-}
america__metlakatla : () -> Time.Zone
america__metlakatla _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -480 (Save 0), DateTime 2015 Nov 1 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 2018 Nov 4 120 WallClock )
            , ( ZoneState -480 (Save 0), DateTime 2019 Jan 20 120 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Mexico_City`
-}
america__mexico_city : () -> Time.Zone
america__mexico_city _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_Mexico), DateTime 2001 Sep 30 120 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2002 Feb 20 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Miquelon`
-}
america__miquelon : () -> Time.Zone
america__miquelon _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1980 May 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1987 Jan 1 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Canada))


{-| `America/Moncton`
-}
america__moncton : () -> Time.Zone
america__moncton _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Moncton), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Canada), DateTime 1993 Jan 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Moncton), DateTime 2007 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Monterrey`
-}
america__monterrey : () -> Time.Zone
america__monterrey _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1988 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 1989 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Montevideo`
-}
america__montevideo : () -> Time.Zone
america__montevideo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Uruguay), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Uruguay), DateTime 1974 Jan 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Uruguay), DateTime 1974 Mar 10 0 WallClock )
            , ( ZoneState -180 (Rules rules_Uruguay), DateTime 1974 Dec 22 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Uruguay))


{-| `America/Montreal`
-}
america__montreal : () -> Time.Zone
america__montreal _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_Mont), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Montserrat`
-}
america__montserrat : () -> Time.Zone
america__montserrat _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Nassau`
-}
america__nassau : () -> Time.Zone
america__nassau _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_Bahamas), DateTime 1976 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/New_York`
-}
america__new_york : () -> Time.Zone
america__new_york _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Rules rules_US))


{-| `America/Nipigon`
-}
america__nipigon : () -> Time.Zone
america__nipigon _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `America/Nome`
-}
america__nome : () -> Time.Zone
america__nome _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -660 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Noronha`
-}
america__noronha : () -> Time.Zone
america__noronha _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -120 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -120 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -120 (Rules rules_Brazil), DateTime 2000 Oct 15 0 WallClock )
            , ( ZoneState -120 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -120 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -120 (Save 0))


{-| `America/North_Dakota/Beulah`
-}
america__north_dakota__beulah : () -> Time.Zone
america__north_dakota__beulah _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 2010 Nov 7 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/North_Dakota/Center`
-}
america__north_dakota__center : () -> Time.Zone
america__north_dakota__center _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 1992 Oct 25 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/North_Dakota/New_Salem`
-}
america__north_dakota__new_salem : () -> Time.Zone
america__north_dakota__new_salem _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 2003 Oct 26 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Nuuk`
-}
america__nuuk : () -> Time.Zone
america__nuuk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Save 0), DateTime 1980 Apr 6 120 WallClock )
            , ( ZoneState -180 (Rules rules_EU), DateTime 2023 Mar 26 60 Universal )
            , ( ZoneState -120 (Save 0), DateTime 2023 Oct 29 60 Universal )
            ]
            (ZoneState -120 (Rules rules_EU))


{-| `America/Ojinaga`
-}
america__ojinaga : () -> Time.Zone
america__ojinaga _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1998 Apr 5 180 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            , ( ZoneState -420 (Rules rules_US), DateTime 2022 Oct 30 120 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2022 Nov 30 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Panama`
-}
america__panama : () -> Time.Zone
america__panama _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `America/Pangnirtung`
-}
america__pangnirtung : () -> Time.Zone
america__pangnirtung _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1999 Oct 31 120 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2000 Oct 29 120 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Paramaribo`
-}
america__paramaribo : () -> Time.Zone
america__paramaribo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -210 (Save 0), DateTime 1984 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Phoenix`
-}
america__phoenix : () -> Time.Zone
america__phoenix _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -420 (Save 0))


{-| `America/Port-au-Prince`
-}
america__port_au_prince : () -> Time.Zone
america__port_au_prince _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Rules rules_Haiti))


{-| `America/Port_of_Spain`
-}
america__port_of_spain : () -> Time.Zone
america__port_of_spain _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Porto_Velho`
-}
america__porto_velho : () -> Time.Zone
america__porto_velho _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Puerto_Rico`
-}
america__puerto_rico : () -> Time.Zone
america__puerto_rico _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Punta_Arenas`
-}
america__punta_arenas : () -> Time.Zone
america__punta_arenas _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Chile), DateTime 2016 Dec 4 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Rainy_River`
-}
america__rainy_river : () -> Time.Zone
america__rainy_river _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Save 0))


{-| `America/Rankin_Inlet`
-}
america__rankin_inlet : () -> Time.Zone
america__rankin_inlet _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_NT_YK), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2001 Apr 1 180 WallClock )
            ]
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Recife`
-}
america__recife : () -> Time.Zone
america__recife _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2000 Oct 15 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Regina`
-}
america__regina : () -> Time.Zone
america__regina _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Save 0))


{-| `America/Resolute`
-}
america__resolute : () -> Time.Zone
america__resolute _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_NT_YK), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2001 Apr 1 180 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2006 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2007 Mar 11 180 WallClock )
            ]
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Rio_Branco`
-}
america__rio_branco : () -> Time.Zone
america__rio_branco _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2008 Jun 24 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2013 Nov 10 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Rosario`
-}
america__rosario : () -> Time.Zone
america__rosario _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Save 0), DateTime 1991 Jul 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2000 Mar 3 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Santarem`
-}
america__santarem : () -> Time.Zone
america__santarem _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2008 Jun 24 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Santiago`
-}
america__santiago : () -> Time.Zone
america__santiago _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Rules rules_Chile))


{-| `America/Santo_Domingo`
-}
america__santo_domingo : () -> Time.Zone
america__santo_domingo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_DR), DateTime 1974 Oct 27 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 2000 Dec 3 60 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Sao_Paulo`
-}
america__sao_paulo : () -> Time.Zone
america__sao_paulo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -180 (Rules rules_Brazil))


{-| `America/Scoresbysund`
-}
america__scoresbysund : () -> Time.Zone
america__scoresbysund _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -120 (Save 0), DateTime 1980 Apr 6 120 WallClock )
            , ( ZoneState -120 (Rules rules_C_Eur), DateTime 1981 Mar 29 0 WallClock )
            , ( ZoneState -60 (Rules rules_EU), DateTime 2024 Mar 31 0 WallClock )
            ]
            (ZoneState -120 (Rules rules_EU))


{-| `America/Sitka`
-}
america__sitka : () -> Time.Zone
america__sitka _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/St_Johns`
-}
america__st_johns : () -> Time.Zone
america__st_johns _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -210 (Rules rules_StJohns), DateTime 2011 Nov 1 0 WallClock )
            ]
            (ZoneState -210 (Rules rules_Canada))


{-| `America/St_Kitts`
-}
america__st_kitts : () -> Time.Zone
america__st_kitts _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/St_Lucia`
-}
america__st_lucia : () -> Time.Zone
america__st_lucia _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/St_Thomas`
-}
america__st_thomas : () -> Time.Zone
america__st_thomas _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/St_Vincent`
-}
america__st_vincent : () -> Time.Zone
america__st_vincent _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Swift_Current`
-}
america__swift_current : () -> Time.Zone
america__swift_current _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Save 0), DateTime 1972 Apr 30 120 WallClock )
            ]
            (ZoneState -360 (Save 0))


{-| `America/Tegucigalpa`
-}
america__tegucigalpa : () -> Time.Zone
america__tegucigalpa _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Rules rules_Hond))


{-| `America/Thule`
-}
america__thule : () -> Time.Zone
america__thule _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Rules rules_Thule))


{-| `America/Thunder_Bay`
-}
america__thunder_bay : () -> Time.Zone
america__thunder_bay _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Tijuana`
-}
america__tijuana : () -> Time.Zone
america__tijuana _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Mexico), DateTime 2001 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_US), DateTime 2002 Feb 20 0 WallClock )
            , ( ZoneState -480 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            ]
            (ZoneState -480 (Rules rules_US))


{-| `America/Toronto`
-}
america__toronto : () -> Time.Zone
america__toronto _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Rules rules_Toronto), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Tortola`
-}
america__tortola : () -> Time.Zone
america__tortola _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Vancouver`
-}
america__vancouver : () -> Time.Zone
america__vancouver _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Rules rules_Vanc), DateTime 1987 Jan 1 0 WallClock )
            ]
            (ZoneState -480 (Rules rules_Canada))


{-| `America/Whitehorse`
-}
america__whitehorse : () -> Time.Zone
america__whitehorse _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -480 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Canada), DateTime 2020 Nov 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Winnipeg`
-}
america__winnipeg : () -> Time.Zone
america__winnipeg _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -360 (Rules rules_Winn), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Yakutat`
-}
america__yakutat : () -> Time.Zone
america__yakutat _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Yellowknife`
-}
america__yellowknife : () -> Time.Zone
america__yellowknife _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `Antarctica/Casey`
-}
antarctica__casey : () -> Time.Zone
antarctica__casey _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 480 (Save 0), DateTime 2009 Oct 18 120 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2010 Mar 5 120 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2011 Oct 28 120 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2012 Feb 21 1020 Universal )
            , ( ZoneState 480 (Save 0), DateTime 2016 Oct 22 0 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2018 Mar 11 240 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2018 Oct 7 240 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2019 Mar 17 180 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2019 Oct 4 180 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2020 Mar 8 180 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2020 Oct 4 1 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2021 Mar 14 0 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2021 Oct 3 1 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2022 Mar 13 0 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2022 Oct 2 1 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2023 Mar 9 180 WallClock )
            ]
            (ZoneState 480 (Save 0))


{-| `Antarctica/Davis`
-}
antarctica__davis : () -> Time.Zone
antarctica__davis _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 2009 Oct 18 120 WallClock )
            , ( ZoneState 300 (Save 0), DateTime 2010 Mar 10 1200 Universal )
            , ( ZoneState 420 (Save 0), DateTime 2011 Oct 28 120 WallClock )
            , ( ZoneState 300 (Save 0), DateTime 2012 Feb 21 1200 Universal )
            ]
            (ZoneState 420 (Save 0))


{-| `Antarctica/DumontDUrville`
-}
antarctica__dumontdurville : () -> Time.Zone
antarctica__dumontdurville _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Antarctica/Macquarie`
-}
antarctica__macquarie : () -> Time.Zone
antarctica__macquarie _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Rules rules_AT), DateTime 2010 Jan 1 0 WallClock )
            , ( ZoneState 600 (Save 60), DateTime 2011 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AT))


{-| `Antarctica/Mawson`
-}
antarctica__mawson : () -> Time.Zone
antarctica__mawson _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Save 0), DateTime 2009 Oct 18 120 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Antarctica/McMurdo`
-}
antarctica__mcmurdo : () -> Time.Zone
antarctica__mcmurdo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Antarctica/Palmer`
-}
antarctica__palmer : () -> Time.Zone
antarctica__palmer _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1982 May 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Chile), DateTime 2016 Dec 4 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `Antarctica/Rothera`
-}
antarctica__rothera : () -> Time.Zone
antarctica__rothera _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1976 Dec 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `Antarctica/Syowa`
-}
antarctica__syowa : () -> Time.Zone
antarctica__syowa _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Antarctica/Troll`
-}
antarctica__troll : () -> Time.Zone
antarctica__troll _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 2005 Feb 12 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_Troll))


{-| `Antarctica/Vostok`
-}
antarctica__vostok : () -> Time.Zone
antarctica__vostok _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 1994 Feb 1 0 WallClock )
            , ( ZoneState 0 (Save 0), DateTime 1994 Nov 1 0 WallClock )
            , ( ZoneState 420 (Save 0), DateTime 2023 Dec 18 120 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Aden`
-}
asia__aden : () -> Time.Zone
asia__aden _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Asia/Almaty`
-}
asia__almaty : () -> Time.Zone
asia__almaty _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2024 Mar 1 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Amman`
-}
asia__amman : () -> Time.Zone
asia__amman _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Jordan), DateTime 2022 Oct 28 0 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Asia/Anadyr`
-}
asia__anadyr : () -> Time.Zone
asia__anadyr _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 780 (Rules rules_Russia), DateTime 1982 Apr 1 0 Standard )
            , ( ZoneState 720 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 720 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 720 (Save 0))


{-| `Asia/Aqtau`
-}
asia__aqtau : () -> Time.Zone
asia__aqtau _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1994 Sep 25 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Aqtobe`
-}
asia__aqtobe : () -> Time.Zone
asia__aqtobe _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Ashgabat`
-}
asia__ashgabat : () -> Time.Zone
asia__ashgabat _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Atyrau`
-}
asia__atyrau : () -> Time.Zone
asia__atyrau _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1999 Mar 28 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Baghdad`
-}
asia__baghdad : () -> Time.Zone
asia__baghdad _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Save 0), DateTime 1982 May 1 0 WallClock )
            ]
            (ZoneState 180 (Rules rules_Iraq))


{-| `Asia/Bahrain`
-}
asia__bahrain : () -> Time.Zone
asia__bahrain _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Save 0), DateTime 1972 Jun 1 0 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Asia/Baku`
-}
asia__baku : () -> Time.Zone
asia__baku _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 1992 Sep 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState 240 (Rules rules_EUAsia), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 240 (Rules rules_Azer))


{-| `Asia/Bangkok`
-}
asia__bangkok : () -> Time.Zone
asia__bangkok _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Barnaul`
-}
asia__barnaul : () -> Time.Zone
asia__barnaul _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 1995 May 28 0 WallClock )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Beirut`
-}
asia__beirut : () -> Time.Zone
asia__beirut _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Rules rules_Lebanon))


{-| `Asia/Bishkek`
-}
asia__bishkek : () -> Time.Zone
asia__bishkek _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Aug 31 120 WallClock )
            , ( ZoneState 300 (Rules rules_Kyrgyz), DateTime 2005 Aug 12 0 WallClock )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Brunei`
-}
asia__brunei : () -> Time.Zone
asia__brunei _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Asia/Chita`
-}
asia__chita : () -> Time.Zone
asia__chita _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 480 (Save 0), DateTime 2016 Mar 27 120 WallClock )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Chongqing`
-}
asia__chongqing : () -> Time.Zone
asia__chongqing _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 1980 May 1 0 WallClock )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Colombo`
-}
asia__colombo : () -> Time.Zone
asia__colombo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 330 (Save 0), DateTime 1996 May 25 0 WallClock )
            , ( ZoneState 390 (Save 0), DateTime 1996 Oct 26 30 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 2006 Apr 15 30 WallClock )
            ]
            (ZoneState 330 (Save 0))


{-| `Asia/Damascus`
-}
asia__damascus : () -> Time.Zone
asia__damascus _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Syria), DateTime 2022 Oct 28 0 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Asia/Dhaka`
-}
asia__dhaka : () -> Time.Zone
asia__dhaka _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Save 0), DateTime 2009 Jan 1 0 WallClock )
            ]
            (ZoneState 360 (Rules rules_Dhaka))


{-| `Asia/Dili`
-}
asia__dili : () -> Time.Zone
asia__dili _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 540 (Save 0), DateTime 1976 May 3 0 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2000 Sep 17 0 WallClock )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Dubai`
-}
asia__dubai : () -> Time.Zone
asia__dubai _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Asia/Dushanbe`
-}
asia__dushanbe : () -> Time.Zone
asia__dushanbe _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Save 60), DateTime 1991 Sep 9 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Famagusta`
-}
asia__famagusta : () -> Time.Zone
asia__famagusta _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Cyprus), DateTime 1998 Sep 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_EUAsia), DateTime 2016 Sep 8 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 2017 Oct 29 60 Universal )
            ]
            (ZoneState 120 (Rules rules_EUAsia))


{-| `Asia/Gaza`
-}
asia__gaza : () -> Time.Zone
asia__gaza _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Zion), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Jordan), DateTime 1999 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Palestine), DateTime 2008 Aug 29 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2008 Sep 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Palestine), DateTime 2010 Jan 1 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2010 Mar 27 1 WallClock )
            , ( ZoneState 120 (Rules rules_Palestine), DateTime 2011 Aug 1 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2012 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Palestine))


{-| `Asia/Hanoi`
-}
asia__hanoi : () -> Time.Zone
asia__hanoi _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Harbin`
-}
asia__harbin : () -> Time.Zone
asia__harbin _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 510 (Save 0), DateTime 1980 May 1 0 WallClock )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Hebron`
-}
asia__hebron : () -> Time.Zone
asia__hebron _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Zion), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Jordan), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Palestine))


{-| `Asia/Ho_Chi_Minh`
-}
asia__ho_chi_minh : () -> Time.Zone
asia__ho_chi_minh _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 480 (Save 0), DateTime 1975 Jun 13 0 WallClock )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Hong_Kong`
-}
asia__hong_kong : () -> Time.Zone
asia__hong_kong _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Rules rules_HK))


{-| `Asia/Hovd`
-}
asia__hovd : () -> Time.Zone
asia__hovd _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Save 0), DateTime 1978 Jan 1 0 WallClock )
            ]
            (ZoneState 420 (Rules rules_Mongol))


{-| `Asia/Irkutsk`
-}
asia__irkutsk : () -> Time.Zone
asia__irkutsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 480 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 540 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Jakarta`
-}
asia__jakarta : () -> Time.Zone
asia__jakarta _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Jayapura`
-}
asia__jayapura : () -> Time.Zone
asia__jayapura _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Asia/Jerusalem`
-}
asia__jerusalem : () -> Time.Zone
asia__jerusalem _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Rules rules_Zion))


{-| `Asia/Kabul`
-}
asia__kabul : () -> Time.Zone
asia__kabul _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 270 (Save 0))


{-| `Asia/Kamchatka`
-}
asia__kamchatka : () -> Time.Zone
asia__kamchatka _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 720 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 720 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 720 (Save 0))


{-| `Asia/Karachi`
-}
asia__karachi : () -> Time.Zone
asia__karachi _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1971 Mar 26 0 WallClock )
            ]
            (ZoneState 300 (Rules rules_Pakistan))


{-| `Asia/Kashgar`
-}
asia__kashgar : () -> Time.Zone
asia__kashgar _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1980 May 1 0 WallClock )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Kathmandu`
-}
asia__kathmandu : () -> Time.Zone
asia__kathmandu _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 330 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            ]
            (ZoneState 345 (Save 0))


{-| `Asia/Khandyga`
-}
asia__khandyga : () -> Time.Zone
asia__khandyga _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 2004 Jan 1 0 WallClock )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2011 Sep 13 0 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Kolkata`
-}
asia__kolkata : () -> Time.Zone
asia__kolkata _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 330 (Save 0))


{-| `Asia/Krasnoyarsk`
-}
asia__krasnoyarsk : () -> Time.Zone
asia__krasnoyarsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 480 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Kuala_Lumpur`
-}
asia__kuala_lumpur : () -> Time.Zone
asia__kuala_lumpur _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 450 (Save 0), DateTime 1981 Dec 31 960 Universal )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Kuching`
-}
asia__kuching : () -> Time.Zone
asia__kuching _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Asia/Kuwait`
-}
asia__kuwait : () -> Time.Zone
asia__kuwait _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Asia/Macau`
-}
asia__macau : () -> Time.Zone
asia__macau _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Rules rules_Macau))


{-| `Asia/Magadan`
-}
asia__magadan : () -> Time.Zone
asia__magadan _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 720 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2016 Apr 24 120 Standard )
            ]
            (ZoneState 660 (Save 0))


{-| `Asia/Makassar`
-}
asia__makassar : () -> Time.Zone
asia__makassar _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Asia/Manila`
-}
asia__manila : () -> Time.Zone
asia__manila _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Rules rules_Phil))


{-| `Asia/Muscat`
-}
asia__muscat : () -> Time.Zone
asia__muscat _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Asia/Nicosia`
-}
asia__nicosia : () -> Time.Zone
asia__nicosia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Cyprus), DateTime 1998 Sep 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EUAsia))


{-| `Asia/Novokuznetsk`
-}
asia__novokuznetsk : () -> Time.Zone
asia__novokuznetsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Novosibirsk`
-}
asia__novosibirsk : () -> Time.Zone
asia__novosibirsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 1993 May 23 0 WallClock )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2016 Jul 24 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Omsk`
-}
asia__omsk : () -> Time.Zone
asia__omsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Oral`
-}
asia__oral : () -> Time.Zone
asia__oral _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Phnom_Penh`
-}
asia__phnom_penh : () -> Time.Zone
asia__phnom_penh _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Pontianak`
-}
asia__pontianak : () -> Time.Zone
asia__pontianak _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 480 (Save 0), DateTime 1988 Jan 1 0 WallClock )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Pyongyang`
-}
asia__pyongyang : () -> Time.Zone
asia__pyongyang _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 540 (Save 0), DateTime 2015 Aug 15 0 WallClock )
            , ( ZoneState 510 (Save 0), DateTime 2018 May 4 1410 WallClock )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Qatar`
-}
asia__qatar : () -> Time.Zone
asia__qatar _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Save 0), DateTime 1972 Jun 1 0 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Asia/Qostanay`
-}
asia__qostanay : () -> Time.Zone
asia__qostanay _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2024 Mar 1 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Qyzylorda`
-}
asia__qyzylorda : () -> Time.Zone
asia__qyzylorda _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Sep 29 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2018 Dec 21 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Riyadh`
-}
asia__riyadh : () -> Time.Zone
asia__riyadh _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Asia/Sakhalin`
-}
asia__sakhalin : () -> Time.Zone
asia__sakhalin _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1997 Mar 30 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 660 (Save 0))


{-| `Asia/Samarkand`
-}
asia__samarkand : () -> Time.Zone
asia__samarkand _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 1 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Seoul`
-}
asia__seoul : () -> Time.Zone
asia__seoul _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 540 (Rules rules_ROK))


{-| `Asia/Shanghai`
-}
asia__shanghai : () -> Time.Zone
asia__shanghai _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Rules rules_PRC))


{-| `Asia/Singapore`
-}
asia__singapore : () -> Time.Zone
asia__singapore _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 450 (Save 0), DateTime 1981 Dec 31 960 Universal )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Srednekolymsk`
-}
asia__srednekolymsk : () -> Time.Zone
asia__srednekolymsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 720 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 660 (Save 0))


{-| `Asia/Taipei`
-}
asia__taipei : () -> Time.Zone
asia__taipei _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Rules rules_Taiwan))


{-| `Asia/Tashkent`
-}
asia__tashkent : () -> Time.Zone
asia__tashkent _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 1 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Tbilisi`
-}
asia__tbilisi : () -> Time.Zone
asia__tbilisi _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 1992 Jan 1 0 WallClock )
            , ( ZoneState 180 (Rules rules_E_EurAsia), DateTime 1994 Sep 25 0 WallClock )
            , ( ZoneState 240 (Rules rules_E_EurAsia), DateTime 1996 Oct 27 0 WallClock )
            , ( ZoneState 240 (Save 60), DateTime 1997 Mar 30 0 WallClock )
            , ( ZoneState 240 (Rules rules_E_EurAsia), DateTime 2004 Jun 27 0 WallClock )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 2005 Mar 27 120 WallClock )
            ]
            (ZoneState 240 (Save 0))


{-| `Asia/Tehran`
-}
asia__tehran : () -> Time.Zone
asia__tehran _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 210 (Rules rules_Iran), DateTime 1977 Oct 20 1440 WallClock )
            , ( ZoneState 240 (Rules rules_Iran), DateTime 1978 Nov 10 1440 WallClock )
            ]
            (ZoneState 210 (Rules rules_Iran))


{-| `Asia/Tel_Aviv`
-}
asia__tel_aviv : () -> Time.Zone
asia__tel_aviv _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Asia/Thimphu`
-}
asia__thimphu : () -> Time.Zone
asia__thimphu _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 330 (Save 0), DateTime 1987 Oct 1 0 WallClock )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Tokyo`
-}
asia__tokyo : () -> Time.Zone
asia__tokyo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Asia/Tomsk`
-}
asia__tomsk : () -> Time.Zone
asia__tomsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 2002 May 1 180 WallClock )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2016 May 29 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Ulaanbaatar`
-}
asia__ulaanbaatar : () -> Time.Zone
asia__ulaanbaatar _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 1978 Jan 1 0 WallClock )
            ]
            (ZoneState 480 (Rules rules_Mongol))


{-| `Asia/Urumqi`
-}
asia__urumqi : () -> Time.Zone
asia__urumqi _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 360 (Save 0))


{-| `Asia/Ust-Nera`
-}
asia__ust_nera : () -> Time.Zone
asia__ust_nera _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 720 (Save 0), DateTime 2011 Sep 13 0 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 600 (Save 0))


{-| `Asia/Vientiane`
-}
asia__vientiane : () -> Time.Zone
asia__vientiane _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Vladivostok`
-}
asia__vladivostok : () -> Time.Zone
asia__vladivostok _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 600 (Save 0))


{-| `Asia/Yakutsk`
-}
asia__yakutsk : () -> Time.Zone
asia__yakutsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Yangon`
-}
asia__yangon : () -> Time.Zone
asia__yangon _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 390 (Save 0))


{-| `Asia/Yekaterinburg`
-}
asia__yekaterinburg : () -> Time.Zone
asia__yekaterinburg _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Yerevan`
-}
asia__yerevan : () -> Time.Zone
asia__yerevan _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 1995 Sep 24 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1997 Jan 1 0 WallClock )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2011 Jan 1 0 WallClock )
            ]
            (ZoneState 240 (Rules rules_Armenia))


{-| `Atlantic/Azores`
-}
atlantic__azores : () -> Time.Zone
atlantic__azores _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -60 (Save 0), DateTime 1982 Mar 28 0 Standard )
            , ( ZoneState -60 (Rules rules_Port), DateTime 1986 Jan 1 0 WallClock )
            , ( ZoneState -60 (Rules rules_EU), DateTime 1992 Dec 27 60 Standard )
            , ( ZoneState 0 (Rules rules_EU), DateTime 1993 Jun 17 60 Universal )
            ]
            (ZoneState -60 (Rules rules_EU))


{-| `Atlantic/Bermuda`
-}
atlantic__bermuda : () -> Time.Zone
atlantic__bermuda _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1974 Apr 28 120 WallClock )
            , ( ZoneState -240 (Rules rules_Canada), DateTime 1976 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_US))


{-| `Atlantic/Canary`
-}
atlantic__canary : () -> Time.Zone
atlantic__canary _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1980 Apr 6 0 Standard )
            , ( ZoneState 0 (Save 60), DateTime 1980 Sep 28 60 Universal )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Atlantic/Cape_Verde`
-}
atlantic__cape_verde : () -> Time.Zone
atlantic__cape_verde _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -120 (Save 0), DateTime 1975 Nov 25 120 WallClock )
            ]
            (ZoneState -60 (Save 0))


{-| `Atlantic/Faroe`
-}
atlantic__faroe : () -> Time.Zone
atlantic__faroe _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Atlantic/Jan_Mayen`
-}
atlantic__jan_mayen : () -> Time.Zone
atlantic__jan_mayen _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -60 (Save 0))


{-| `Atlantic/Madeira`
-}
atlantic__madeira : () -> Time.Zone
atlantic__madeira _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1982 Apr 4 0 WallClock )
            , ( ZoneState 0 (Rules rules_Port), DateTime 1986 Jul 31 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Atlantic/Reykjavik`
-}
atlantic__reykjavik : () -> Time.Zone
atlantic__reykjavik _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Atlantic/South_Georgia`
-}
atlantic__south_georgia : () -> Time.Zone
atlantic__south_georgia _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -120 (Save 0))


{-| `Atlantic/St_Helena`
-}
atlantic__st_helena : () -> Time.Zone
atlantic__st_helena _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Atlantic/Stanley`
-}
atlantic__stanley : () -> Time.Zone
atlantic__stanley _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -240 (Rules rules_Falk), DateTime 1983 May 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Falk), DateTime 1985 Sep 15 0 WallClock )
            , ( ZoneState -240 (Rules rules_Falk), DateTime 2010 Sep 5 120 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `Australia/Adelaide`
-}
australia__adelaide : () -> Time.Zone
australia__adelaide _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 570 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 570 (Rules rules_AS))


{-| `Australia/Brisbane`
-}
australia__brisbane : () -> Time.Zone
australia__brisbane _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AQ))


{-| `Australia/Broken_Hill`
-}
australia__broken_hill : () -> Time.Zone
australia__broken_hill _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 570 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState 570 (Rules rules_AN), DateTime 2000 Jan 1 0 WallClock )
            ]
            (ZoneState 570 (Rules rules_AS))


{-| `Australia/Currie`
-}
australia__currie : () -> Time.Zone
australia__currie _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Australia/Darwin`
-}
australia__darwin : () -> Time.Zone
australia__darwin _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 570 (Save 0))


{-| `Australia/Eucla`
-}
australia__eucla : () -> Time.Zone
australia__eucla _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 525 (Rules rules_AW))


{-| `Australia/Hobart`
-}
australia__hobart : () -> Time.Zone
australia__hobart _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 600 (Rules rules_AT))


{-| `Australia/Lindeman`
-}
australia__lindeman : () -> Time.Zone
australia__lindeman _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState 600 (Rules rules_AQ), DateTime 1992 Jul 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_Holiday))


{-| `Australia/Lord_Howe`
-}
australia__lord_howe : () -> Time.Zone
australia__lord_howe _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1981 Mar 1 0 WallClock )
            , ( ZoneState 630 (Rules rules_LH), DateTime 1985 Jul 1 0 WallClock )
            ]
            (ZoneState 630 (Rules rules_LH))


{-| `Australia/Melbourne`
-}
australia__melbourne : () -> Time.Zone
australia__melbourne _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AV))


{-| `Australia/Perth`
-}
australia__perth : () -> Time.Zone
australia__perth _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Rules rules_AW))


{-| `Australia/Sydney`
-}
australia__sydney : () -> Time.Zone
australia__sydney _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AN))


{-| `CET`
-}
cet : () -> Time.Zone
cet _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `CST6CDT`
-}
cst6cdt : () -> Time.Zone
cst6cdt _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Save 0))


{-| `EET`
-}
eet : () -> Time.Zone
eet _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `EST`
-}
est : () -> Time.Zone
est _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `EST5EDT`
-}
est5edt : () -> Time.Zone
est5edt _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `Etc/GMT`
-}
etc__gmt : () -> Time.Zone
etc__gmt _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Etc/GMT+1`
-}
etc__gmt_plus1 : () -> Time.Zone
etc__gmt_plus1 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -60 (Save 0))


{-| `Etc/GMT+10`
-}
etc__gmt_plus10 : () -> Time.Zone
etc__gmt_plus10 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Etc/GMT+11`
-}
etc__gmt_plus11 : () -> Time.Zone
etc__gmt_plus11 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -660 (Save 0))


{-| `Etc/GMT+12`
-}
etc__gmt_plus12 : () -> Time.Zone
etc__gmt_plus12 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -720 (Save 0))


{-| `Etc/GMT+2`
-}
etc__gmt_plus2 : () -> Time.Zone
etc__gmt_plus2 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -120 (Save 0))


{-| `Etc/GMT+3`
-}
etc__gmt_plus3 : () -> Time.Zone
etc__gmt_plus3 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -180 (Save 0))


{-| `Etc/GMT+4`
-}
etc__gmt_plus4 : () -> Time.Zone
etc__gmt_plus4 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `Etc/GMT+5`
-}
etc__gmt_plus5 : () -> Time.Zone
etc__gmt_plus5 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `Etc/GMT+6`
-}
etc__gmt_plus6 : () -> Time.Zone
etc__gmt_plus6 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -360 (Save 0))


{-| `Etc/GMT+7`
-}
etc__gmt_plus7 : () -> Time.Zone
etc__gmt_plus7 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -420 (Save 0))


{-| `Etc/GMT+8`
-}
etc__gmt_plus8 : () -> Time.Zone
etc__gmt_plus8 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -480 (Save 0))


{-| `Etc/GMT+9`
-}
etc__gmt_plus9 : () -> Time.Zone
etc__gmt_plus9 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -540 (Save 0))


{-| `Etc/GMT-1`
-}
etc__gmt_1 : () -> Time.Zone
etc__gmt_1 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Etc/GMT-10`
-}
etc__gmt_10 : () -> Time.Zone
etc__gmt_10 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Etc/GMT-11`
-}
etc__gmt_11 : () -> Time.Zone
etc__gmt_11 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 660 (Save 0))


{-| `Etc/GMT-12`
-}
etc__gmt_12 : () -> Time.Zone
etc__gmt_12 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Etc/GMT-13`
-}
etc__gmt_13 : () -> Time.Zone
etc__gmt_13 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 780 (Save 0))


{-| `Etc/GMT-14`
-}
etc__gmt_14 : () -> Time.Zone
etc__gmt_14 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 840 (Save 0))


{-| `Etc/GMT-2`
-}
etc__gmt_2 : () -> Time.Zone
etc__gmt_2 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Etc/GMT-3`
-}
etc__gmt_3 : () -> Time.Zone
etc__gmt_3 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Etc/GMT-4`
-}
etc__gmt_4 : () -> Time.Zone
etc__gmt_4 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Etc/GMT-5`
-}
etc__gmt_5 : () -> Time.Zone
etc__gmt_5 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 300 (Save 0))


{-| `Etc/GMT-6`
-}
etc__gmt_6 : () -> Time.Zone
etc__gmt_6 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 360 (Save 0))


{-| `Etc/GMT-7`
-}
etc__gmt_7 : () -> Time.Zone
etc__gmt_7 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Etc/GMT-8`
-}
etc__gmt_8 : () -> Time.Zone
etc__gmt_8 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Etc/GMT-9`
-}
etc__gmt_9 : () -> Time.Zone
etc__gmt_9 _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Etc/UTC`
-}
etc__utc : () -> Time.Zone
etc__utc _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Europe/Amsterdam`
-}
europe__amsterdam : () -> Time.Zone
europe__amsterdam _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Andorra`
-}
europe__andorra : () -> Time.Zone
europe__andorra _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1985 Mar 31 120 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Astrakhan`
-}
europe__astrakhan : () -> Time.Zone
europe__astrakhan _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Athens`
-}
europe__athens : () -> Time.Zone
europe__athens _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Greece), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Belfast`
-}
europe__belfast : () -> Time.Zone
europe__belfast _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1971 Oct 31 120 Universal )
            , ( ZoneState 0 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Europe/Belgrade`
-}
europe__belgrade : () -> Time.Zone
europe__belgrade _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Nov 27 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Berlin`
-}
europe__berlin : () -> Time.Zone
europe__berlin _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Brussels`
-}
europe__brussels : () -> Time.Zone
europe__brussels _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Bucharest`
-}
europe__bucharest : () -> Time.Zone
europe__bucharest _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Romania), DateTime 1981 Mar 29 120 Standard )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1991 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Romania), DateTime 1994 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Budapest`
-}
europe__budapest : () -> Time.Zone
europe__budapest _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_Hungary), DateTime 1984 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Chisinau`
-}
europe__chisinau : () -> Time.Zone
europe__chisinau _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 May 6 120 WallClock )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1992 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Moldova))


{-| `Europe/Copenhagen`
-}
europe__copenhagen : () -> Time.Zone
europe__copenhagen _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Dublin`
-}
europe__dublin : () -> Time.Zone
europe__dublin _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Rules rules_Eire))


{-| `Europe/Gibraltar`
-}
europe__gibraltar : () -> Time.Zone
europe__gibraltar _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Guernsey`
-}
europe__guernsey : () -> Time.Zone
europe__guernsey _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1971 Oct 31 120 Universal )
            , ( ZoneState 0 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Europe/Helsinki`
-}
europe__helsinki : () -> Time.Zone
europe__helsinki _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Finland), DateTime 1983 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Isle_of_Man`
-}
europe__isle_of_man : () -> Time.Zone
europe__isle_of_man _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1971 Oct 31 120 Universal )
            , ( ZoneState 0 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Europe/Istanbul`
-}
europe__istanbul : () -> Time.Zone
europe__istanbul _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Rules rules_Turkey), DateTime 1978 Jun 29 0 WallClock )
            , ( ZoneState 180 (Rules rules_Turkey), DateTime 1984 Nov 1 120 WallClock )
            , ( ZoneState 120 (Rules rules_Turkey), DateTime 2007 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2011 Mar 27 60 Universal )
            , ( ZoneState 120 (Save 0), DateTime 2011 Mar 28 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2014 Mar 30 60 Universal )
            , ( ZoneState 120 (Save 0), DateTime 2014 Mar 31 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2015 Oct 25 60 Universal )
            , ( ZoneState 120 (Save 60), DateTime 2015 Nov 8 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2016 Sep 7 0 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Jersey`
-}
europe__jersey : () -> Time.Zone
europe__jersey _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1971 Oct 31 120 Universal )
            , ( ZoneState 0 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Europe/Kaliningrad`
-}
europe__kaliningrad : () -> Time.Zone
europe__kaliningrad _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 120 (Save 0))


{-| `Europe/Kirov`
-}
europe__kirov : () -> Time.Zone
europe__kirov _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Kyiv`
-}
europe__kyiv : () -> Time.Zone
europe__kyiv _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jul 1 120 WallClock )
            , ( ZoneState 120 (Save 60), DateTime 1991 Sep 29 180 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1996 May 13 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Lisbon`
-}
europe__lisbon : () -> Time.Zone
europe__lisbon _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1976 Sep 26 60 WallClock )
            , ( ZoneState 0 (Rules rules_Port), DateTime 1986 Jan 1 0 WallClock )
            , ( ZoneState 0 (Rules rules_EU), DateTime 1992 Sep 27 60 Universal )
            , ( ZoneState 60 (Rules rules_EU), DateTime 1996 Mar 31 60 Universal )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Europe/Ljubljana`
-}
europe__ljubljana : () -> Time.Zone
europe__ljubljana _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Nov 27 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/London`
-}
europe__london : () -> Time.Zone
europe__london _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1971 Oct 31 120 Universal )
            , ( ZoneState 0 (Rules rules_GB_Eire), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Europe/Luxembourg`
-}
europe__luxembourg : () -> Time.Zone
europe__luxembourg _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Madrid`
-}
europe__madrid : () -> Time.Zone
europe__madrid _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_Spain), DateTime 1979 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Malta`
-}
europe__malta : () -> Time.Zone
europe__malta _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_Italy), DateTime 1973 Mar 31 0 WallClock )
            , ( ZoneState 60 (Rules rules_Malta), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Minsk`
-}
europe__minsk : () -> Time.Zone
europe__minsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Monaco`
-}
europe__monaco : () -> Time.Zone
europe__monaco _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Moscow`
-}
europe__moscow : () -> Time.Zone
europe__moscow _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Oslo`
-}
europe__oslo : () -> Time.Zone
europe__oslo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Paris`
-}
europe__paris : () -> Time.Zone
europe__paris _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_France), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Prague`
-}
europe__prague : () -> Time.Zone
europe__prague _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1979 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Riga`
-}
europe__riga : () -> Time.Zone
europe__riga _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Save 60), DateTime 1989 Sep 24 120 Standard )
            , ( ZoneState 120 (Rules rules_Latvia), DateTime 1997 Jan 21 0 WallClock )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2000 Feb 29 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2001 Jan 2 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Rome`
-}
europe__rome : () -> Time.Zone
europe__rome _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_Italy), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Samara`
-}
europe__samara : () -> Time.Zone
europe__samara _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1991 Sep 29 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 1991 Oct 20 180 WallClock )
            , ( ZoneState 240 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Sarajevo`
-}
europe__sarajevo : () -> Time.Zone
europe__sarajevo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Nov 27 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Saratov`
-}
europe__saratov : () -> Time.Zone
europe__saratov _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1988 Mar 27 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2016 Dec 4 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Simferopol`
-}
europe__simferopol : () -> Time.Zone
europe__simferopol _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1990 Jul 1 120 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1992 Mar 20 0 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1994 May 1 0 WallClock )
            , ( ZoneState 180 (Rules rules_C_Eur), DateTime 1996 Mar 31 0 Standard )
            , ( ZoneState 180 (Save 60), DateTime 1996 Oct 27 180 Standard )
            , ( ZoneState 180 (Save 0), DateTime 1997 Mar 30 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2014 Mar 30 120 WallClock )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Skopje`
-}
europe__skopje : () -> Time.Zone
europe__skopje _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Nov 27 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Sofia`
-}
europe__sofia : () -> Time.Zone
europe__sofia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 120 (Save 0), DateTime 1979 Mar 31 1380 WallClock )
            , ( ZoneState 120 (Rules rules_Bulg), DateTime 1982 Sep 26 180 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1991 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Stockholm`
-}
europe__stockholm : () -> Time.Zone
europe__stockholm _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Tallinn`
-}
europe__tallinn : () -> Time.Zone
europe__tallinn _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Save 60), DateTime 1989 Sep 24 120 Standard )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1998 Sep 22 0 WallClock )
            , ( ZoneState 120 (Rules rules_EU), DateTime 1999 Oct 31 240 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2002 Feb 21 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Tirane`
-}
europe__tirane : () -> Time.Zone
europe__tirane _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_Albania), DateTime 1984 Jul 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Tiraspol`
-}
europe__tiraspol : () -> Time.Zone
europe__tiraspol _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Save 0), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1992 Jan 19 120 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Ulyanovsk`
-}
europe__ulyanovsk : () -> Time.Zone
europe__ulyanovsk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Uzhgorod`
-}
europe__uzhgorod : () -> Time.Zone
europe__uzhgorod _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Save 0), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1990 Jul 1 120 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 1991 Mar 31 180 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1992 Mar 20 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1996 May 13 0 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Europe/Vaduz`
-}
europe__vaduz : () -> Time.Zone
europe__vaduz _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Vienna`
-}
europe__vienna : () -> Time.Zone
europe__vienna _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Rules rules_Austria), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Vilnius`
-}
europe__vilnius : () -> Time.Zone
europe__vilnius _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1991 Sep 29 120 Standard )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1998 Mar 29 60 Universal )
            , ( ZoneState 60 (Rules rules_EU), DateTime 1999 Oct 31 60 Universal )
            , ( ZoneState 120 (Save 0), DateTime 2003 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Volgograd`
-}
europe__volgograd : () -> Time.Zone
europe__volgograd _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1988 Mar 27 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2018 Oct 28 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2020 Dec 27 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Warsaw`
-}
europe__warsaw : () -> Time.Zone
europe__warsaw _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            , ( ZoneState 60 (Rules rules_W_Eur), DateTime 1988 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Zagreb`
-}
europe__zagreb : () -> Time.Zone
europe__zagreb _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Nov 27 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Europe/Zaporozhye`
-}
europe__zaporozhye : () -> Time.Zone
europe__zaporozhye _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 180 (Save 0), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1992 Mar 20 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1996 May 13 0 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Europe/Zurich`
-}
europe__zurich : () -> Time.Zone
europe__zurich _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `HST`
-}
hst : () -> Time.Zone
hst _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Indian/Antananarivo`
-}
indian__antananarivo : () -> Time.Zone
indian__antananarivo _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Indian/Chagos`
-}
indian__chagos : () -> Time.Zone
indian__chagos _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 360 (Save 0))


{-| `Indian/Christmas`
-}
indian__christmas : () -> Time.Zone
indian__christmas _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Indian/Cocos`
-}
indian__cocos : () -> Time.Zone
indian__cocos _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 390 (Save 0))


{-| `Indian/Comoro`
-}
indian__comoro : () -> Time.Zone
indian__comoro _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Indian/Kerguelen`
-}
indian__kerguelen : () -> Time.Zone
indian__kerguelen _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 300 (Save 0))


{-| `Indian/Mahe`
-}
indian__mahe : () -> Time.Zone
indian__mahe _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Indian/Maldives`
-}
indian__maldives : () -> Time.Zone
indian__maldives _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 300 (Save 0))


{-| `Indian/Mauritius`
-}
indian__mauritius : () -> Time.Zone
indian__mauritius _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 240 (Rules rules_Mauritius))


{-| `Indian/Mayotte`
-}
indian__mayotte : () -> Time.Zone
indian__mayotte _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Indian/Reunion`
-}
indian__reunion : () -> Time.Zone
indian__reunion _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `MET`
-}
met : () -> Time.Zone
met _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `MST`
-}
mst : () -> Time.Zone
mst _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -420 (Save 0))


{-| `MST7MDT`
-}
mst7mdt : () -> Time.Zone
mst7mdt _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -420 (Save 0))


{-| `PST8PDT`
-}
pst8pdt : () -> Time.Zone
pst8pdt _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -480 (Save 0))


{-| `Pacific/Apia`
-}
pacific__apia : () -> Time.Zone
pacific__apia _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -660 (Rules rules_WS), DateTime 2011 Dec 29 1440 WallClock )
            ]
            (ZoneState 780 (Rules rules_WS))


{-| `Pacific/Auckland`
-}
pacific__auckland : () -> Time.Zone
pacific__auckland _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Rules rules_NZ))


{-| `Pacific/Bougainville`
-}
pacific__bougainville : () -> Time.Zone
pacific__bougainville _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 2014 Dec 28 120 WallClock )
            ]
            (ZoneState 660 (Save 0))


{-| `Pacific/Chatham`
-}
pacific__chatham : () -> Time.Zone
pacific__chatham _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 765 (Rules rules_Chatham))


{-| `Pacific/Chuuk`
-}
pacific__chuuk : () -> Time.Zone
pacific__chuuk _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Pacific/Easter`
-}
pacific__easter : () -> Time.Zone
pacific__easter _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -420 (Rules rules_Chile), DateTime 1982 Mar 14 180 Universal )
            ]
            (ZoneState -360 (Rules rules_Chile))


{-| `Pacific/Efate`
-}
pacific__efate : () -> Time.Zone
pacific__efate _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 660 (Rules rules_Vanuatu))


{-| `Pacific/Enderbury`
-}
pacific__enderbury : () -> Time.Zone
pacific__enderbury _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Pacific/Fakaofo`
-}
pacific__fakaofo : () -> Time.Zone
pacific__fakaofo _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -660 (Save 0), DateTime 2011 Dec 30 0 WallClock )
            ]
            (ZoneState 780 (Save 0))


{-| `Pacific/Fiji`
-}
pacific__fiji : () -> Time.Zone
pacific__fiji _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Rules rules_Fiji))


{-| `Pacific/Funafuti`
-}
pacific__funafuti : () -> Time.Zone
pacific__funafuti _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Galapagos`
-}
pacific__galapagos : () -> Time.Zone
pacific__galapagos _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Ecuador))


{-| `Pacific/Gambier`
-}
pacific__gambier : () -> Time.Zone
pacific__gambier _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -540 (Save 0))


{-| `Pacific/Guadalcanal`
-}
pacific__guadalcanal : () -> Time.Zone
pacific__guadalcanal _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 660 (Save 0))


{-| `Pacific/Guam`
-}
pacific__guam : () -> Time.Zone
pacific__guam _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Rules rules_Guam), DateTime 2000 Dec 23 0 WallClock )
            ]
            (ZoneState 600 (Save 0))


{-| `Pacific/Honolulu`
-}
pacific__honolulu : () -> Time.Zone
pacific__honolulu _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Pacific/Johnston`
-}
pacific__johnston : () -> Time.Zone
pacific__johnston _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Pacific/Kanton`
-}
pacific__kanton : () -> Time.Zone
pacific__kanton _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -720 (Save 0), DateTime 1979 Oct 1 0 WallClock )
            , ( ZoneState -660 (Save 0), DateTime 1994 Dec 31 0 WallClock )
            ]
            (ZoneState 780 (Save 0))


{-| `Pacific/Kiritimati`
-}
pacific__kiritimati : () -> Time.Zone
pacific__kiritimati _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -640 (Save 0), DateTime 1979 Oct 1 0 WallClock )
            , ( ZoneState -600 (Save 0), DateTime 1994 Dec 31 0 WallClock )
            ]
            (ZoneState 840 (Save 0))


{-| `Pacific/Kosrae`
-}
pacific__kosrae : () -> Time.Zone
pacific__kosrae _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 720 (Save 0), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState 660 (Save 0))


{-| `Pacific/Kwajalein`
-}
pacific__kwajalein : () -> Time.Zone
pacific__kwajalein _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -720 (Save 0), DateTime 1993 Aug 20 1440 WallClock )
            ]
            (ZoneState 720 (Save 0))


{-| `Pacific/Majuro`
-}
pacific__majuro : () -> Time.Zone
pacific__majuro _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Marquesas`
-}
pacific__marquesas : () -> Time.Zone
pacific__marquesas _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -570 (Save 0))


{-| `Pacific/Midway`
-}
pacific__midway : () -> Time.Zone
pacific__midway _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -660 (Save 0))


{-| `Pacific/Nauru`
-}
pacific__nauru : () -> Time.Zone
pacific__nauru _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 690 (Save 0), DateTime 1979 Feb 10 120 WallClock )
            ]
            (ZoneState 720 (Save 0))


{-| `Pacific/Niue`
-}
pacific__niue : () -> Time.Zone
pacific__niue _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -660 (Save 0))


{-| `Pacific/Norfolk`
-}
pacific__norfolk : () -> Time.Zone
pacific__norfolk _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 690 (Save 0), DateTime 1974 Oct 27 120 Standard )
            , ( ZoneState 690 (Save 60), DateTime 1975 Mar 2 120 Standard )
            , ( ZoneState 690 (Save 0), DateTime 2015 Oct 4 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2019 Jul 1 0 WallClock )
            ]
            (ZoneState 660 (Rules rules_AN))


{-| `Pacific/Noumea`
-}
pacific__noumea : () -> Time.Zone
pacific__noumea _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 660 (Rules rules_NC))


{-| `Pacific/Pago_Pago`
-}
pacific__pago_pago : () -> Time.Zone
pacific__pago_pago _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -660 (Save 0))


{-| `Pacific/Palau`
-}
pacific__palau : () -> Time.Zone
pacific__palau _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Pacific/Pitcairn`
-}
pacific__pitcairn : () -> Time.Zone
pacific__pitcairn _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -510 (Save 0), DateTime 1998 Apr 27 0 WallClock )
            ]
            (ZoneState -480 (Save 0))


{-| `Pacific/Pohnpei`
-}
pacific__pohnpei : () -> Time.Zone
pacific__pohnpei _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 660 (Save 0))


{-| `Pacific/Port_Moresby`
-}
pacific__port_moresby : () -> Time.Zone
pacific__port_moresby _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Pacific/Rarotonga`
-}
pacific__rarotonga : () -> Time.Zone
pacific__rarotonga _ =
    fromSpecification <|
        Zone
            [ ( ZoneState -630 (Save 0), DateTime 1978 Nov 12 0 WallClock )
            ]
            (ZoneState -600 (Rules rules_Cook))


{-| `Pacific/Saipan`
-}
pacific__saipan : () -> Time.Zone
pacific__saipan _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 2000 Dec 23 0 WallClock )
            ]
            (ZoneState 600 (Save 0))


{-| `Pacific/Tahiti`
-}
pacific__tahiti : () -> Time.Zone
pacific__tahiti _ =
    fromSpecification <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Pacific/Tarawa`
-}
pacific__tarawa : () -> Time.Zone
pacific__tarawa _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Tongatapu`
-}
pacific__tongatapu : () -> Time.Zone
pacific__tongatapu _ =
    fromSpecification <|
        Zone
            [ ( ZoneState 780 (Save 0), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState 780 (Rules rules_Tonga))


{-| `Pacific/Wake`
-}
pacific__wake : () -> Time.Zone
pacific__wake _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Wallis`
-}
pacific__wallis : () -> Time.Zone
pacific__wallis _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `WET`
-}
wet : () -> Time.Zone
wet _ =
    fromSpecification <|
        Zone
            []
            (ZoneState 0 (Save 0))



-- Links


{-| `Africa/Asmera` (alias of `Africa/Nairobi`)
-}
africa__asmera : () -> Time.Zone
africa__asmera =
    africa__nairobi


{-| `America/Atka` (alias of `America/Adak`)
-}
america__atka : () -> Time.Zone
america__atka =
    america__adak


{-| `America/Buenos_Aires` (alias of `America/Argentina/Buenos_Aires`)
-}
america__buenos_aires : () -> Time.Zone
america__buenos_aires =
    america__argentina__buenos_aires


{-| `America/Catamarca` (alias of `America/Argentina/Catamarca`)
-}
america__catamarca : () -> Time.Zone
america__catamarca =
    america__argentina__catamarca


{-| `America/Cordoba` (alias of `America/Argentina/Cordoba`)
-}
america__cordoba : () -> Time.Zone
america__cordoba =
    america__argentina__cordoba


{-| `America/Fort_Wayne` (alias of `America/Indiana/Indianapolis`)
-}
america__fort_wayne : () -> Time.Zone
america__fort_wayne =
    america__indiana__indianapolis


{-| `America/Godthab` (alias of `America/Nuuk`)
-}
america__godthab : () -> Time.Zone
america__godthab =
    america__nuuk


{-| `America/Indianapolis` (alias of `America/Indiana/Indianapolis`)
-}
america__indianapolis : () -> Time.Zone
america__indianapolis =
    america__indiana__indianapolis


{-| `America/Jujuy` (alias of `America/Argentina/Jujuy`)
-}
america__jujuy : () -> Time.Zone
america__jujuy =
    america__argentina__jujuy


{-| `America/Knox_IN` (alias of `America/Indiana/Knox`)
-}
america__knox_in : () -> Time.Zone
america__knox_in =
    america__indiana__knox


{-| `America/Kralendijk` (alias of `America/Puerto_Rico`)
-}
america__kralendijk : () -> Time.Zone
america__kralendijk =
    america__puerto_rico


{-| `America/Louisville` (alias of `America/Kentucky/Louisville`)
-}
america__louisville : () -> Time.Zone
america__louisville =
    america__kentucky__louisville


{-| `America/Lower_Princes` (alias of `America/Puerto_Rico`)
-}
america__lower_princes : () -> Time.Zone
america__lower_princes =
    america__puerto_rico


{-| `America/Marigot` (alias of `America/Puerto_Rico`)
-}
america__marigot : () -> Time.Zone
america__marigot =
    america__puerto_rico


{-| `America/Mendoza` (alias of `America/Argentina/Mendoza`)
-}
america__mendoza : () -> Time.Zone
america__mendoza =
    america__argentina__mendoza


{-| `America/Porto_Acre` (alias of `America/Rio_Branco`)
-}
america__porto_acre : () -> Time.Zone
america__porto_acre =
    america__rio_branco


{-| `America/Santa_Isabel` (alias of `America/Tijuana`)
-}
america__santa_isabel : () -> Time.Zone
america__santa_isabel =
    america__tijuana


{-| `America/Shiprock` (alias of `America/Denver`)
-}
america__shiprock : () -> Time.Zone
america__shiprock =
    america__denver


{-| `America/St_Barthelemy` (alias of `America/Puerto_Rico`)
-}
america__st_barthelemy : () -> Time.Zone
america__st_barthelemy =
    america__puerto_rico


{-| `America/Virgin` (alias of `America/Puerto_Rico`)
-}
america__virgin : () -> Time.Zone
america__virgin =
    america__puerto_rico


{-| `Antarctica/South_Pole` (alias of `Pacific/Auckland`)
-}
antarctica__south_pole : () -> Time.Zone
antarctica__south_pole =
    pacific__auckland


{-| `Arctic/Longyearbyen` (alias of `Europe/Berlin`)
-}
arctic__longyearbyen : () -> Time.Zone
arctic__longyearbyen =
    europe__berlin


{-| `Asia/Ashkhabad` (alias of `Asia/Ashgabat`)
-}
asia__ashkhabad : () -> Time.Zone
asia__ashkhabad =
    asia__ashgabat


{-| `Asia/Calcutta` (alias of `Asia/Kolkata`)
-}
asia__calcutta : () -> Time.Zone
asia__calcutta =
    asia__kolkata


{-| `Asia/Choibalsan` (alias of `Asia/Ulaanbaatar`)
-}
asia__choibalsan : () -> Time.Zone
asia__choibalsan =
    asia__ulaanbaatar


{-| `Asia/Chungking` (alias of `Asia/Shanghai`)
-}
asia__chungking : () -> Time.Zone
asia__chungking =
    asia__shanghai


{-| `Asia/Dacca` (alias of `Asia/Dhaka`)
-}
asia__dacca : () -> Time.Zone
asia__dacca =
    asia__dhaka


{-| `Asia/Istanbul` (alias of `Europe/Istanbul`)
-}
asia__istanbul : () -> Time.Zone
asia__istanbul =
    europe__istanbul


{-| `Asia/Katmandu` (alias of `Asia/Kathmandu`)
-}
asia__katmandu : () -> Time.Zone
asia__katmandu =
    asia__kathmandu


{-| `Asia/Macao` (alias of `Asia/Macau`)
-}
asia__macao : () -> Time.Zone
asia__macao =
    asia__macau


{-| `Asia/Rangoon` (alias of `Asia/Yangon`)
-}
asia__rangoon : () -> Time.Zone
asia__rangoon =
    asia__yangon


{-| `Asia/Saigon` (alias of `Asia/Ho_Chi_Minh`)
-}
asia__saigon : () -> Time.Zone
asia__saigon =
    asia__ho_chi_minh


{-| `Asia/Thimbu` (alias of `Asia/Thimphu`)
-}
asia__thimbu : () -> Time.Zone
asia__thimbu =
    asia__thimphu


{-| `Asia/Ujung_Pandang` (alias of `Asia/Makassar`)
-}
asia__ujung_pandang : () -> Time.Zone
asia__ujung_pandang =
    asia__makassar


{-| `Asia/Ulan_Bator` (alias of `Asia/Ulaanbaatar`)
-}
asia__ulan_bator : () -> Time.Zone
asia__ulan_bator =
    asia__ulaanbaatar


{-| `Atlantic/Faeroe` (alias of `Atlantic/Faroe`)
-}
atlantic__faeroe : () -> Time.Zone
atlantic__faeroe =
    atlantic__faroe


{-| `Australia/ACT` (alias of `Australia/Sydney`)
-}
australia__act : () -> Time.Zone
australia__act =
    australia__sydney


{-| `Australia/Canberra` (alias of `Australia/Sydney`)
-}
australia__canberra : () -> Time.Zone
australia__canberra =
    australia__sydney


{-| `Australia/LHI` (alias of `Australia/Lord_Howe`)
-}
australia__lhi : () -> Time.Zone
australia__lhi =
    australia__lord_howe


{-| `Australia/NSW` (alias of `Australia/Sydney`)
-}
australia__nsw : () -> Time.Zone
australia__nsw =
    australia__sydney


{-| `Australia/North` (alias of `Australia/Darwin`)
-}
australia__north : () -> Time.Zone
australia__north =
    australia__darwin


{-| `Australia/Queensland` (alias of `Australia/Brisbane`)
-}
australia__queensland : () -> Time.Zone
australia__queensland =
    australia__brisbane


{-| `Australia/South` (alias of `Australia/Adelaide`)
-}
australia__south : () -> Time.Zone
australia__south =
    australia__adelaide


{-| `Australia/Tasmania` (alias of `Australia/Hobart`)
-}
australia__tasmania : () -> Time.Zone
australia__tasmania =
    australia__hobart


{-| `Australia/Victoria` (alias of `Australia/Melbourne`)
-}
australia__victoria : () -> Time.Zone
australia__victoria =
    australia__melbourne


{-| `Australia/West` (alias of `Australia/Perth`)
-}
australia__west : () -> Time.Zone
australia__west =
    australia__perth


{-| `Australia/Yancowinna` (alias of `Australia/Broken_Hill`)
-}
australia__yancowinna : () -> Time.Zone
australia__yancowinna =
    australia__broken_hill


{-| `Brazil/Acre` (alias of `America/Rio_Branco`)
-}
brazil__acre : () -> Time.Zone
brazil__acre =
    america__rio_branco


{-| `Brazil/DeNoronha` (alias of `America/Noronha`)
-}
brazil__denoronha : () -> Time.Zone
brazil__denoronha =
    america__noronha


{-| `Brazil/East` (alias of `America/Sao_Paulo`)
-}
brazil__east : () -> Time.Zone
brazil__east =
    america__sao_paulo


{-| `Brazil/West` (alias of `America/Manaus`)
-}
brazil__west : () -> Time.Zone
brazil__west =
    america__manaus


{-| `Canada/Atlantic` (alias of `America/Halifax`)
-}
canada__atlantic : () -> Time.Zone
canada__atlantic =
    america__halifax


{-| `Canada/Central` (alias of `America/Winnipeg`)
-}
canada__central : () -> Time.Zone
canada__central =
    america__winnipeg


{-| `Canada/Eastern` (alias of `America/Toronto`)
-}
canada__eastern : () -> Time.Zone
canada__eastern =
    america__toronto


{-| `Canada/Mountain` (alias of `America/Edmonton`)
-}
canada__mountain : () -> Time.Zone
canada__mountain =
    america__edmonton


{-| `Canada/Newfoundland` (alias of `America/St_Johns`)
-}
canada__newfoundland : () -> Time.Zone
canada__newfoundland =
    america__st_johns


{-| `Canada/Pacific` (alias of `America/Vancouver`)
-}
canada__pacific : () -> Time.Zone
canada__pacific =
    america__vancouver


{-| `Canada/Saskatchewan` (alias of `America/Regina`)
-}
canada__saskatchewan : () -> Time.Zone
canada__saskatchewan =
    america__regina


{-| `Canada/Yukon` (alias of `America/Whitehorse`)
-}
canada__yukon : () -> Time.Zone
canada__yukon =
    america__whitehorse


{-| `Chile/Continental` (alias of `America/Santiago`)
-}
chile__continental : () -> Time.Zone
chile__continental =
    america__santiago


{-| `Chile/EasterIsland` (alias of `Pacific/Easter`)
-}
chile__easterisland : () -> Time.Zone
chile__easterisland =
    pacific__easter


{-| `Cuba` (alias of `America/Havana`)
-}
cuba : () -> Time.Zone
cuba =
    america__havana


{-| `Egypt` (alias of `Africa/Cairo`)
-}
egypt : () -> Time.Zone
egypt =
    africa__cairo


{-| `Eire` (alias of `Europe/Dublin`)
-}
eire : () -> Time.Zone
eire =
    europe__dublin


{-| `Etc/GMT+0` (alias of `Etc/GMT`)
-}
etc__gmt_plus0 : () -> Time.Zone
etc__gmt_plus0 =
    etc__gmt


{-| `Etc/GMT-0` (alias of `Etc/GMT`)
-}
etc__gmt_0 : () -> Time.Zone
etc__gmt_0 =
    etc__gmt


{-| `Etc/GMT0` (alias of `Etc/GMT`)
-}
etc__gmt0 : () -> Time.Zone
etc__gmt0 =
    etc__gmt


{-| `Etc/Greenwich` (alias of `Etc/GMT`)
-}
etc__greenwich : () -> Time.Zone
etc__greenwich =
    etc__gmt


{-| `Etc/UCT` (alias of `Etc/UTC`)
-}
etc__uct : () -> Time.Zone
etc__uct =
    etc__utc


{-| `Etc/Universal` (alias of `Etc/UTC`)
-}
etc__universal : () -> Time.Zone
etc__universal =
    etc__utc


{-| `Etc/Zulu` (alias of `Etc/UTC`)
-}
etc__zulu : () -> Time.Zone
etc__zulu =
    etc__utc


{-| `Europe/Bratislava` (alias of `Europe/Prague`)
-}
europe__bratislava : () -> Time.Zone
europe__bratislava =
    europe__prague


{-| `Europe/Busingen` (alias of `Europe/Zurich`)
-}
europe__busingen : () -> Time.Zone
europe__busingen =
    europe__zurich


{-| `Europe/Kiev` (alias of `Europe/Kyiv`)
-}
europe__kiev : () -> Time.Zone
europe__kiev =
    europe__kyiv


{-| `Europe/Mariehamn` (alias of `Europe/Helsinki`)
-}
europe__mariehamn : () -> Time.Zone
europe__mariehamn =
    europe__helsinki


{-| `Europe/Nicosia` (alias of `Asia/Nicosia`)
-}
europe__nicosia : () -> Time.Zone
europe__nicosia =
    asia__nicosia


{-| `Europe/Podgorica` (alias of `Europe/Belgrade`)
-}
europe__podgorica : () -> Time.Zone
europe__podgorica =
    europe__belgrade


{-| `Europe/San_Marino` (alias of `Europe/Rome`)
-}
europe__san_marino : () -> Time.Zone
europe__san_marino =
    europe__rome


{-| `Europe/Vatican` (alias of `Europe/Rome`)
-}
europe__vatican : () -> Time.Zone
europe__vatican =
    europe__rome


{-| `GB` (alias of `Europe/London`)
-}
gb : () -> Time.Zone
gb =
    europe__london


{-| `GB-Eire` (alias of `Europe/London`)
-}
gb_eire : () -> Time.Zone
gb_eire =
    europe__london


{-| `GMT` (alias of `Etc/GMT`)
-}
gmt : () -> Time.Zone
gmt =
    etc__gmt


{-| `GMT+0` (alias of `Etc/GMT`)
-}
gmt_plus0 : () -> Time.Zone
gmt_plus0 =
    etc__gmt


{-| `GMT-0` (alias of `Etc/GMT`)
-}
gmt_0 : () -> Time.Zone
gmt_0 =
    etc__gmt


{-| `GMT0` (alias of `Etc/GMT`)
-}
gmt0 : () -> Time.Zone
gmt0 =
    etc__gmt


{-| `Greenwich` (alias of `Etc/GMT`)
-}
greenwich : () -> Time.Zone
greenwich =
    etc__gmt


{-| `Hongkong` (alias of `Asia/Hong_Kong`)
-}
hongkong : () -> Time.Zone
hongkong =
    asia__hong_kong


{-| `Iceland` (alias of `Africa/Abidjan`)
-}
iceland : () -> Time.Zone
iceland =
    africa__abidjan


{-| `Iran` (alias of `Asia/Tehran`)
-}
iran : () -> Time.Zone
iran =
    asia__tehran


{-| `Israel` (alias of `Asia/Jerusalem`)
-}
israel : () -> Time.Zone
israel =
    asia__jerusalem


{-| `Jamaica` (alias of `America/Jamaica`)
-}
jamaica : () -> Time.Zone
jamaica =
    america__jamaica


{-| `Japan` (alias of `Asia/Tokyo`)
-}
japan : () -> Time.Zone
japan =
    asia__tokyo


{-| `Kwajalein` (alias of `Pacific/Kwajalein`)
-}
kwajalein : () -> Time.Zone
kwajalein =
    pacific__kwajalein


{-| `Libya` (alias of `Africa/Tripoli`)
-}
libya : () -> Time.Zone
libya =
    africa__tripoli


{-| `Mexico/BajaNorte` (alias of `America/Tijuana`)
-}
mexico__bajanorte : () -> Time.Zone
mexico__bajanorte =
    america__tijuana


{-| `Mexico/BajaSur` (alias of `America/Mazatlan`)
-}
mexico__bajasur : () -> Time.Zone
mexico__bajasur =
    america__mazatlan


{-| `Mexico/General` (alias of `America/Mexico_City`)
-}
mexico__general : () -> Time.Zone
mexico__general =
    america__mexico_city


{-| `NZ` (alias of `Pacific/Auckland`)
-}
nz : () -> Time.Zone
nz =
    pacific__auckland


{-| `NZ-CHAT` (alias of `Pacific/Chatham`)
-}
nz_chat : () -> Time.Zone
nz_chat =
    pacific__chatham


{-| `Navajo` (alias of `America/Denver`)
-}
navajo : () -> Time.Zone
navajo =
    america__denver


{-| `PRC` (alias of `Asia/Shanghai`)
-}
prc : () -> Time.Zone
prc =
    asia__shanghai


{-| `Pacific/Ponape` (alias of `Pacific/Guadalcanal`)
-}
pacific__ponape : () -> Time.Zone
pacific__ponape =
    pacific__guadalcanal


{-| `Pacific/Samoa` (alias of `Pacific/Pago_Pago`)
-}
pacific__samoa : () -> Time.Zone
pacific__samoa =
    pacific__pago_pago


{-| `Pacific/Truk` (alias of `Pacific/Port_Moresby`)
-}
pacific__truk : () -> Time.Zone
pacific__truk =
    pacific__port_moresby


{-| `Pacific/Yap` (alias of `Pacific/Port_Moresby`)
-}
pacific__yap : () -> Time.Zone
pacific__yap =
    pacific__port_moresby


{-| `Poland` (alias of `Europe/Warsaw`)
-}
poland : () -> Time.Zone
poland =
    europe__warsaw


{-| `Portugal` (alias of `Europe/Lisbon`)
-}
portugal : () -> Time.Zone
portugal =
    europe__lisbon


{-| `ROC` (alias of `Asia/Taipei`)
-}
roc : () -> Time.Zone
roc =
    asia__taipei


{-| `ROK` (alias of `Asia/Seoul`)
-}
rok : () -> Time.Zone
rok =
    asia__seoul


{-| `Singapore` (alias of `Asia/Singapore`)
-}
singapore : () -> Time.Zone
singapore =
    asia__singapore


{-| `Turkey` (alias of `Europe/Istanbul`)
-}
turkey : () -> Time.Zone
turkey =
    europe__istanbul


{-| `UCT` (alias of `Etc/UTC`)
-}
uct : () -> Time.Zone
uct =
    etc__utc


{-| `US/Alaska` (alias of `America/Anchorage`)
-}
us__alaska : () -> Time.Zone
us__alaska =
    america__anchorage


{-| `US/Aleutian` (alias of `America/Adak`)
-}
us__aleutian : () -> Time.Zone
us__aleutian =
    america__adak


{-| `US/Arizona` (alias of `America/Phoenix`)
-}
us__arizona : () -> Time.Zone
us__arizona =
    america__phoenix


{-| `US/Central` (alias of `America/Chicago`)
-}
us__central : () -> Time.Zone
us__central =
    america__chicago


{-| `US/East-Indiana` (alias of `America/Indiana/Indianapolis`)
-}
us__east_indiana : () -> Time.Zone
us__east_indiana =
    america__indiana__indianapolis


{-| `US/Eastern` (alias of `America/New_York`)
-}
us__eastern : () -> Time.Zone
us__eastern =
    america__new_york


{-| `US/Hawaii` (alias of `Pacific/Honolulu`)
-}
us__hawaii : () -> Time.Zone
us__hawaii =
    pacific__honolulu


{-| `US/Indiana-Starke` (alias of `America/Indiana/Knox`)
-}
us__indiana_starke : () -> Time.Zone
us__indiana_starke =
    america__indiana__knox


{-| `US/Michigan` (alias of `America/Detroit`)
-}
us__michigan : () -> Time.Zone
us__michigan =
    america__detroit


{-| `US/Mountain` (alias of `America/Denver`)
-}
us__mountain : () -> Time.Zone
us__mountain =
    america__denver


{-| `US/Pacific` (alias of `America/Los_Angeles`)
-}
us__pacific : () -> Time.Zone
us__pacific =
    america__los_angeles


{-| `US/Samoa` (alias of `Pacific/Pago_Pago`)
-}
us__samoa : () -> Time.Zone
us__samoa =
    pacific__pago_pago


{-| `UTC` (alias of `Etc/UTC`)
-}
utc : () -> Time.Zone
utc =
    etc__utc


{-| `Universal` (alias of `Etc/UTC`)
-}
universal : () -> Time.Zone
universal =
    etc__utc


{-| `W-SU` (alias of `Europe/Moscow`)
-}
w_su : () -> Time.Zone
w_su =
    europe__moscow


{-| `Zulu` (alias of `Etc/UTC`)
-}
zulu : () -> Time.Zone
zulu =
    etc__utc
