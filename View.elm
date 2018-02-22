module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Version exposing (version, gitRepo)
import Update exposing (Msg(..))
import Model exposing (..)
import Time exposing (Time)
import Date exposing (..)
import Date.Format exposing (formatISO8601)
import Numeral exposing (format)
import String


-- VIEW


view : Model -> Html Msg
view model =
    let
        response =
            buildResponse model

        clist =
            List.map .uname model.tList
    in
        div []
            [ h2 [] [ text "CamperBot for KGISL Meetups" ]
            , footer
            , hr [] []

            --, p [] [ text "Here I want to grab the ''browniePoints''"]
            --, p [] [ text ("FCC URL: " ++ fccAPI) ]
            , input
                [ placeholder "Enter a FCC username"
                , onInput StoreID
                ]
                []
            , button [ onClick FetchData ] [ text "Fetch and Add!" ]
            , input
                [ placeholder model.gRoom.name
                , onInput StoreRoom
                ]
                []
            , button [ onClick FetchGitter ] [ text "Update from Gitter" ]
            , label []
                [ -- br [] []
                  input [ type_ "radio", checked model.min5, onCheck Set5min ] []
                , text "5 min"
                ]
            , label []
                [ input [ type_ "radio", checked model.min15, onCheck Set15min ] []
                , text "15 min"
                ]

            -- , updateSettings model
            , h1 [ rStyle ] [ text response ]

            --, div [] [ text (toString model.gRoom) ]
            --, div [] [ text (toString model.gList) ]
            , campList True model.ts model.tList
            ]


rStyle : Attribute msg
rStyle =
    style
        [ ( "backgroundColor", "#ff6600" )
        , ( "color", "white" )
        , ( "fontSize", "150%" )
        ]



{- formatData : Time.Time -> Cdata -> String -- (Int, String)
   formatData firstEntry cdata =
     let
       timeLapsed = Time.inHours (cdata.ts - firstEntry.ts)
     in
       case (cdata.ts, timeLapsed) of
         (0,_) -> toString cdata.points
         (_,0) -> toString cdata.points
         (_,_)-> (toString cdata.delta) ++
                 "(" ++ format "0.00" timeLapsed ++ ")"
-}


formatData : Maybe Cdata -> Cdata -> String
formatData prevEntry cdata =
    case (prevEntry) of
        Nothing ->
            toString cdata.points

        Just prevEntry ->
            let
                timeLapsed =
                    Time.inHours (cdata.ts - prevEntry.ts)
            in
                case ( cdata.ts, timeLapsed /= 0 && timeLapsed >= -720 ) of
                    ( 0, _ ) ->
                        toString cdata.points

                    ( _, False ) ->
                        toString cdata.points

                    ( _, True ) ->
                        (toString cdata.delta)
                            ++ "("
                            ++ format "+0.00" timeLapsed
                            ++ ")"


camperItem : Camper -> Html Msg
camperItem camper =
    let
        history =
            List.take 10 camper.chist

        prev =
            List.head history

        --points  = String.join ", " (List.map (formatData camper.last.ts) history )
        points =
            String.join ", " (List.map (formatData prev) history)
    in
        li []
            [ span [ class "uname" ] [ text camper.uname ]
            , --        span [ class "points" ] [text "   -   "],
              span [ class "points" ] [ text points ]
            ]


campList : Bool -> Time -> List Camper -> Html Msg
campList display now campers =
    let
        -- campers_ = List.sortWith flippedComparison2 campers
        campers_ =
            Update.sortBasedOnHistory now cutOff30Days campers

        items =
            List.map camperItem campers_
    in
        div []
            [ ul [] items
            ]



{--
updateSettings: Model -> Html Msg
updateSettings model =
  div []
    [ span [] [text "Hello, how are you?!"]
    , radio Red "red" model
    , radio Underline "underline" model
    , radio Bold "bold" model
    ]


radio : Style -> String -> Model -> Html Msg
radio style name model =
  let
    isSelected =
      model.style == style
  in
    label []
      [ br [] []
      , input [ type' "radio", checked isSelected, onCheck (\_ -> Switch style) ] []
      , text name
      ]

--}


footer : Html Msg
footer =
    div [ id "footer" ]
        [ a
            [ href (gitRepo ++ "/issues/new")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text version ]
        ]


buildResponse : Model -> String
buildResponse model =
    let
        now =
            fromTime model.ts

        shour =
            format "00" (toFloat (hour now))

        smin =
            format "00" (toFloat (minute now))

        dateString =
            formatISO8601 now
    in
        --if model.error == True
        --then "Error: userID not valid? " ++ model.message
        if model.tPoints /= -1 then
            "Challenges completed: "
                ++ (toString model.tPoints)
                ++ (difference model.tPoints model.tPoints_prev)
                ++ " by "
                ++ (toString (List.length model.tList))
                ++ " campers (excluded "
                ++ toString (List.length model.exclude)
                ++ "); last auto update @ "
                ++ dateString
        else
            ""


difference : Int -> Int -> String
difference current previous =
    case (current - previous) of
        0 ->
            ""

        _ ->
            "(" ++ toString (current - previous) ++ ")"


sortBasedOnHistory : Time -> Time -> List Camper -> List Camper
sortBasedOnHistory now cutOff campers =
    campers
        |> List.map (truncateHistory now cutOff)
        |> List.sortWith flippedComparison


truncateHistory : Time -> Time -> Camper -> Camper
truncateHistory now cutOff camper =
    { camper
        | chist =
            List.filterMap (isWithinCutOff now cutOff) camper.chist
    }


isWithinCutOff : Time -> Time -> Cdata -> Maybe Cdata
isWithinCutOff now cutOff data =
    case (data.ts >= (now - cutOff)) of
        True ->
            Just data

        False ->
            Nothing


{-| comparator for campers based on their
challenge completion activity, in the following order:

  - latest timestamp of last completed challenge

  - sum of all delta in history (or truncated history)

  - total challenges completed

    flippedComparison { uname = "kgashok"
    , chist = [{ points = 229, ts = 17000, delta = 3 }]
    , last = { points = 229, ts = 17000, delta = 3 }
    }
    { uname = "sudhar"
    , chist = [ { points = 124, ts = 16000, delta = 4 }
    , { points = 120, ts = 15000, delta = 10 }
    ]
    , last = { points = 124, ts = 16000, delta = 4 } }
    --> LT

-}
flippedComparison : Camper -> Camper -> Order
flippedComparison a b =
    let
        adelta =
            a.chist
                --|> List.take ((List.length a.chist) - 1)
                |> List.map .delta
                |> List.sum

        bdelta =
            b.chist
                --|> List.take ((List.length b.chist) - 1)
                |> List.map .delta
                |> List.sum
    in
        case
            compare ( b.last.ts, bdelta, b.last.points )
                ( a.last.ts, adelta, a.last.points )
        of
            GT ->
                GT

            EQ ->
                EQ

            _ ->
                LT


sortBasedOnHistory2 : Time -> Time -> List Camper -> List Camper
sortBasedOnHistory2 now cutOff campers =
    -- campers_ = List.sortWith flippedComparison2 campers
    campers
        |> List.map (truncateHistory now cutOff)
        --|> Debug.log "post cut"
        --|> List.sortWith flippedComparison3
        --|> Debug.log "post compare3"
        |> List.sortWith flippedComparison2
        --|> Debug.log "post compare2"
        |> List.sortWith flippedComparison



--|> Debug.log "post compare"


flippedComparison3 : Camper -> Camper -> Order
flippedComparison3 a b =
    case compare a.last.points b.last.points of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


flippedComparison2 : Camper -> Camper -> Order
flippedComparison2 a b =
    case compare a.last.ts b.last.ts of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT



{-
   ahist =
       List.map .points a.chist

   bhist =
       List.map .points b.chist

   deltaA =
       Maybe.withDefault 0 (List.maximum ahist)
           - Maybe.withDefault 0 (List.minimum ahist)

   deltaB =
       Maybe.withDefault 0 (List.maximum bhist)
           - Maybe.withDefault 0 (List.minimum bhist)

   _ = Debug.log "a " a
   _ = Debug.log "b " b
   _ = Debug.log "deltaA " deltaA
   _ = Debug.log "deltaB " deltaB
-}
