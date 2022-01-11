module Pages.Example exposing (Model, Msg, page)

import Date exposing (Date, Interval(..), Month, Unit(..))
import Element exposing (Element, alignLeft, alignRight, fill, fillPortion, padding, px, rgb, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Example exposing (Params)
import Page
import Request
import Shared
import Time exposing (Month(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { today : Date
    , selected : Maybe Date
    }


type alias Properties =
    { title : String
    , icon : String
    }


type alias Options msg =
    { select : msg
    , today : Date
    , current : Date
    , currentMonth : Date
    }


type alias DayItem =
    { date : Date
    , isCurrentMonth : Bool
    }


init : Model
init =
    { today = Date.fromCalendarDate 2022 Feb 11, selected = Just <| Date.fromCalendarDate 2022 Feb 10 }



-- UPDATE


type Msg
    = Previous
    | Next
    | Selected Date


update : Msg -> Model -> Model
update msg model =
    case msg of
        Previous ->
            let
                prev =
                    Date.add Months -1 model.today
            in
            { model | today = prev }

        Next ->
            let
                next =
                    Date.add Months 1 model.today
            in
            { model | today = next }

        Selected date ->
            { model | selected = Just date }



-- VIEW


view : Model -> View Msg
view model =
    { title = "Xamoke"
    , attributes = []
    , element =
        let
            numberOfDays =
                daysOfMonth (Date.month model.today) (Date.year model.today)

            fromPreviousMonth =
                dateDaysFromPreviousMonth model.today

            dates =
                fromPreviousMonth
                    ++ datesOfTheMonth model.today
                    ++ dateDaysFromNextMonth (List.length fromPreviousMonth + numberOfDays) model.today
        in
        Element.column []
            [ header model.today
            , drawDateMatrix dates model.selected model.today
            ]
    }


drawDateMatrix : List DayItem -> Maybe Date -> Date -> Element Msg
drawDateMatrix matrix selected today =
    let
        ( first, rest ) =
            split 7 matrix

        ( second, secondRest ) =
            split 7 rest

        ( third, thirdRest ) =
            split 7 secondRest

        ( fourth, fourthRest ) =
            split 7 thirdRest

        ( fifth, sixth ) =
            split 7 fourthRest
    in
    Element.column [ spacing 2, Border.width 1, Element.width (px 600) ]
        [ Element.row [ spacing 2, Element.centerX, Element.width fill ] (List.map drawCol weekHeader)
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate selected today) first
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate selected today) second
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate selected today) third
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate selected today) fourth
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate selected today) fifth
        , Element.row [ spacing 2, Element.centerX, Element.width fill ] <| List.map (drawDate selected today) sixth
        ]


drawDate : Maybe Date -> Date -> DayItem -> Element Msg
drawDate selected today day =
    let
        isSelected =
            case selected of
                Just date ->
                    date == day.date

                Nothing ->
                    False
    in
    Element.el
        [ Element.width (fillPortion 7)
        , Element.centerX
        , if isSelected then
            Background.color (Element.rgb255 20 100 240)

          else
            Background.color (Element.rgb255 255 255 255)
        , Element.mouseOver
            [ Background.color (Element.rgb255 130 130 130)
            ]
        , Element.Events.onClick (Selected day.date)
        , if day.date == today then
            Element.inFront (Element.el [ Element.centerX, Element.alignBottom ] (Element.text "."))

          else
            Element.inFront Element.none
        ]
    <|
        Element.column
            [ Element.height (px 32)
            , Element.centerX
            , Font.color <|
                if day.isCurrentMonth then
                    Element.rgb255 0 0 0

                else
                    Element.rgb255 169 169 169
            , Font.size 16
            ]
            [ Element.el
                [ Element.centerY
                ]
              <|
                Element.text <|
                    String.fromInt (Date.day day.date)
            ]


drawCol : String -> Element Msg
drawCol day =
    Element.el [ Element.width (fillPortion 7), Element.centerX, Element.centerY ] <|
        Element.el [ Element.centerX ]
            (Element.text <| day)


header : Date -> Element Msg
header date =
    let
        label =
            Date.format "MMMM" date ++ " " ++ (String.fromInt <| Date.year date)
    in
    Element.row [ Element.width fill ]
        [ Input.button
            [ alignLeft ]
            { onPress = Just Previous
            , label = Element.text "<"
            }
        , Element.el [ Element.centerX ] (Element.text label)
        , Input.button
            [ alignRight ]
            { onPress = Just Next
            , label = Element.text ">"
            }
        ]


basicPickerView : Properties -> Options msg -> Element msg
basicPickerView properties options =
    Element.none


weekHeader : List String
weekHeader =
    [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]


daysInTheView : Int
daysInTheView =
    42


weekDayOfFirstDay : Date -> Int
weekDayOfFirstDay date =
    let
        firstDay =
            Date.fromCalendarDate (Date.year date) (Date.month date) 1
    in
    Date.weekdayNumber firstDay


dateDaysFromPreviousMonth : Date -> List DayItem
dateDaysFromPreviousMonth date =
    let
        firstWeekDayMonth =
            weekDayOfFirstDay date
    in
    if firstWeekDayMonth == 1 then
        []

    else
        let
            year =
                Date.year date

            month =
                Date.month date

            first =
                Date.fromCalendarDate year month 1

            start =
                Date.add Days (-firstWeekDayMonth + 1) first

            until =
                Date.add Days -1 first

            dates =
                Date.range Day 1 start until ++ List.singleton until
        in
        List.map
            (\day ->
                { date = day
                , isCurrentMonth = False
                }
            )
            dates


dateDaysFromNextMonth : Int -> Date -> List DayItem
dateDaysFromNextMonth currentSize dayInCurrentMonth =
    if currentSize == daysInTheView then
        []

    else
        let
            nextMonth =
                Date.add Months 1 dayInCurrentMonth

            start =
                Date.fromCalendarDate (Date.year nextMonth) (Date.month nextMonth) 1

            until =
                Date.fromCalendarDate (Date.year nextMonth) (Date.month nextMonth) (daysInTheView - currentSize)

            dates =
                Date.range Day 1 start until ++ List.singleton until
        in
        List.map
            (\day ->
                { date = day
                , isCurrentMonth = False
                }
            )
            dates


datesOfTheMonth : Date -> List DayItem
datesOfTheMonth day =
    let
        year =
            Date.year day

        month =
            Date.month day

        start =
            Date.fromCalendarDate year month 1

        until =
            Date.fromCalendarDate year month <| daysOfMonth month year

        dates =
            Date.range Day 1 start until ++ [ Date.fromCalendarDate year month <| daysOfMonth month year ]
    in
    List.map
        (\date ->
            { date = date
            , isCurrentMonth = True
            }
        )
        dates


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysOfMonth : Date.Month -> Int -> Int
daysOfMonth month year =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


split : Int -> List a -> ( List a, List a )
split i xs =
    ( List.take i xs, List.drop i xs )
