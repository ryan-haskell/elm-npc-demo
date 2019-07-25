module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import Random as R exposing (Generator)


type alias Model =
    { seed : String
    }


type Msg
    = UpdateSeed String


type alias World =
    { characters : List Character
    }


type alias Id =
    Int


type alias Character =
    { id : Id
    , name : String
    , race : Race
    , disposition : Disposition
    , archetypes : ( Archetype, Archetype )
    }


type Race
    = Human


type Trait
    = HasDisposition Disposition
    | HasArchetype Archetype
    | HasRelation Relation


type alias Disposition =
    { anger : Int
    , disgust : Int
    , fear : Int
    , happiness : Int
    , sadness : Int
    , surprise : Int
    }


type Relation
    = FollowerOf Id
    | SiblingOf Id
    | LoverOf Id
    | FriendOf Id



-- based on "Jungian Archetypes"


type Archetype
    = Ruler -- control
    | Creator -- innovation
    | Innocent -- safety
    | Sage -- knowledge
    | Explorer -- freedom
    | Outlaw -- liberation
    | Hero -- mastery
    | Wizard -- power
    | Jester -- pleasure
    | Everyman -- belonging
    | Lover -- intimacy
    | Caregiver -- service



-- based on "Paul Ekman's emotion wheel"


type Emotion
    = Anger
    | Disgust
    | Fear
    | Happiness
    | Sadness
    | Surprise



-- GENERATORS


worldGenerator : Generator World
worldGenerator =
    R.int 2 5
        |> R.andThen
            (\characters ->
                R.map World
                    (R.list
                        characters
                        characterGenerator
                    )
            )


characterGenerator : Generator Character
characterGenerator =
    let
        characterFrom race =
            R.map5 Character
                (R.int 0 R.maxInt)
                (nameGenerator race)
                (R.constant race)
                dispositionGenerator
                archetypesGenerator
    in
    raceGenerator
        |> R.andThen characterFrom


raceGenerator : Generator Race
raceGenerator =
    R.uniform Human []


apply :
    Generator a
    -> Generator (a -> b)
    -> Generator b
apply =
    R.map2 (|>)


dispositionGenerator : Generator Disposition
dispositionGenerator =
    R.constant Disposition
        |> apply (R.int 1 50)
        |> apply (R.int 1 50)
        |> apply (R.int 1 50)
        |> apply (R.int 1 50)
        |> apply (R.int 1 50)
        |> apply (R.int 1 50)


emotionGenerator : Disposition -> Generator Emotion
emotionGenerator disposition =
    R.uniform Anger
        [ Disgust
        , Fear
        , Happiness
        , Sadness
        , Surprise
        ]


archetypesGenerator : Generator ( Archetype, Archetype )
archetypesGenerator =
    R.map2 Tuple.pair
        archetypeGenerator
        archetypeGenerator


archetypeGenerator : Generator Archetype
archetypeGenerator =
    R.uniform
        Ruler
        [ Creator
        , Innocent
        , Sage
        , Explorer
        , Outlaw
        , Hero
        , Wizard
        , Jester
        , Everyman
        , Lover
        , Caregiver
        ]


nameGenerator : Race -> Generator String
nameGenerator race =
    case race of
        Human ->
            humanNameGenerator


humanNameGenerator : Generator String
humanNameGenerator =
    R.uniform
        "Ryan"
        [ "Scott"
        , "Nick"
        , "Harry"
        , "Dhruv"
        , "Will"
        , "Alex"
        , "Erik"
        , "Jimmy"
        , "Karthik"
        , "Rene"
        , "Keegan"
        , "Sean"
        , "David"
        , "Jackie"
        , "Jeff"
        , "Alexa"
        , "Jessica"
        , "Anne"
        , "Joy"
        , "Cammie"
        , "Kendall"
        , "Heather"
        , "Sarah"
        , "Olivia"
        , "Hannah"
        , "Justine"
        , "Toni"
        , "Emma"
        , "Courtney"
        , "Katherine"
        , "Haskell"
        , "Blessing"
        , "Kirk"
        , "Miles"
        , "Kelch"
        , "Vajpeyi"
        , "Norris"
        , "Hawley"
        , "Morasek"
        , "Carron"
        , "Delanois"
        , "Tsao"
        , "Singh"
        , "Hernandez"
        , "Bowers"
        , "Whaley"
        , "Guan"
        , "Lu"
        , "Tuskey"
        , "DeJong"
        , "Gershak"
        , "Unterfenger"
        , "Cunningham"
        , "Ufer"
        ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


init : Model
init =
    Model "elm"



-- UPDATE


update msg model =
    case msg of
        UpdateSeed seed ->
            { model | seed = seed }



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( world, _ ) =
            String.toList model.seed
                |> List.map Char.toCode
                |> List.sum
                |> R.initialSeed
                |> R.step worldGenerator
    in
    div [ style "font-family" "Avenir, monospace" ]
        [ div
            [ style "margin" "2rem"
            ]
            [ div []
                [ h1 [] [ text "NPC Generator" ]
                , div [ style "margin-top" "-1em" ]
                    [ text "Random Seed"
                    , div [ style "margin-top" "0.5em" ]
                        [ input
                            [ Events.onInput UpdateSeed
                            , value model.seed
                            , style "font-family" "inherit"
                            , style "font-size" "inherit"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , div [] [ viewWorld world ]
        ]


viewWorld : World -> Html msg
viewWorld world =
    div [ class "world" ]
        (List.map viewCharacter world.characters)


viewCharacter : Character -> Html msg
viewCharacter character =
    div
        [ class "character"
        , style "margin" "3rem 2rem"
        ]
        [ div [ style "margin-right" "1rem" ]
            [ h2 [ style "margin" "0" ]
                [ text character.name
                , text " the "
                , text (Tuple.first character.archetypes |> toString)
                ]
            , div []
                [ strong [] [ text "Archetype: " ]
                , character.archetypes
                    |> (\( first, second ) ->
                            if first /= second then
                                [ first, second ]
                                    |> List.map seeks
                                    |> String.join " and "

                            else
                                "True " ++ seeks first
                       )
                    |> text
                ]
            ]
        , viewDispositions character
        ]


toString : Archetype -> String
toString archetype =
    case archetype of
        Ruler ->
            "Ruler"

        Creator ->
            "Creator"

        Innocent ->
            "Innocent"

        Sage ->
            "Sage"

        Explorer ->
            "Explorer"

        Outlaw ->
            "Outlaw"

        Hero ->
            "Hero"

        Wizard ->
            "Wizard"

        Jester ->
            "Jester"

        Everyman ->
            "Everyman"

        Lover ->
            "Lover"

        Caregiver ->
            "Caregiver"


seeks : Archetype -> String
seeks archetype =
    case archetype of
        Ruler ->
            "Control"

        Creator ->
            "Innovation"

        Innocent ->
            "Safety"

        Sage ->
            "Knowledge"

        Explorer ->
            "Freedom"

        Outlaw ->
            "Liberation"

        Hero ->
            "Mastery"

        Wizard ->
            "Power"

        Jester ->
            "Pleasure"

        Everyman ->
            "Belonging"

        Lover ->
            "Intimacy"

        Caregiver ->
            "Service"


viewDispositions : Character -> Html msg
viewDispositions character =
    let
        emotions =
            [ ( "Anger", .anger, "red" )
            , ( "Sadness", .sadness, "blue" )
            , ( "Happiness", .happiness, "yellow" )
            , ( "Fear", .fear, "purple" )
            , ( "Disgust", .disgust, "green" )
            , ( "Surprise", .surprise, "orange" )
            ]

        total =
            emotions
                |> List.map
                    ((\( a, b, c ) -> b)
                        >> (\fn -> fn character.disposition)
                    )
                |> List.sum
    in
    p []
        (emotions
            |> List.map
                (\( label, fn, color ) ->
                    let
                        percentage =
                            fn character.disposition
                                |> (\num ->
                                        toFloat num
                                            * 100
                                            / toFloat total
                                   )
                    in
                    div
                        [ style "display" "flex"
                        , style "align-items" "center"
                        ]
                        [ strong [ style "width" "6rem" ] [ text label ]
                        , span
                            [ style "display" "block"
                            , style "min-width" "26px"
                            , style "width" (String.fromFloat (percentage / 1.5) ++ "em")
                            , style "padding" "0.25em 0.5em"
                            , style "margin-right" "0.5em"
                            , style "background" color
                            , style "box-shadow" "0 0 4px 0 rgba(0,0,0,0.25)"
                            , style "color" <|
                                if color == "yellow" then
                                    "black"

                                else
                                    "white"
                            ]
                            [ text (String.fromInt (round percentage) ++ "%")
                            ]
                        ]
                )
        )
