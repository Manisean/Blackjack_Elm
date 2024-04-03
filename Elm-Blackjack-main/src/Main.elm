module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import String
import Array
import List
import Time
import Random
import Random.Array
import Task

type alias Model = { currentTime: Time.Posix, deck : List String.String, dealerHand : List String.String, playerHand : List String.String, playerScore : Int, dealerScore : Int, outcome : Outcome }

type Outcome = PlayerWins | DealerWins | Tie | Ongoing

type Msg = Deal Time.Posix | Hit | Stay | RefreshNow | Refresh Time.Posix

seed = 20

drawCard : List String -> List String -> (List String, List String)
drawCard hand deck =
  case List.reverse deck of
    card :: remainingDeck ->
      (card :: hand, List.take (List.length deck - 1) deck)

    [] ->
      (hand, deck)


calculateScore : List String -> Int
calculateScore hand =
  let
    (score, hasAce) =
      List.foldl
        (\card (s, ace) ->
          if card == "A" then
            (s, True)
          else
            case String.toInt card of
              Just cardValue ->
                (s + cardValue, ace)

              Nothing ->
                (s + 10, ace)
        )
        (0, False)
        hand
  in
  if hasAce && (score + 11) <= 21 then
    score + 11
  else
    score + if hasAce then 1 else 0


dealerDraw : List String.String -> List String.String -> (List String.String, List String.String)
dealerDraw cards deck=
  let
    sum = calculateScore cards
  in
    if sum >= 17 then
      (cards, deck)
    else
      let
        newCard = Tuple.first (drawCard cards deck)
        newDeck = Tuple.second (drawCard cards deck)
      in
        dealerDraw newCard newDeck


main : Program () Model Msg
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : () -> (Model, Cmd Msg)
init _ =
  (let
    initDeck = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
    ( shuffledList, _ ) =
      Random.step (Random.Array.shuffle (Array.fromList initDeck)) (Random.initialSeed (seed))

    dealerCards = Tuple.first (drawCard [] (Array.toList shuffledList))
    updatedDeck1 = Tuple.second (drawCard [] (Array.toList shuffledList))
    firstCard = Tuple.first (drawCard [] updatedDeck1)
    updatedDeck2 = Tuple.second (drawCard [] updatedDeck1)
    playerCards = Tuple.first (drawCard firstCard updatedDeck2)
    finalDeck = Tuple.second (drawCard [] updatedDeck2)
    pScore = calculateScore playerCards
    dScore = calculateScore dealerCards
    
  in
    { 
      currentTime = Time.millisToPosix 0,
      deck = finalDeck, 
      dealerHand = dealerCards, 
      playerHand = playerCards, 
      playerScore = pScore, 
      dealerScore = dScore, 
      outcome = Ongoing }, Cmd.none)
    
-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Deal time ->
       let
          currentMillis = Time.posixToMillis time
          initDeck = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
          ( shuffledList, _ ) =
              Random.step (Random.Array.shuffle (Array.fromList initDeck)) (Random.initialSeed (currentMillis))
          dealerCards = Tuple.first (drawCard [] (Array.toList shuffledList)) 
          updatedDeck1 = Tuple.second (drawCard [] (Array.toList shuffledList))
          firstCard = Tuple.first (drawCard [] updatedDeck1)  
          updatedDeck2 = Tuple.second (drawCard [] updatedDeck1) 
          playerCards = Tuple.first (drawCard firstCard updatedDeck2) 
          finalDeck = Tuple.second (drawCard [] updatedDeck2) 
          pScore = calculateScore playerCards    
          dScore = calculateScore dealerCards
      in
        ({ model | deck = finalDeck,
                 dealerHand = dealerCards,
                 playerHand = playerCards,
                 playerScore = pScore,
                 dealerScore = dScore,
                 outcome = Ongoing }, Cmd.none)
  
    Hit ->
       let
          ( newCard, newDeck ) =
              drawCard model.playerHand model.deck
          pScore =
              calculateScore newCard
          newPlayerHand =
              newCard
          newOutcome =
              if pScore > 21 then
                  DealerWins
              else
                  Ongoing
      in
        ({ model | 
          currentTime = model.currentTime,
          deck = newDeck,
          dealerHand = model.dealerHand,
          playerHand = newPlayerHand,
          playerScore = pScore,
          dealerScore = model.dealerScore,
          outcome = newOutcome }, Cmd.none)
    Stay ->
        let
          ( dealerCards, newDeck ) =
            dealerDraw model.dealerHand model.deck
          dScore =
            calculateScore dealerCards
          pScore =
            calculateScore model.playerHand
          newOutcome =
            if dScore > 21 || dScore < pScore then
              PlayerWins
            else if dScore > model.playerScore then
              DealerWins
            else if dScore == model.playerScore then
              Tie
            else
              Ongoing
        in
          ({ model | deck = newDeck,
            dealerHand = dealerCards,
            dealerScore = dScore,
            outcome = newOutcome }, Cmd.none)
    RefreshNow -> 
      (model, Task.perform Refresh Time.now)
    Refresh time ->
      let
        newModel =
            { currentTime = time,
              deck = model.deck,
              dealerHand = [],
              playerHand = [],
              playerScore = 0,
              dealerScore = 0,
              outcome = Ongoing }
      in
        update (Deal time) newModel


-- VIEW

cardView : Maybe String.String -> Html Msg
cardView card =
    div []
        [ div [] [ ]
        ]

handView : List String.String -> Html Msg
handView hand =
    div []
        [ div [] [ ]
        ]

outcomeView : Outcome -> Html Msg
outcomeView outcome =
    let
        message =
            case outcome of
                PlayerWins ->
                    "You win!"
                DealerWins ->
                    "Dealer wins."
                Tie ->
                    "It's a tie."
                Ongoing ->
                    ""
    in
    div [] [ text message ]

playerView : Model -> Html Msg
playerView state =
    div []
        [ handView state.playerHand
        , div [] [ text <| "Player Score:"]
        , h3 [] [ text <| String.fromInt state.playerScore ]
        , div [] [ text <| "Player Hand:"]
        , h3 [] [ text <| (String.join " " state.playerHand) ]
        ]

dealerView : Model -> Html Msg
dealerView state =
    div []
        [ handView state.dealerHand
        , div [] [ text <| "Dealer Score:"]
        , h3 [] [ text <| String.fromInt state.dealerScore ]
        , div [] [ text <| "Dealer Hand:"]
        , h3 [] [ text <| (String.join " " state.dealerHand) ]
        ]

view : Model -> Html Msg
view model =
  if model.outcome == Ongoing then
    div []
      [ dealerView model
      , playerView model
      , outcomeView model.outcome
      , div []
        [ button [ onClick Hit ] [ text "Hit" ]
        , button [ onClick Stay ] [ text "Stay" ]
        , button [ onClick RefreshNow ] [ text "Refresh" ]
        ]
      ]
  else
    div []
      [ dealerView model
        , playerView model
        , outcomeView model.outcome
        , div []
          [ button [ onClick RefreshNow ] [ text "Refresh" ]]
        ]