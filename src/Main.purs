module Main where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat (string)
import Data.Array (find, index, intersect, mapWithIndex, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (default, on)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Exception (throw, throwException)
import Genres (Genre, genres, initialGenre)
import Movie (Movie, mov)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (type (/\), Component, JSX, ReactComponent, component, element, reactComponent, useState)
import React.Basic.Hooks as R
import React.Basic.Hooks.Aff (useAff)
import Simple.JSON (readJSON)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLDocument (body, toNonElementParentNode)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.HTMLOptionElement (setSelected)
import Web.HTML.Window (document)
import Environment (getApiKey)

mov_nr_decisions :: Int
mov_nr_decisions = 20

type DiscoverMovieResponse = {
  page:: Int,
  results:: Array Movie 
}

baseUrl :: String 
baseUrl = "https://api.themoviedb.org/3/"

imgBaseUrl :: String
imgBaseUrl = "https://image.tmdb.org/t/p/" 
          
getMovie2 :: String -> Int -> Aff Movie
getMovie2 apiKey id = do
  res <- get string (baseUrl <> "movie/" <> show id <> "?api_key=" <> apiKey) 
  case res of
    Left err -> do
      --log $ "GET /api response failed to decode: " <> printError err
      pure mov 
    Right response -> do 
      case readJSON response.body of
        Left err -> do
          pure mov
        Right (r ::Movie) -> do
          --log $ "userID is: " <> show r.title
          pure r

discoverMovies :: String -> String -> Int -> Aff (Array Movie) 
discoverMovies apiKey genre page = do
  res <- get string (baseUrl 
  <> "discover/movie/" 
  <> "?api_key=" <> apiKey 
  <> "&with_genres=" <> genre 
  <> "&page=" <> show page
  <> "&networks=213"
  ) 
  case res of
    Left err -> do
      --log $ "GET /api response failed to decode: " <> printError err
      pure [mov] 
    Right response -> do 
      case readJSON response.body of
        Left err -> do
          pure [mov]
        Right (r :: DiscoverMovieResponse) -> do
          --log $ "userID is: " <> show r.title
          pure r.results
          
type SetState state
  = (state -> state) -> Effect Unit

type VotesTuple = Array Movie /\ Array Movie

type MkMoviesProps = {
  movies:: Maybe (Array Movie),
  votedYes:: VotesTuple, 
  setVotedYes:: SetState (VotesTuple),
  setShowVoteResults:: SetState Boolean 
} 

setTupleAt :: Int -> VotesTuple -> Array Movie -> VotesTuple
setTupleAt 0 t arr = Tuple arr (snd t)
setTupleAt 1 t arr = Tuple (fst t) arr 
setTupleAt _ t arr = t 

getArrAt :: Int -> VotesTuple -> Array Movie
getArrAt 0 t = fst t
getArrAt 1 t = snd t 
getArrAt _ t = fst t 

mkMovieSelection :: Component MkMoviesProps 
mkMovieSelection = do
  component "Movies" \props -> R.do
    Tuple step setStep <- useState 0 
    Tuple voter setVoter <- useState 0
    let 
      movies :: Array Movie  
      movies = fromMaybe [mov] props.movies

      movie :: Movie
      movie = fromMaybe mov (index (movies) step)
      
      newVotes :: Array Movie
      newVotes = movie : (getArrAt voter props.votedYes)
      
      handleYes :: Effect Unit
      handleYes = do
        handleMoveForward
        props.setVotedYes (\t -> setTupleAt voter t newVotes)

      handleNo :: Effect Unit
      handleNo = handleMoveForward

      handleMoveForward:: Effect Unit
      handleMoveForward = 
        if step == mov_nr_decisions -1 then 
          if voter == 0 then 
            do
              setStep \i -> 0
              setVoter \i -> 1
          else   
            props.setShowVoteResults \_ -> true
        else
          setStep \i -> i + 1


    pure $
      D.div {
      className: "mt-100",
      children: [ 
        D.h3 {
          className: "text-center",
          children: [
            D.text $ "Voter" <> show (voter + 1) <> ", please choose yes or no (" <> show step <> "/" <> show mov_nr_decisions <>")." 
          ]
        },
        D.div {
          children: [
            mkMovie movie,
            D.div {
              className: "row",
              children: [
                D.button {
                  onClick: handler_ handleYes,
                  className : "col-sm-6 btn-lg btn btn-success",
                  children: [ D.text "yay!" ]
                },
                D.button {
                  onClick: handler_ handleNo,
                  className : "col-sm-6 btn-lg p-10 btn btn-secondary",
                  children: [ D.text "nöö!" ]
                }
              ] 
            }
          ]
        }
      ]
    }
  
mkMovie :: Movie -> R.JSX
mkMovie movie =
  D.div {
    children: [
      D.div {
        className: "row",
          children: [
            D.div {
              className: "col-sm-6 row p-3",
              children: [
                D.img {
                  className: "mx-auto" ,
                  src: imgBaseUrl <> "w300" <> movie.poster_path  
                }
             ]
            },
            D.div {
              className: "col-sm-6 p-3",
              children: [
                D.h2 {
                  children: [
                    D.text movie.title
                  ]
                },
                D.p {
                  children: [
                    D.text movie.overview
                  ]
                },
                D.i {
                  children: [
                    D.text $ "Popularity: " <> show movie.popularity,
                    D.br {},
                    D.br {},
                    D.text $ "Release Date: " <> movie.release_date,
                    D.br {},
                    D.br {},
                    D.text $ "Vote Average: " <> show movie.vote_average
                  ]
                }
              ]
          }
        ]
      }
    ]
  }    

type MkMovieResultsProps = {
  votedYes:: VotesTuple   
}

mkMovieResults :: Component MkMovieResultsProps 
mkMovieResults = do
  component "MovieVoteResults" \props -> R.do
    Tuple selectedMovieIndex setSelectedMovieIndex <- useState Nothing 
    let
      moviesShared = intersect (getArrAt 0 props.votedYes) (getArrAt 1 props.votedYes)

      handleClickImg:: Int -> Effect Unit 
      handleClickImg i = do
        setSelectedMovieIndex \m -> Just (i)
      
      deselectMovie:: Effect Unit
      deselectMovie = do
        setSelectedMovieIndex \m -> Nothing

      selectedMovie :: Movie
      selectedMovie = fromMaybe mov (index (moviesShared) $ fromMaybe 0 selectedMovieIndex)

    pure $
      D.div {
        children: [
          D.h2 {
            children: [ D.text "Your Matches" ]
          },
          case selectedMovieIndex of 
          Just(i) -> D.div {
            children: [
              mkMovie selectedMovie,
              D.button {
                onClick: handler_ deselectMovie,
                className: "btn btn-secondary",
                children: [ 
                  D.text "Back To Results"
                ] 
              }
            ]
          }
          Nothing -> D.div {
            className: "row",
            children: mapWithIndex (\ind movie -> D.div {
              className : "col-sm-3 p-5",
              children: [
                D.img {
                  src: imgBaseUrl <> "w185" <> movie.poster_path,
                  onClick: handler_ (handleClickImg ind)
                }
              ]
            }) moviesShared
          }
        ] 
      }

mkMyApp :: Component String 
mkMyApp = do
  -- incoming \props are unused
  movieSelectionComponent <- mkMovieSelection
  movieResultsComponent <- mkMovieResults
  component "MainApp" \apiKey -> R.do
    Tuple id setId <- useState 0 
    -- result <- useAff id $ do
    --   getMovie2 id
    Tuple genre setGenre <- useState initialGenre 
    Tuple votedYes setVotedYes <- useState (Tuple [] [])
    Tuple showVoteResults setShowVoteResults <- useState false
    movies <- useAff genre.id $ do
      discoverMovies apiKey (show genre.id) 1
    let 
      handleClick :: Effect Unit
      handleClick = do
        logShow id
        setId \curr -> curr + 1
      changeGenre :: Maybe String -> Effect Unit 
      changeGenre maybeGenre = do 
        let
          newGenreName = fromMaybe initialGenre.name maybeGenre 
          foundGenre = find (\g -> g.name == newGenreName) genres 
          newGenre =  fromMaybe initialGenre foundGenre 
        setGenre (\g -> newGenre)

    pure $
      D.div {
        className: "container",
        children: [
          D.h1 {
            children : [
              D.text "Movie Tinder" 
            ]
          },
          D.select { 
            className: "form-select form-select-lg",
            children: map mkGenre genres,
            onChange: handler targetValue changeGenre 
          },
          if showVoteResults then
            movieResultsComponent { votedYes: votedYes }
          else
            movieSelectionComponent 
            { movies: movies
            , votedYes:votedYes
            , setVotedYes: setVotedYes 
            , setShowVoteResults: setShowVoteResults
            }
        ]
      }
    
mkGenre :: Genre -> R.JSX 
mkGenre genre = 
  D.option {
    children : [ 
      D.text genre.name 
    ]
  }

main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  apiKey <- getApiKey
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- body doc 
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkMyApp 
      -- let
        -- Create JSX node from react component. Pass-in empty props
        -- app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      -- D.render app c
      D.render (addressBookApp apiKey ) (toElement c)
