module Movie where
  
type Movie = {
  id :: Int
  ,overview:: String
  ,popularity:: Number
  ,poster_path:: String
  ,title:: String
  ,release_date:: String
  ,vote_average:: Number
} 

mov :: Movie
mov = {
  id : 1
  ,overview : ""
  ,popularity: 0.0
  ,poster_path: ""
  ,title: ""
  ,release_date: ""
  ,vote_average: 0.0
}