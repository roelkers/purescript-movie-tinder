module Networks where
  
type Network = {
  id:: Int,
  name:: String
}

networks :: Array Network
networks = [
  { 
    id: 213, name: "Netflix"
  }
]