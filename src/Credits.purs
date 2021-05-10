module Credits where
  
type MovieCredits = {
  id:: Int,
  cast:: Array {
    name:: String,
    popularity:: Number
  }
}

cred :: MovieCredits
cred = {
  id: 0,
  cast : [
    {
      name: "",
      popularity: 0.0
    }
  ]
}