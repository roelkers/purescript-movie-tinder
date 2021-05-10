
exports.getApiKey = () => {
  return process.env.MOVIE_DB_API_KEY  
}

exports.getRandomInt = (a) => (b) => () => {
  return Math.floor(a + Math.random() * b)
}