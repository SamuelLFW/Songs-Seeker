check_singer = function(singer) {
  if("Taylor Swift" %in% singer) {
    return ("Taylor Swift")
  }
  return(sort(singer)[1] )
  
}