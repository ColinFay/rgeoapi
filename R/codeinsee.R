#'Get By INSEE Code
#'
#'Takes an INSEE Code, return a data.frame with name, postal code, INSEE code, surface, lat and long of the city.
#'@param an INSEE Code 
#'@return a data.frame with names, postal code, INSEE code, surface, lat and long. 
#'@export

getByIC <- function(codeInsee) {
  if(nchar(codeInsee) == 4) {
    codeInsee <- paste0("0", codeInsee)
  }
  url <- paste0("https://geo.api.gouv.fr/communes/", codeInsee, "?fields=nom,codeInsee,codesPostaux,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("Error Code Insee")
    } else {
        if(is.null(content$nom)) {
          city <- NA
        } else {
          city <- content$nom
        }
        if(is.null(content$codeInsee)) {
          codeInsee <- NA
        } else {
          codeInsee <- content$codeInsee
        }
        if(is.null(content$codesPostaux)) {
          codesPostaux <- NA
        } else {
          codesPostaux <- content$codesPostaux
        }
        if(is.null(content$surface)) {
          surface <- NA
        } else {
          surface <- content$surface
        }
        if(is.null(content$centre)) {
          lat <- NA
          long <- NA
        } else {
          coord <- content$centre$coordinates
          lat <- coord[2]
          long <- coord[1]
        }
        identity <- data.frame(name = city, codeInsee = codeInsee, codePostal = codesPostaux, surface = surface, lat=lat, long=long)
      return(identity)
    }
  } else {
    print("Error Code Insee or API Error")
  }
}
