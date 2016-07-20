#'Get City by Postal Code
#'
#'Get informations about a french city by its INSEE Code. Please note that this package works only with french cities.
#'
#'Takes a postal code, returns a data.frame with the available values.
#'@param codePostal a numeric vector with a postal code.
#'@return Returns a data.frame with name, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84). 
#'@export
#'@note If you don't know the postal code of the city you're looking for, you can find it by using the \code{\link{ComByName}} function.
#'@examples
#'ComByPostal(29000)

ComByPostal <- function(codePostal) {
  if(nchar(codePostal) == 4) {
    codePostal <- paste0("0", codePostal)
  }
  url <- paste0("https://geo.api.gouv.fr/communes?codePostal=", codePostal, "&fields=nom,codeInsee,codesPostaux,codeDepartement,codeRegion,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No Content for that Postal Code : your input may not be an actual postal code")
    } else {
      identity <- data.frame()
      for(i in 1:length(content)) {
        obj <- content[[i]]
        if(is.null(obj$nom)) {
          nom <- NA
        } else {
          nom <- obj$nom
        }
        if(is.null(obj$code)) {
          codeInsee <- NA
        } else {
          codeInsee <- obj$code
        }
        if(is.null(obj$codesPostaux)) {
          codesPostaux <- NA
        } else {
          codesPostaux <- obj$codesPostaux
        }
        if(is.null(obj$surface)) {
          surface <- NA
        } else {
          surface <- obj$surface
        }
        if(is.null(obj$centre)) {
          lat <- NA
          long <- NA
        } else {
          coord <- obj$centre$coordinates
          lat <- coord[2]
          long <- coord[1]
        }
        if(is.null(obj$codeDepartement)) {
          codeDepartement <- NA
        } else {
          codeDepartement <- obj$codeDepartement
        }
        if(is.null(obj$codeRegion)) {
          codeRegion <- NA
        } else {
          codeRegion <- obj$codeRegion
        }
        if(is.null(obj$population)) {
          population <- NA
        } else {
          population <- obj$population
        }
        objbis <- data.frame(name = nom, codeInsee = codeInsee, codesPostaux = codesPostaux, codeDepartement = codeDepartement, codeRegion = codeRegion, population = population, surface = surface, lat=lat, long=long, stringsAsFactors = FALSE)
        identity <- rbind(identity,objbis)
      }
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual postal code")
  }
}

