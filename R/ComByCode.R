#'Get City by INSEE Code
#'
#'Get informations about a french city by its INSEE code. Please note that this package works only with french cities.
#'
#'Takes an INSEE Code, returns a data.frame with the available values.
#'@param codeInsee numeric vector with an INSEE code.
#'@return Returns a data.frame with names, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84). 
#'@export 
#'@note If you don't know the INSEE code of the city you're looking for, you can find it by using the \code{\link{ComByName}} function.
#'@examples
#' ComByCode(codeInsee = 88160) 
#' ComByCode(codeInsee = 31555)
#' library(plyr)
#' ldply(c("88160", "60057"), ComByCode)
#' ComByCode(codeInsee = ComByName("Rennes")[1,"codeInsee"])

ComByCode <- function(codeInsee) {
  if(nchar(codeInsee) == 4) {
    codeInsee <- paste0("0", codeInsee)
  }
  url <- paste0("https://geo.api.gouv.fr/communes/", codeInsee, "?fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No Content for that INSEE code : your input may not be an actual INSEE code")
    } else {
        if(is.null(content$nom)) {
          nom <- NA
        } else {
          nom <- content$nom
        }
        if(is.null(content$code)) {
          codeInsee <- NA
        } else {
          codeInsee <- content$code
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
      if(is.null(content$codeDepartement)) {
        codeDepartement <- NA
      } else {
        codeDepartement <- content$codeDepartement
      }
      if(is.null(content$codeRegion)) {
        codeRegion <- NA
      } else {
        codeRegion <- content$codeRegion
      }
      if(is.null(content$population)) {
        population <- NA
      } else {
        population <- content$population
      }
        identity <- data.frame(nom = nom, codeInsee = codeInsee, codesPostaux = codesPostaux, codeDepartement = codeDepartement, codeRegion = codeRegion, population = population, surface = surface, lat=lat, long=long, stringsAsFactors = FALSE)
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual INSEE code")
  }
}


