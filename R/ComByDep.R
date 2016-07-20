#'Get Cities by Department 
#'
#'Get informations about all the cities in a french department by its INSEE Code. Please note that this package works only with french cities.
#'
#'Takes a department INSEE Code, returns a data.frame with the available values.
#'@param codeDepartement a numeric vector with a department INSEE Code. 
#'@return Returns a data.frame with names of the cities, INSEE codes, postal codes, INSEE department codes, INSEE region codes, population of the cities (approx), surface of the cities (in hectares), lat and long of the cities (WGS-84). 
#'@export
#'@note If you don't know the INSEE code of the department you're looking for, you can find it by using the \code{\link{DepByName}} function.
#'@examples
#'ComByDep(35) 

ComByDep <- function(codeDepartement) {
  if(nchar(codeDepartement) == 1) {
    codeDepartement <- paste0("0", codeDepartement)
  }
  url <- paste0("https://geo.api.gouv.fr/communes?codeDepartement=", codeDepartement, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No Content for that code : your input may not be an actual INSEE code")
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
    print("Bad API request : your input may not be an actual INSEE code")
  }
}