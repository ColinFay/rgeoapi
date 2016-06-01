#'Get by name
#'
#'Takes a the name of a French commune and returns name, code Insee, postal code, area, latitude and longitude
#'@param Name the name of a commune 
#'@return a data.frame with names, postal code, INSEE code, surface, lat and long. 
#'@export
#'@examples
#'getByName("Épinal")
#'
getByName <- function(Name) {
  Name <- chartr("éèëêÉÈËÊàÀçÇôoœoöÔOŒOÖuûùüúUÛÙÜÚîïÎÏ", "eeeeEEEEaAcCoooooOOOOOuuuuuUUUUUIIII", Name)
  url <- paste0("https://geo.api.gouv.fr/communes?nom=", Name, "&fields=nom,codeInsee,codesPostaux,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("Error Nom")
    } else {
      identity <- data.frame()
      for(i in 1:length(content)) {
        obj <- content[[i]]
        if(is.null(obj$nom)) {
          city <- NA
        } else {
          city <- obj$nom
        }
        if(is.null(obj$codeInsee)) {
          codeInsee <- NA
        } else {
          codeInsee <- obj$codeInsee
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
        objbis <- data.frame(name = city, codeInsee = codeInsee, codesPostaux = codesPostaux, surface = surface, lat=lat, long=long, stringsAsFactors = FALSE)
        identity <- rbind(identity,objbis)
      }
      return(identity)
    }
  } else {
    print("API Error")
  }
}

