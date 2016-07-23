#'Get City by Name
#'
#'Get informations about a french city by its name (partial matches possible). Please note that this package works only with french cities.
#'
#'Takes a the name of a French city, returns a data.frame with the available values. Partial matches are possible. In that case, typographic pertinence scores are given, and can be weighted by population with the "boost" argument.
#'@param nom a character string with the name of the city. 
#'@param boost a TRUE or FALSE. Default is FALSE. If TRUE, typographic pertinence score will be weighted by population. 
#'@param postal wether or not to include postal codes. Default is FALSE.  
#'@return Returns a data.frame with name(s), INSEE code(s), postal code(s), INSEE department code(s), INSEE region code(s), population (approx), surface(s) (in hectares), lat and long (WGS-84).  
#'@export
#'@examples
#'ComByName("Brest")
#'ComByName("Vitré", boost = TRUE)
#'ComByName("Lo")

ComByName <- function(nom, boost = FALSE) {
  nom <- chartr("éèëêÉÈËÊàÀçÇôoœoöÔOŒOÖuûùüúUÛÙÜÚîïÎÏ", "eeeeEEEEaAcCoooooOOOOOuuuuuUUUUUIIII", nom)
  if(postal){
    requete <- "nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre"
  } else {
    requete <- "nom,code,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre"
  }
  if(boost){
    url <- paste0("https://geo.api.gouv.fr/communes?nom=", nom, "&boost=population&fields=", requete)
    } else {    
      url <- paste0("https://geo.api.gouv.fr/communes?nom=", nom, "&fields=", requete)
      }
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No content for that name : your input may not be an actual city name.")
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
        if(is.null(obj["_score"])) {
          score <- NA
        } else {
          score <- obj["_score"]
        }
        if(postal){
          objbis <- data.frame(name = nom, codeInsee = codeInsee, codesPostaux = codesPostaux, codeDepartement = codeDepartement, codeRegion = codeRegion, population = population, surface = surface, lat=lat, long=long, score = score, stringsAsFactors = FALSE)
          identity <- rbind(identity,objbis)
          } else {
          objbis <- data.frame(name = nom, codeInsee = codeInsee, codeDepartement = codeDepartement, codeRegion = codeRegion, population = population, surface = surface, lat=lat, long=long, score = score, stringsAsFactors = FALSE)
          identity <- rbind(identity,objbis)
        }
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual city name.")
  }
}


