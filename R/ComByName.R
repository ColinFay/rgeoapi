#'Get City by Name
#'
#'Get informations about a French city by its name (partial matches possible). Please note that this package works only with French cities.
#'
#'Takes a the name of a French city, returns a data.frame with the available values. Partial matches are possible. In that case, typographic pertinence scores are given, and can be weighted by population with the "boost" argument.
#'@param nom a character string with the name of the city, in full ASCII character. 
#'@param boost a TRUE or FALSE. Default is FALSE. If TRUE, typographic pertinence score will be weighted by population. 
#'@param postal wether or not to include postal codes. Default is FALSE.  
#'@return Returns a data.frame with name(s), INSEE code(s), postal code(s), INSEE department code(s), INSEE region code(s), population (approx), surface(s) (in hectares), lat and long (WGS-84).  
#'@export
#'@examples
#'ComByName(nom = "Brest")
#'ComByName(nom = "Rennes", boost = TRUE)
#'ComByName(nom = "Lo", postal = TRUE)

ComByName <- function(nom, boost = FALSE, postal = FALSE) {
  . <- NULL 
  if(postal){
    default <- data.frame(name = vector("character"), 
                          codeInsee = vector("character"),
                          codesPostaux = vector("character"),
                          codeDepartement = vector("character"),
                          codeRegion = vector("character"),
                          population = vector("character"),
                          surface  = vector("character"),
                          lat  = vector("character"),
                          long = vector("character"), 
                          score = vector("character"))
  } else {
    default <- data.frame(name = vector("character"), 
                          codeInsee = vector("character"),
                          codeDepartement = vector("character"),
                          codeRegion = vector("character"),
                          population = vector("character"),
                          surface  = vector("character"),
                          lat  = vector("character"),
                          long = vector("character"))
  }
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
      warning("No Content for that name : your input may not be an actual name (was ", nom,")")
      identity <- default
    } else {
      if(postal){
        identity <- lapply(content, function(obj){
          data.frame(name = obj$nom %||% NA, 
                     codeInsee = obj$code %||% NA,
                     codesPostaux = obj$codesPostaux %||% NA,
                     codeDepartement = obj$codeDepartement %||% NA,
                     codeRegion = obj$codeRegion %||% NA,
                     population = obj$population %||% NA,
                     surface  = obj$surface %||% NA,
                     lat  = obj$centre$coordinates [2] %||% NA,
                     long = obj$centre$coordinates [1] %||% NA,
                     score = obj$`_score` %||% NA,
                     stringsAsFactors = FALSE)
        }) %>% do.call(rbind, .)  
      } else {
        identity <- lapply(content, function(obj){
          data.frame(name = obj$nom %||% NA, 
                     codeInsee = obj$code %||% NA,
                     codeDepartement = obj$codeDepartement %||% NA,
                     codeRegion = obj$codeRegion %||% NA,
                     population = obj$population %||% NA,
                     surface  = obj$surface %||% NA,
                     lat  = obj$centre$coordinates [2] %||% NA,
                     long = obj$centre$coordinates [1] %||% NA,
                     score = obj$`_score` %||% NA,
                     stringsAsFactors = FALSE)
        }) %>% do.call(rbind, .)  
      }
    }
    if(length(showNonASCII(nom)) != 0){
      warning("Your name '", nom, "' contains non ASCII character, this might impact the accuracy of your result. Please use only ASCII characters in your function call.")
    }
    return(identity)
  } else {
    warning("Bad API request : your input may not be an actual name (was ", nom,")")
    identity <- default
    return(identity)
  }
}
