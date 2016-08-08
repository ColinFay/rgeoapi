#'Get Department by Name
#'
#'Get informations about a french Department by its name. Please note that this package works only with french Department.
#'
#'Takes a department INSEE code, returns a data.frame with the available values. Partial matches are possible. In that case, typographic pertinence scores are given.
#'@param nom a character string with the name of the department. 
#'@return Returns a data.frame with name, INSEE code, and region code of the department. 
#'@export 
#'@examples
#'DepByName(nom = "Ille-et-Vilaine")
#'DepByName(nom = "mo")

DepByName <- function(nom) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codeRegion = vector("character"))
  nom <- chartr("éèëêÉÈËÊàÀçÇôoœoöÔOŒOÖuûùüúUÛÙÜÚîïÎÏ", "eeeeEEEEaAcCoooooOOOOOuuuuuUUUUUIIII", nom)
  url <- paste0("https://geo.api.gouv.fr/departements?nom=", nom, "&fields=nom,code,codeRegion")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that name : your input may not be an actual name (was ", nom,")")
      identity <- default
    } else {
      identity <- lapply(content, function(obj){
        data.frame(name = obj$nom %||% NA, 
                   codeInsee = obj$code %||% NA, 
                   codeRegion = obj$codeRegion %||% NA, 
                   stringsAsFactors = FALSE)
      }) %>% do.call(rbind, .)
    }
    return(identity)
  } else {
    warning("Bad API request : your input may not be an actual name (was ", nom,")")
    identity <- default
    return(identity)
  }
}
