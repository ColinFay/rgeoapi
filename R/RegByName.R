#'Get Region by Name
#'
#'Get informations about a French region by its name. Please note that this package works only with French regions.
#'
#'Takes a name, returns a data.frame with the available values.
#'@param nom a character string with the name of the department. Partial matches are possible. In that case, typographic pertinence scores are given.
#'@return Returns a data.frame with name, INSEE code, and typographic pertinence score for the request. 
#'@export 
#'@examples
#'RegByName(nom = "Bretagne")
#'RegByName(nom = "M")

RegByName <- function(nom) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codeRegion = vector("character"))
  url <- paste0("https://geo.api.gouv.fr/regions?nom=", nom, "&fields=nom,code")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that name : your input may not be an name (was ", nom,")")
      identity <- default
    } else {
      identity <- lapply(content, function(obj){
        data.frame(name = obj$nom %||% NA, 
                   codeInsee = obj$code %||% NA, 
                   score = score <- obj["_score"] %||% NA,
                   stringsAsFactors = FALSE)
      }) %>% do.call(rbind, .)
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

