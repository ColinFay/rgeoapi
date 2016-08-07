#'Get Region by INSEE Code
#'
#'Get informations about a french region by its INSEE code. Please note that this package works only with french regions.
#'
#'Takes an INSEE code, returns a data.frame with the available values.
#'@param codeRegion a numeric vector with an INSEE code.
#'@return Returns a data.frame with name and region code of the region. 
#'@export 
#'@examples
#'RegByCode(53)

RegByCode <- function(codeRegion) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"))
  if(nchar(codeRegion) == 1) {
    codeRegion <- paste0("0", codeRegion)
  }
  url <- paste0("https://geo.api.gouv.fr/regions?code=", codeRegion, "&fields=nom,code,codeRegion")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that INSEE code : your input may not be an actual INSEE code (was ", codeRegion,")")
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
    warning("Bad API request : your input may not be an actual INSEE code (was ", codeRegion,")")
    identity <- default
    return(identity)
  }
}