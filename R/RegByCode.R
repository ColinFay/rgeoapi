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
  if(nchar(codeRegion) == 1) {
    codeRegion <- paste0("0", codeRegion)
  }
  url <- paste0("https://geo.api.gouv.fr/regions?code=", codeRegion, "&fields=nom,code,codeRegion")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No Content for that INSEE code : your input may not be an actual INSEE code")
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
        identity <- data.frame(name = nom, codeInsee = codeInsee, stringsAsFactors = FALSE)
      }
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual INSEE code")
  }
}
