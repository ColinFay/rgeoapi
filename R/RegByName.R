#'Get Region by Name
#'
#'Get informations about a french region by its name. Please note that this package works only with french regions.
#'
#'Takes a name, returns a data.frame with the available values.
#'@param nom a character string with the name of the department. Partial matches are possible. In that case, typographic pertinence scores are given.
#'@return Returns a data.frame with name, INSEE code, and typographic pertinence score for the request. 
#'@export 
#'@examples
#'@examples
#'RegByName("Bretagne")
#'RegByName("M")

RegByName <- function(nom) {
  nom <- chartr("éèëêÉÈËÊàÀçÇôoœoöÔOŒOÖuûùüúUÛÙÜÚîïÎÏ", "eeeeEEEEaAcCoooooOOOOOuuuuuUUUUUIIII", nom)
  url <- paste0("https://geo.api.gouv.fr/regions?nom=", nom, "&fields=nom,code")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No content for that name : your input may not be an actual region name.")
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
        if(is.null(obj["_score"])) {
          score <- NA
        } else {
          score <- obj["_score"]
        }
        objbis <- data.frame(name = nom, codeInsee = codeInsee,score = score, stringsAsFactors = FALSE)
        identity <- rbind(identity,objbis)
      }
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual region name.")
  }
}

