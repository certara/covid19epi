# https://stackoverflow.com/questions/21011672/automatically-add-variable-names-to-elements-of-a-list
listN <- function(...){
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}
