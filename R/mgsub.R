#' Safe, multiple gsub
#' 
#' \code{mgsub} - A safe, simultaneous, multiple global string replacement that 
#' takes a vector of replacement strings, named with their matching search terms
#' and applies them to a single string to be modified.
#'
#' @param string a character vector where replacements are sought
#' @param conversions named list of conversions to apply
#' @param \dots arguments to pass to \code{\link[base]{regexpr}} / \code{\link[base]{sub}}
#' @rdname mgsub
#' @return Converted string.
#' @examples
#' mgsub("hey, ho",list("hey"="ho","ho"="hey"))
#' mgsub("developer",list("e" ="p", "p" = "e"))
#' mgsub("The chemical Dopaziamine is fake",
#'       list("dopa(.*?) "="mega\\1 ","fake"="real"),
#'       ignore.case=TRUE)
#' @export

mgsub = function(string,conversions=list(),...){
  if(is.null(names(conversions))) stop("The object provided for `conversions` must be named")
  newString = ""
  conversions = conversions[order(nchar(names(conversions)),decreasing = T)]
  while(nchar(string) > 0){
    matches = lapply(names(conversions),regexpr,text=string,...)
    m = unlist(lapply(matches,`[[`,1))
    if(all(m < 0)){
      newString = paste0(newString,string)
      string = ''
    } else {
      m[m<0] = Inf
      fr = which(m==min(m))
      nc = unlist(lapply(matches,attr,"match.length"))[fr]
      fr = fr[which.max(nc)]
      nc = nc[which.max(nc)]
      fp = unlist(lapply(matches,`[[`,1))[fr]
      if(fp > 1){
        newString = paste0(newString,substr(string,1,fp-1))
      }
      newString = paste0(newString,sub(names(conversions)[fr],conversions[[fr]],substr(string,fp,fp+nc-1),...))
      string = substr(string,fp+nc,nchar(string))
    }
  }
  return(newString)
}


#' Safe, multiple gsub
#' 
#' \code{mgsub_alt} - Alternate call to \code{mgsub} that 
#' takes a vector of search terms, a vector of replacements and applies them to a 
#' single string to be modified.
#' 
#' @param pattern Character string to be matched in the given character vector
#' @param replacement Character string equal in length to pattern or of length one which are 
#' a replacement for matched pattern.
#' @param recycle logical. should replacement be recylced if lengths differ?
#' 
#' @rdname mgsub
#' @export

mgsub_alt = function(pattern,replacement,string,recycle=FALSE,...){
  if(!recycle & length(pattern) != length(replacement)) stop("pattern and replacement vectors must be the same length")
  if(length(replacement) > length(pattern)){
    warning("You provided more replacements than search strings - some will be dropped")
    replacement = replacement[seq_along(pattern)]
  }
  if(recycle & length(pattern) != length(replacement)){
    replacement = rep(replacement,ceiling(length(pattern) / length(replacement)))[seq_along(pattern)]
  } 
  names(replacement) = pattern
  return(mgsub(string=string, conversions=replacement,...))
}
