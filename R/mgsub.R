#' Takes a list of conversions and applies it simultaneously to a string
#'
#' @param string a character vector where replacements are sought
#' @param conversions named list of conversions to apply
#' @param ... arguments to pass to gregexpr/sub
#' @return Converted string.
#' @examples
#' mgsub("hey, ho",list("hey"="ho","ho"="hey"))
#' mgsub("developer",list("e" ="p", "p" = "e"))
#' mgsub("The chemical Dopaziamine is fake",
#'       list("dopa(.*?) "="mega\\1 ","fake"="real"),
#'       ignore.case=TRUE)
#' @export

mgsub = function(string,conversions=list(),...){
  if(is.null(names(conversions))) stop("The object provided in conversions must be named")
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
