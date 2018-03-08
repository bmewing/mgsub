#' Safe, multiple gsub
#' 
#' \code{mgsub} - A safe, simultaneous, multiple global string replacement wrapper that 
#' allows access to multiple methods of specifying matches and replacements.
#'
#' @param string a character vector where replacements are sought
#' @param pattern Character string to be matched in the given character vector
#' @param replacement Character string equal in length to pattern or of length one which are 
#' a replacement for matched pattern.
#' @param recycle logical. should replacement be recylced if lengths differ?
#' @param conversions DEPRECATED - will be removed in a later release
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

mgsub = function(string,pattern,replacement,recycle=FALSE,conversions=list(),...){
  sna = !is.na(string)
  if((missing(replacement) & !missing(pattern)) | (missing(replacement) & missing(pattern) & length(conversions) > 0)){
    warning("Calling mgsub with a named list is deprecated. You may continue to call mgsub_dict.")
    passthrough = if(missing(pattern)) conversions else pattern
    result = mgsub_dict(string[sna],passthrough,...)
  } else {
    if(!is.logical(recycle)) stop("Recycle must be a boolean")
    if(!recycle & length(pattern) != length(replacement)) stop("pattern and replacement vectors must be the same length")
    if(length(replacement) > length(pattern)){
      warning("You provided more replacements than search strings - some will be dropped")
      replacement = replacement[seq_along(pattern)]
    }
    if(recycle & length(pattern) != length(replacement)){
      replacement = rep(replacement,ceiling(length(pattern) / length(replacement)))[seq_along(pattern)]
    } 
    names(replacement) = pattern
    result = unlist(worker(string[sna], pattern, replacement,...))
  }
  string[sna] = result
  return(string)
}

#' Safe, multiple gsub
#' 
#' \code{mgsub_dict} - Dictionary version of \code{mgsub} that 
#' takes a vector of replacement strings, named with their matching search terms
#' and applies them to a single string to be modified.
#' 
#' @rdname mgsub
#' @export

mgsub_dict = function(string,conversions=list(),...){
  if(is.null(names(conversions))) stop("The object provided for `conversions` must be named")
  return(unlist(worker(string,names(conversions),unlist(conversions),...)))
}

worker = function(string,p,r,...){
  if(length(string)==1){
    return(w2(string,p,r,...))
  }
  if(any(grepl("\\$|\\^",p))){
    return(lapply(string,w2,p=p,r=r,...))
  }
  div = c("\001", "\002", "\003", "\004", "\005", "\006", "\a", "\b", 
                 "\t", "\n", "\v", "\f", "\r", "\016", "\017", "\020", "\021", 
                 "\022", "\023", "\024", "\025", "\026", "\027", "\030", "\031", 
                 "\032", "\033", "\034", "\035", "\036", "\037", "\177")
  while(any(grepl(div[1],string))){
    div = div[-1]
  }
  s = paste(string,collapse = div[1])
  p = gsub("([^\\])\\.",paste0("\\1[^",div[1],"]"),p)
  return(strsplit(w2(s,p,r,...),div,fixed=T))
}

w2 = function(s,p,r,...){
  newString = ""
  while(nchar(s) > 0){
    matches = lapply(p,regexpr,text=s,...)
    m = unlist(lapply(matches,`[[`,1))
    if(all(m < 0)){
      newString = paste0(newString,s)
      s = ''
    } else {
      m[m<0] = Inf
      fr = which(m==min(m))
      nc = unlist(lapply(matches,attr,"match.length"))[fr]
      fr = fr[which.max(nc)]
      nc = nc[which.max(nc)]
      fp = unlist(lapply(matches,`[[`,1))[fr]
      if(fp > 1){
        newString = paste0(newString,substr(s,1,fp-1))
      }
      newString = paste0(newString,sub(p[fr],r[fr],substr(s,fp,fp+nc-1),...))
      s = substr(s,fp+nc,nchar(s))
    }
  }
  return(newString)
}