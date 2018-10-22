#' @export

mgsub = function(string,pattern,replacement,recycle=FALSE,...){
  #' @title Safe, multiple gsub
  #' 
  #' @description \code{mgsub} - A safe, simultaneous, multiple global string 
  #' replacement wrapper that allows access to multiple methods of specifying 
  #' matches and replacements.
  #'
  #' @param string a character vector where replacements are sought
  #' @param pattern Character string to be matched in the given character vector
  #' @param replacement Character string equal in length to pattern or of length 
  #' one which are a replacement for matched pattern.
  #' @param recycle logical. should replacement be recycled if lengths differ?
  #' @param \dots arguments to pass to \code{\link[base]{regexpr}} / 
  #' \code{\link[base]{sub}}
  #' @rdname mgsub
  #' @return Converted string.
  #' @examples
  #' mgsub("hey, ho",pattern=c("hey","ho"),replacement=c("ho","hey"))
  #' mgsub("developer",pattern=c("e","p"),replacement=c("p","e"))
  #' mgsub("The chemical Dopaziamine is fake",
  #'       pattern=c("dopa(.*?) ","fake"),
  #'       replacement=c("mega\\1 ","real"),
  #'       ignore.case=TRUE)

  if(all(is.na(string))) return(string)
  sna = !is.na(string)
  if(!is.logical(recycle)) stop("Recycle must be a boolean")
  if(!recycle & length(pattern) != length(replacement)){
    stop("pattern and replacement vectors must be the same length")
  }
  if(length(replacement) > length(pattern)){
    warning("You provided more replacements than 
            search strings - some will be dropped")
    replacement = replacement[seq_along(pattern)]
  }
  if(recycle & length(pattern) != length(replacement)){
    lp = length(pattern)
    lr = length(replacement)
    replacement = rep(replacement,ceiling(lp/lr))[seq_along(pattern)]
  } 
  result = vapply(X = string[sna],
                  FUN = worker,
                  FUN.VALUE = c(""),
                  USE.NAMES = FALSE,
                  pattern=pattern,
                  replacement=replacement,...)
  string[sna] = result
  return(string)
}

worker = function(string,pattern,replacement,...){
  #' @title mgsub worker
  #' 
  #' @description The hard worker doing everything for mgsub
  #' 
  #' @param string a character vector where replacements are sought
  #' @param pattern Character string to be matched in the given character vector
  #' @param replacement Character string equal in length to pattern or of length 
  #' one which are a replacement for matched pattern.
  #' @param \dots arguments to pass to regexpr family
  
  x0 = do.call(rbind,lapply(seq_along(pattern),
                            getMatches,
                            string=string,
                            pattern=pattern,...))
  x0 = matrix(x0[x0[,2] != -1,],ncol=4)
  uid = unique(x0[,1])
  if(nrow(x0)==0) return(string)
  if(length(unique(x0[,1])) == 1){
    return(fastReplace(string,pattern[uid],replacement[uid],...))
  }
  if(nrow(x0) > 1){
    x = x0[order(x0[,3],decreasing = T),]
    x = filterOverlap(x)
    uid = unique(x[,1])
    if(length(uid) == 1){
      return(fastReplace(string,pattern[uid],replacement[uid],...))
    }
    x = x[order(x[,2]),] 
  }
  for(i in nrow(x):1){
    s = x[i,2]
    e = x[i,4]
    p = pattern[x[i,1]]
    r = replacement[x[i,1]]
    pre = if(s > 1) substr(string,1,s-1) else ""
    r0 = sub(p,r,substr(string,s,e),...)
    end = if(e < nchar(string)) substr(string,e+1,nchar(string)) else ""
    string = paste0(pre,r0,end)
  }
  return(string)
}