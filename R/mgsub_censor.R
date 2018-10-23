#' @export

mgsub_censor = function(string,pattern,censor="*",split=any(nchar(censor) > 1),seed=NULL,...){
  #' @title Safe, multiple censoring of text strings
  #' 
  #' @description \code{mgsub_censor} - A safe, simultaneous, multiple global string censoring
  #'  (replace matches with a censoring character like '*')
  #'
  #' @param string a character vector to censor
  #' @param pattern regular expressions used to identify where to censor
  #' @param censor character to use in censoring - see details
  #' @param split if a multicharacter censor pattern is provided, should it be 
  #' split to preserve original string length
  #' @param seed optional parameter to fix sampling of multicharacter censors
  #' @param \dots arguments to pass to \code{\link[base]{regexpr}} / 
  #' \code{\link[base]{sub}}
  #' @rdname mgsub_censor
  #' @return Censored string.
  #' @details When censor is provided as a >1 length vector or as a multicharacter 
  #' string with split = TRUE, it will be sampled to return random censoring patterns. 
  #' This can be helpful if you want to create cartoonish swear censoring. If 
  #' needed, the randomization can be controlled with the seed argument.
  #' 
  #' @examples
  #' mgsub_censor("Flowers for a friend",pattern=c("low"),censor="*")
  
  if(all(is.na(string))) return(string)
  sna = !is.na(string)
  result = vapply(string[sna]
                 ,censor_worker
                 ,c("")
                 ,USE.NAMES = FALSE
                 ,pattern=pattern
                 ,censor=censor
                 ,split=split
                 ,seed=seed
                 ,...)
  string[sna] = result
  return(string)
}

censor_worker = function(string,pattern,censor,split=any(nchar(censor) > 1),seed=NULL,...){
  #' @title mgsub_censor worker
  #' 
  #' @description The hard worker doing everything for mgsub_censor
  #' 
  #' @param string a character vector where replacements are sought
  #' @param pattern Character string to be matched in the given character vector
  #' @param censor character to use in censoring - see details
  #' @param split if a multicharacter censor pattern is provided, should it be 
  #' split to preserve original string length
  #' @param seed optional parameter to fix sampling of multicharacter censors
  #' @param \dots arguments to pass to regexpr family
  
  x0 = do.call(rbind,lapply(seq_along(pattern)
                           ,getMatches
                           ,string=string
                           ,pattern=pattern
                           ,...))
  x0 = matrix(x0[x0[,2] != -1,],ncol=4)
  uid = unique(x0[,1])
  if(nrow(x0)==0) return(string)
  if(nrow(x0) > 1){
    x = x0[order(x0[,3],decreasing = T),]
    x = filterOverlap(x)
    x = x[order(x[,2]),,drop=FALSE]
  } else {
    x = x0
  }
  for(i in nrow(x):1){
    s = x[i,2]
    e = x[i,4]
    p = pattern[x[i,1]]
    if(split) censor = unlist(strsplit(censor,""))
    if(!is.null(seed)) set.seed(seed)
    r = if(length(censor) > 1){
      paste(sample(censor,x[i,3],replace=TRUE),collapse="")
    } else {
      paste(rep(censor,x[i,3]),collapse="")
    }
    pre = if(s > 1) substr(string,1,s-1) else ""
    r0 = sub(p,r,substr(string,s,e),...)
    end = if(e < nchar(string)) substr(string,e+1,nchar(string)) else ""
    string = paste0(pre,r0,end)
  }
  return(string)
}
