# mgsub 

***A safe, multiple, simultaneous string substitution function***

[![Build Status](https://travis-ci.org/bmewing/mgsub.svg?branch=dev)](https://travis-ci.org/bmewing/mgsub) [![Coverage Status](https://img.shields.io/codecov/c/github/bmewing/mgsub/dev.svg)](https://codecov.io/github/bmewing/mgsub?branch=dev) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mgsub)](https://CRAN.R-project.org/package=mgsub) ![](http://cranlogs.r-pkg.org/badges/mgsub)

## Why do I want this?
You have a string you want to make substitutions on. You want to make many different substitutions at the same time and you want them done in a safe way.
For example, you want to shift each word in "hey, how are you?" to the left by replacing "hey" with "how", "how" with "are, etc.  Existing functions either do not support vectorization (e.g. `gsub`), don't support simultaneous substition (e.g. `stringr::str_replace_all`) or do the replacements in an unsafe manner (e.g. `qdap::mgsub`).
This is a lightweight, pure R function with no dependencies to avoid package bloat when being used.

## Install it!

```r   
#install from this github repo
devtools::install_github("bmewing/mgsub")
```

## Usage

There are currently two supported method for specifying the patterns to match and their associated replacements. `r mgsub::mgsub` is a NSE wrapper that determines the method supplied and calls either `r mgsub::mgsub_dict` for dictionary style specification or `r mgusb::mgsub_vm` for a vector method specification.  You do not need to name the arguments when supplying them.

### Dictionary method

```r
mgsub::mgsub(string,conversions=list(),...)
```

Conversions are supplied using a named list.  The name is the matching pattern, the value is the replacement.

```r
mgsub::mgsub("hey, how are you?",list("hey"="how","how"="are","are"="you","you"="hey"))
```

Matches and replacements can be supplied as regex expressions.  Additional arguments can be passed to the `sub`/`gsub`/`gregexpr` family of internal functions.

```r
mgsub::mgsub("Dopazamine is not the same as Dopachloride and is still fake.", 
             list("[Dd]opa(.*?mine)"="Meta\\1","fake"="real"),ignore.case=F)
```

### Vector method

```r
mgsub::mgsub(string,pattern=c(),replacement=c(),recycle=FALSE,...)
```

The pattern to match is supplied first and the replacement vector follows.

```r
mgsub::mgsub("hey, how are you?",c("hey","how","are","you"),c("how","are","you","hey"))
```

Recycling is to make it easy to provide a single replacement (or a pattern of replacements) for multiple matches.

```r
mgsub::mgsub("hey, ho, let's go!",c("hey","ho","go"),"ugh",recycle=TRUE)
```

Matches and replacements can still be supplied as regex exressions. Additional arguments can be passed to the `sub`/`gsub`/`gregexpr` family of internal functions.

```r
mgsub::mgsub("Dopazamine is not the same as Dopachloride and is still fake.", 
             c("[Dd]opa(.*?mine)","fake"), c("Meta\\1","real"),ignore.case=F)
```