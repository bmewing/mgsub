![mgsub logo](https://s3.amazonaws.com/mgsub/logo.jpg)

***A safe, multiple, simultaneous string substitution function***

[![Build Status](https://travis-ci.org/bmewing/mgsub.svg?branch=master)](https://travis-ci.org/bmewing/mgsub) [![Coverage Status](https://img.shields.io/codecov/c/github/bmewing/mgsub/master.svg)](https://codecov.io/github/bmewing/mgsub?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mgsub)](https://CRAN.R-project.org/package=mgsub) ![](http://cranlogs.r-pkg.org/badges/mgsub) [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/2029/badge)](https://bestpractices.coreinfrastructure.org/projects/2029)

## Why do I want this?
You have a string you want to make substitutions on. You want to make many different substitutions at the same time and you want them done in a safe way.
For example, you want to shift each word in "hey, how are you?" to the left by replacing "hey" with "how", "how" with "are, etc.  Existing functions either do not support vectorization (e.g. `gsub`), don't support simultaneous substition (e.g. `stringr::str_replace_all`) or do the replacements in an unsafe manner (e.g. `qdap::mgsub`).
This is a lightweight, pure R function with no dependencies to avoid package bloat when being used.

## Install it!

```r   
install.packages('mgsub')
#Install the latest version from GitHub
#devtools::install_github("bmewing/mgsub")
```

## Usage

Simply pass in a vector of strings to be modified, a vector of patterns to match and a vector of replacements. Then watch as they are safely, simultaneously replaced!

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
