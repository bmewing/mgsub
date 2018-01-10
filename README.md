# mgsub 

***A safe, multiple, simultaneous string substitution function***

[![Build Status](https://travis-ci.org/bmewing/mgsub.svg?branch=master)](https://travis-ci.org/bmewing/mgsub)

## Why do I want this?
You have a string you want to make substitutions on. You want to make many different substitutions at the same time and you want them done in a safe way.
For example, you want to shift each word in "hey, how are you?" to the left by replacing "hey" with "how", "how" with "are, etc.  Existing functions either do not support vectorization (e.g. `gsub`), don't support simultaneous substition (e.g. `stringr::str_replace_all`) or do the replacements in an unsafe manner (e.g. `qdap::mgsub`).
This is a lightweight, pure R function with no dependencies to avoid package bloat when being used.

## Install it!
    
    #install from this github repo
    devtools::install_github("bmewing/mgsub")

## Usage

Conversions are supplied using a named list.  The name is the matching pattern, the value is the replacement.

`mgsub::mgsub("hey, how are you?",list("hey"="how","how"="are","are"="you","you"="hey"))`

Matches and replacements can be supplied as regex expressions.  Additional arguments can be passed to the `sub`/`gsub`/`gregexpr` family of internal functions.

`mgsub::mgsub("Dopazamine is not the same as Dopachloride and is still fake.", list("[Dd]opa(.*?mine)"="Meta\\1","fake"="real"),ignore.case=F)`
