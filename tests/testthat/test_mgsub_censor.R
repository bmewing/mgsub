#mgsub_censor--------
context("Censor Call")
suppressWarnings(RNGversion("3.5.0"))

test_that("basic functionality",{
  expect_equal(mgsub_censor("hey, ho",pattern=c("hey","ho"),censor="*"),"***, **")
  expect_equal(mgsub_censor("Production Workers, All Other",c("s$","s([[:punct:] ])",",? All Other$"),"*"),"Production Workers***********")
})

test_that("non-named mgsub_censor and named sub works",{
  expect_equal(mgsub_censor("Dopazamine and dopaloramide are fake chemicals."
                            ,c("dopa","fake"),"?",ignore.case=TRUE)
               ,"????zamine and ????loramide are ???? chemicals.")
  expect_equal(mgsub_censor("Hey, ho",ignore.case=TRUE,c("hey"),"&"),"&&&, ho")
})

test_that("partially named mgsub_censor inputs works",{
  expect_equal(mgsub_censor("Dopazamine and dopaloramide are fake chemicals."
                            ,pattern=c("dopa","fake"),"*",ignore.case=TRUE)
               ,"****zamine and ****loramide are **** chemicals.")
  expect_equal(mgsub_censor("Dopazamine and dopaloramide are fake chemicals."
                            ,"*",pattern=c("dopa","fake"),ignore.case=TRUE)
               ,"****zamine and ****loramide are **** chemicals.")
})

test_that("multiple input strings are processed",{
  expect_equal(mgsub_censor(c("string","test"),c("t"),"*")
               ,c("s*ring","*es*"))
  expect_equal(mgsub_censor(c("Dopazamine is not the same as Dopachloride and is still fake."
                              ,"dopazamine is undergoing a review by the fake news arm of the Dopazamine government"),
                            c("[Dd]opa(.*?mine)","fake"),"*"),
               c("********** is not the same as Dopachloride and is still ****.", 
                 "********** is undergoing a review by the **** news arm of the ********** government"))
})

test_that("NAs are correctly handled",{
  expect_equal(mgsub_censor(c("string",NA,"test"),c("t"),"*")
               ,c("s*ring",NA,"*es*"))
})

test_that("works even with start/end symbols",{
  expect_equal(mgsub_censor(c("hi there","who said hi to me?","who said hi"),c("^hi"),"*")
               ,c("** there","who said hi to me?","who said hi"))
  expect_equal(mgsub_censor(c("hi there","who said hi to me?","who said hi"),c("hi$"),"*")
               ,c("hi there","who said hi to me?","who said **"))
})

test_that("all NA fails quickly",{
  expect_equal(mgsub_censor(rep(NA,4),"A","b"),rep(NA,4))
  expect_equal(mgsub_censor(NA,"A","b"),NA)
})

test_that("multicharacter censor works",{
  expect_equal(mgsub_censor("What the flip?","flip","?#!*",seed=10),"What the !##!?")
  expect_equal(mgsub_censor("What the flip?","flip",c("?","#","!","*"),seed=10),"What the !##!?")
  expect_equal(mgsub_censor("What the flip?","flip",c("?#","!*"),seed=10),"What the !##!?")
})

test_that("multicharacter censor can be used badly",{
  expect_equal(mgsub_censor("What the flip?","flip","?#!*",seed=10,split = FALSE)
               ,"What the ?#!*?#!*?#!*?#!*?")
  expect_equal(mgsub_censor("What the flip?","flip",c("?#","!*"),seed=10,split=FALSE)
               ,"What the !*?#?#!*?")
})

#censor_worker --------
context("censor_worker")

test_that("Letter substitution works",{
  expect_equal(unlist(censor_worker("ho ho hoot",c("h","o"),"*")),"** ** ***t")
  expect_equal(unlist(censor_worker("developer",c("e","p"),"*")),"d*v*lo**r")
})

test_that("Substring matches are superceded by longer matches",{
  expect_equal(unlist(censor_worker("they don't understand the value of what they seek."
                                    ,c("the","they"),"*"))
               ,"**** don't understand *** value of what **** seek.")
})

test_that("Regular expression matches work",{
  expect_equal(unlist(censor_worker("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake."
                                    ,c("[Dd]opa","fake"),"*"))
               ,"****zamine (of ****zamines family) is not the same as ****chloride and is still ****.")
})

test_that("Options passed to sub family work",{
  expect_equal(unlist(censor_worker("Dopazamine and dopaloramide are fake chemicals."
                                    ,c("dopa","fake"),"*",ignore.case=TRUE))
               ,"****zamine and ****loramide are **** chemicals.")
  expect_equal(unlist(censor_worker("Where are the \\bspaces\\b - not the spaces I want?"
                                    ,c("\\bspaces\\b","[Ww]here"),"*",fixed=TRUE))
               ,"Where are the ********** - not the spaces I want?")
})

test_that("Priority is based on matched length",{
  expect_equal(censor_worker("Dopazamine is a fake chemical",c("do.*ne","dopazamin"),"*",ignore.case=TRUE)
               ,"********** is a fake chemical")
})

test_that("all missing patterns works",{
  expect_equal(censor_worker("hi there",c("why","not","go"),"*"),"hi there")
})

test_that("some missing patterns work",{
  expect_equal(censor_worker("hi there",c("hi","bye"),"*"),"** there")
})