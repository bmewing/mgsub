#mgsub--------
context("Primary Call")

test_that("basic functionality",{
  expect_equal(mgsub("hey, ho",pattern=c("hey","ho"),replacement=c("ho","hey")),"ho, hey")
  expect_equal(mgsub("Production Workers, All Other",c("s$","s([[:punct:] ])",",? All Other$"),c("","\\1","")),"Production Workers")
})

test_that("non-named mgsub and named sub works",{
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",c("dopa","fake"),c("meta","real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Hey, ho",ignore.case=TRUE,c("hey"),c("tomorrow")),"tomorrow, ho")
})

test_that("partially named mgsub inputs works",{
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",pattern=c("dopa","fake"),c("meta","real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",c("meta","real"),pattern=c("dopa","fake"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
})

test_that("multiple input strings are processed",{
  expect_equal(mgsub(c("string","test"),c("t"),c("p")),c("spring","pesp"))
  expect_equal(mgsub(c("Dopazamine is not the same as Dopachloride and is still fake.","dopazamine is undergoing a review by the fake news arm of the Dopazamine government"),
                     c("[Dd]opa(.*?mine)","fake"),c("Meta\\1","real")),
                     c("Metazamine is not the same as Dopachloride and is still real.", 
                       "Metazamine is undergoing a review by the real news arm of the Metazamine government"))
})

test_that("NAs are correctly handled",{
  expect_equal(mgsub(c("string",NA,"test"),c("t"),c("p")),c("spring",NA,"pesp"))
})

test_that("recylce has to be a boolean",{
  expect_error(mgsub("hey, ho",c("hey"),c("ho","hey"),recycle = "yes"))
  expect_error(mgsub("hey, ho",c("hey"),c("ho","hey"),recycle = 1))
})

test_that("non-recycled non-equal length match and replace input fails",{
  expect_error(mgsub("hey, ho",c("hey","ho"),c("yo")))
  expect_error(mgsub("hey, ho",c("hey"),c("ho","yo")))
})

test_that("recycled longer replace than match warns and truncates",{
  expect_warning(mgsub("hey, ho",c("hey"),c("ho","hey"),recycle = TRUE))
})

test_that("recycled replacements works",{
  expect_equal(mgsub("hey, ho",c("hey","ho"),"yo",recycle = TRUE),"yo, yo")
})

test_that("works even with start/end symbols",{
  expect_equal(mgsub(c("hi there","who said hi to me?","who said hi"),c("^hi"),c("bye")),c("bye there","who said hi to me?","who said hi"))
  expect_equal(mgsub(c("hi there","who said hi to me?","who said hi"),c("hi$"),c("bye")),c("hi there","who said hi to me?","who said bye"))
})

test_that("all NA fails quickly",{
  expect_equal(mgsub(rep(NA,4),"A","b"),rep(NA,4))
  expect_equal(mgsub(NA,"A","b"),NA)
})

#Worker --------
context("Worker")

test_that("Letter substitution works",{
  expect_equal(unlist(worker("ho ho hoot",c("h","o"),c("o","h"))),"oh oh ohht")
  expect_equal(unlist(worker("developer",c("e","p"),c("p","e"))),"dpvploepr")
})

test_that("Non-equilength matches replace",{
  expect_equal(unlist(worker("hey, ho",c("hey","ho"),c("ho","hey"))),"ho, hey")
  expect_equal(unlist(worker("hi there, buddy boy!",c("there","buddy"),c("where","enemy"))),"hi where, enemy boy!")
})

test_that("Substring matches are superceded by longer matches",{
  expect_equal(unlist(worker("they don't understand the value of what they seek.",c("the","they"),c("a","we"))),"we don't understand a value of what we seek.")
})

test_that("Same length matches are safely converted",{
  expect_equal(unlist(worker("hey, how are you?",c("hey","how","are","you"),c("how","are","you","hey"))),"how, are you hey?")
})

test_that("Regular expression matches work",{
  expect_equal(unlist(worker("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake.",c("[Dd]opa","fake"),c("Meta","real"))),"Metazamine (of Metazamines family) is not the same as Metachloride and is still real.")
})

test_that("Regular expression substitions work",{
  expect_equal(unlist(worker("Dopazamine is not the same as Dopachloride and is still fake.",c("[Dd]opa(.*?mine)","fake"),c("Meta\\1","real"))),"Metazamine is not the same as Dopachloride and is still real.")
})

test_that("Options passed to sub family work",{
  expect_equal(unlist(worker("Dopazamine and dopaloramide are fake chemicals.",c("dopa","fake"),c("meta","real"),ignore.case=TRUE)),"metazamine and metaloramide are real chemicals.")
  expect_equal(unlist(worker("Where are the \\bspaces\\b - not the spaces I want?",c("\\bspaces\\b","[Ww]here"),c("boxes","There"),fixed=TRUE)),"Where are the boxes - not the spaces I want?")
})

test_that("Priority is based on matched length",{
  expect_equal(worker("Dopazamine is a fake chemical",c("do.*ne","dopazamin"),c("metazamine","freakout"),ignore.case=TRUE),"metazamine is a fake chemical")
})

test_that("all missing patterns works",{
  expect_equal(worker("hi there",c("why","not","go"),c("a","b","c")),"hi there")
})

test_that("some missing patterns work",{
  expect_equal(worker("hi there",c("hi","bye"),c("bye","hi")),"bye there")
})

test_that("two patterns, only overlap, fast exit",{
  expect_equal(worker("the the the",c("the","th"),c("a","b")),"a a a")
})