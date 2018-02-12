#NSE Wrapper--------
context("NSE wrapper")

test_that("named inputs correctly route",{
  expect_equal(mgsub("hey, ho",pattern=c("hey","ho"),replacement=c("ho","hey")),"ho, hey")
  expect_equal(mgsub("hey, ho",conversions=list("hey"="ho","ho"="hey")),"ho, hey")
  expect_equal(mgsub("hey, ho",conversions=c("hey"="ho","ho"="hey")),"ho, hey")
})

test_that("non-named inputs correctly route",{
  expect_equal(mgsub("hey, ho",c("hey","ho"),c("ho","hey")),"ho, hey")
  expect_equal(mgsub("hey, ho",list("hey"="ho","ho"="hey")),"ho, hey")
  expect_equal(mgsub("hey, ho",c("hey"="ho","ho"="hey")),"ho, hey")
})

test_that("non-named mgsub and named sub works",{
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",c("dopa","fake"),c("meta","real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",list("dopa"="meta","fake"="real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",c("dopa"="meta","fake"="real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_error(mgsub("Hey, ho",ignore.case=TRUE,c("hey"),c("tomorrow"),list("today")))
  expect_equal(mgsub("Hey, ho",ignore.case=TRUE,c("hey"),c("tomorrow"),list("hey"="today")),"today, ho")
  expect_equal(mgsub("Hey, ho",ignore.case=TRUE,c("hey"),c("tomorrow")),"tomorrow, ho")
})

test_that("partially named mgsub inputs works",{
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",pattern=c("dopa","fake"),c("meta","real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",c("meta","real"),pattern=c("dopa","fake"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub(list("dopa"="meta","fake"="real"),string="Dopazamine and dopaloramide are fake chemicals.",ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
})

test_that("multiple named arguments warns and defaults to dict",{
  expect_warning(tmp <- mgsub(pattern=c("dopa","fake"),replacement=c("beta","super fake"),conversions=list("dopa"="meta","fake"="real"),string="Dopazamine and dopaloramide are fake chemicals.",ignore.case=TRUE))
  expect_true(identical(tmp,"metazamine and metaloramide are real chemicals."))
})

test_that("multiple input strings are processed",{
  expect_equal(mgsub(c("string","test"),c("t"="p")),c("spring","pesp"))
})

#Vector mode -----
context("Vector mode")

test_that("non-recycled non-equal length match and replace input fails",{
  expect_error(mgsub_vm("hey, ho",c("hey","ho"),c("yo")))
  expect_error(mgsub_vm("hey, ho",c("hey"),c("ho","yo")))
})

test_that("recycled longer replace than match warns and truncates",{
  expect_warning(mgsub_vm("hey, ho",c("hey"),c("ho","hey"),recycle = TRUE))
})

test_that("recycled replacements works",{
  expect_equal(mgsub_vm("hey, ho",c("hey","ho"),"yo",recycle = TRUE),"yo, yo")
})

test_that("Letter substitution works",{
  expect_equal(mgsub_vm("ho ho hoot",c("h","o"),c("o","h")),"oh oh ohht")
  expect_equal(mgsub_vm("developer",c("e","p"),c("p","e")),"dpvploepr")
})

test_that("Non-equilength matches replace",{
  expect_equal(mgsub_vm("hey, ho",c("hey","ho"),c("ho","hey")),"ho, hey")
  expect_equal(mgsub_vm("hi there, buddy boy!",c("there","buddy"),c("where","enemy")),"hi where, enemy boy!")
})

test_that("Substring matches are superceded by longer matches",{
  expect_equal(mgsub_vm("they don't understand the value of what they seek.",c("the","they"),c("a","we")),"we don't understand a value of what we seek.")
})

test_that("Same length matches are safely converted",{
  expect_equal(mgsub_vm("hey, how are you?",c("hey","how","are","you"),c("how","are","you","hey")),"how, are you hey?")
})

test_that("Regular expression matches work",{
  expect_equal(mgsub_vm("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake.",c("[Dd]opa","fake"),c("Meta","real")),"Metazamine (of Metazamines family) is not the same as Metachloride and is still real.")
})

test_that("Regular expression substitions work",{
  expect_equal(mgsub_vm("Dopazamine is not the same as Dopachloride and is still fake.",c("[Dd]opa(.*?mine)","fake"),c("Meta\\1","real")),"Metazamine is not the same as Dopachloride and is still real.")
})

test_that("Options passed to sub family work",{
  expect_equal(mgsub_vm("Dopazamine and dopaloramide are fake chemicals.",c("dopa","fake"),c("meta","real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub_vm("Where are the \\bspaces\\b - not the spaces I want?",c("\\bspaces\\b","[Ww]here"),c("boxes","There"),fixed=TRUE),"Where are the boxes - not the spaces I want?")
})

test_that("Priority is based on matched length",{
  expect_equal(mgsub_vm("Dopazamine is a fake chemical",c("do.*ne","dopazamin"),c("metazamine","freakout"),ignore.case=TRUE),"metazamine is a fake chemical")
})

#Dictionary mode ------
context("Dictionary mode")

test_that("Letter substitution works",{
  expect_equal(mgsub_dict("ho ho hoot",list("h"="o","o"="h")),"oh oh ohht")
  expect_equal(mgsub_dict("developer",list("e" ="p", "p" = "e")),"dpvploepr")
})

test_that("Non-equilength matches replace",{
  expect_equal(mgsub_dict("hey, ho",list("hey"="ho","ho"="hey")),"ho, hey")
  expect_equal(mgsub_dict("hi there, buddy boy!",list("there"="where","buddy"="enemy")),"hi where, enemy boy!")
})

test_that("Substring matches are superceded by longer matches",{
  expect_equal(mgsub_dict("they don't understand the value of what they seek.",list("the"="a","they"="we")),"we don't understand a value of what we seek.")
})

test_that("Same length matches are safely converted",{
  expect_equal(mgsub_dict("hey, how are you?",list("hey"="how","how"="are","are"="you","you"="hey")),"how, are you hey?")
})

test_that("Regular expression matches work",{
  expect_equal(mgsub_dict("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake.",list("[Dd]opa"="Meta","fake"="real")),"Metazamine (of Metazamines family) is not the same as Metachloride and is still real.")
})

test_that("Regular expression substitions work",{
  expect_equal(mgsub_dict("Dopazamine is not the same as Dopachloride and is still fake.",list("[Dd]opa(.*?mine)"="Meta\\1","fake"="real")),"Metazamine is not the same as Dopachloride and is still real.")
})

test_that("Options passed to sub family work",{
  expect_equal(mgsub_dict("Dopazamine and dopaloramide are fake chemicals.",list("dopa"="meta","fake"="real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub_dict("Where are the \\bspaces\\b - not the spaces I want?",list("\\bspaces\\b"="boxes","[Ww]here"="There"),fixed=TRUE),"Where are the boxes - not the spaces I want?")
})

test_that("Priority is based on matched length",{
  expect_equal(mgsub_dict("Dopazamine is a fake chemical",list("do.*ne"="metazamine","dopazamin"="freakout"),ignore.case=TRUE),"metazamine is a fake chemical")
})

test_that("Function fails when non-named object is passed",{
  expect_error(mgsub_dict("hey ho",list("hey","ho")))
  expect_error(mgsub_dict("hey ho",c("hey","ho")))
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
  expect_equal(unlist(worker("Dopazamine is a fake chemical",c("do.*ne","dopazamin"),c("metazamine","freakout"),ignore.case=TRUE)),"metazamine is a fake chemical")
})

#Backwards compatibility------
context("Backwards Compatibility")

test_that("Letter substitution works",{
  expect_equal(mgsub("ho ho hoot",list("h"="o","o"="h")),"oh oh ohht")
  expect_equal(mgsub("developer",list("e" ="p", "p" = "e")),"dpvploepr")
})

test_that("Non-equilength matches replace",{
  expect_equal(mgsub("hey, ho",list("hey"="ho","ho"="hey")),"ho, hey")
  expect_equal(mgsub("hi there, buddy boy!",list("there"="where","buddy"="enemy")),"hi where, enemy boy!")
})

test_that("Substring matches are superceded by longer matches",{
  expect_equal(mgsub("they don't understand the value of what they seek.",list("the"="a","they"="we")),"we don't understand a value of what we seek.")
})

test_that("Same length matches are safely converted",{
  expect_equal(mgsub("hey, how are you?",list("hey"="how","how"="are","are"="you","you"="hey")),"how, are you hey?")
})

test_that("Regular expression matches work",{
  expect_equal(mgsub("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake.",list("[Dd]opa"="Meta","fake"="real")),"Metazamine (of Metazamines family) is not the same as Metachloride and is still real.")
})

test_that("Regular expression substitions work",{
  expect_equal(mgsub("Dopazamine is not the same as Dopachloride and is still fake.",list("[Dd]opa(.*?mine)"="Meta\\1","fake"="real")),"Metazamine is not the same as Dopachloride and is still real.")
})

test_that("Options passed to sub family work",{
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",list("dopa"="meta","fake"="real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Where are the \\bspaces\\b - not the spaces I want?",list("\\bspaces\\b"="boxes","[Ww]here"="There"),fixed=TRUE),"Where are the boxes - not the spaces I want?")
})

test_that("Priority is based on matched length",{
  expect_equal(mgsub("Dopazamine is a fake chemical",list("do.*ne"="metazamine","dopazamin"="freakout"),ignore.case=TRUE),"metazamine is a fake chemical")
})

test_that("Function fails when non-named object is passed",{
  expect_error(mgsub("hey ho",list("hey","ho")))
  expect_error(mgsub("hey ho",c("hey","ho")))
})