library(StQ)
context("Class VarNameCorresp")

test_that("initializer returns the correct empty VNC class object", {
    EmptyVNC <- new(Class = 'VarNameCorresp')
    expect_that(slotNames(EmptyVNC), is_identical_to('VarNameCorresp'))
    expect_that(slot(EmptyVNC, 'VarNameCorresp'), 
                is_identical_to(
                    list(Microdata = 
                             data.table(IDQual = character(0), 
                                        NonIDQual = character(0),
                                        IDDD = character(0),
                                        Unit1 = character(0)))))
})
