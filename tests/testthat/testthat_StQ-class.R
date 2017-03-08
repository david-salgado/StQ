library(StQ)
context("Class StQ")

test_that("initializer returns the correct empty StQ class object", {
    EmptyStQ <- new(Class = 'StQ')
    expect_that(slotNames(EmptyStQ), is_identical_to(c('Data', 'DD')))
    expect_that(slot(EmptyStQ, 'Data'), 
                is_identical_to(new(Class = 'Datadt')))
    expect_that(slot(EmptyStQ, 'DD'), 
                is_identical_to(new(Class = 'DD')))
})
