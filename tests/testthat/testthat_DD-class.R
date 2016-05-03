library(StQ)
context("Class DD")

test_that("initializer returns the correct empty DD class object", {
    EmptyDD <- new(Class = 'DD')
    expect_that(slotNames(EmptyDD), is_identical_to(c('VarNameCorresp',
                                                      'ID',
                                                      'MicroData',
                                                      'ParaData',
                                                      'Aggregates',
                                                      'AggWeights',
                                                      'Other'))
    )
    expect_that(slot(EmptyDD, 'VarNameCorresp'), 
                is_identical_to(new(Class = 'VarNameCorresp'))
    )
})
