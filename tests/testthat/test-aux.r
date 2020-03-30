context("aux")
test_that("aux:",{
    # Farg
    ll <- list(arg1=c(1,2,3), arg2=c(4,5,6))
    expect_equal(.farg(ll, arg3=c(7,8,9)),
                 list(arg1=c(1,2,3), arg2=c(4,5,6), arg3=c(7,8,9)))
    expect_equal(.farg(ll, arg1=c(1,4,3)),
                 list(arg1=c(1,2,3), arg2=c(4,5,6)))
    # Garg
    ll <- list(arg1=list(1,2,3), arg2=list(4,5,6))
    expect_equal(.garg(ll, 'arg1'), 1)
    expect_equal(.garg(ll, 'arg1',2), 2)
    expect_equal(.garg(ll, 'arg2',3), 6)
    expect_equal(.garg(ll, 'arg3'), ll$arg3)
    # Slice
    ll <- list(arg1=list(c(1,2,3), c(2,3,4), c(3,4,5)),
               arg2=list('a', 'b', 'c'))
    expect_equal(.slice.run(ll, 1), list(arg1=c(1,2,3), arg2='a'))
    expect_equal(.slice.run(ll, 2), list(arg1=c(2,3,4), arg2='b'))
    expect_equal(.slice.run(ll, 3), list(arg1=c(3,4,5), arg2='c'))
    
    ll <- list(arg1=list(c(1,2,3), c(2,3,4), c(3,4,5)),
               arg2=c('a', 'b', 'c'))
    expect_equal(.slice.run(ll, 1), list(arg1=c(1,2,3), arg2=c('a', 'b', 'c')))
    expect_equal(.slice.run(ll, 2), list(arg1=c(2,3,4), arg2=c('a', 'b', 'c')))
    expect_equal(.slice.run(ll, 3), list(arg1=c(3,4,5), arg2=c('a', 'b', 'c')))
    # .select.args
    actual <- ROCR:::.select.args(ll, "arg1")
    expect_equal(actual,ll["arg1"])
    actual <- ROCR:::.select.args(ll, "arg1", complement = TRUE)
    expect_equal(actual,ll["arg2"])
    # .construct.linefunct
    actual <- ROCR:::.construct.linefunct(1,2,3,4)
    expect_type(actual, "closure")
    expect_error(ROCR:::.construct.linefunct(1,2,1,4),
                 "Cannot construct a function from data.")
    # .intersection.point
    f <-  ROCR:::.construct.linefunct(1,2,3,4)
    g <-  ROCR:::.construct.linefunct(2,3,4,5)
    actual <- ROCR:::.intersection.point(f,g)
    expect_equal(actual, c(Inf,Inf))
    g <-  ROCR:::.construct.linefunct(2,3,1,5)
    actual <-  ROCR:::.intersection.point(f,g)
    expect_equal(actual, c(2,3))
})
