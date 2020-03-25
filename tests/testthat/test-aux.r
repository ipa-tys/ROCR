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
})
