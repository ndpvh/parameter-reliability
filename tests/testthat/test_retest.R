testthat::test_that(
    "Check the output of parameter_grid",
    {
        # Make sequences of the parameter values
        params_1 <- seq(-10, 10, length.out = 5)
        params_2 <- seq(-5, 5, length.out = 5)

        # Test for linear model
        ref <- cbind(
            rep(params_1, each = 5),
            rep(params_2, times = 5)
        )
        tst <- paramrel::parameter_grid(paramrel::linear())

        testthat::expect_equal(tst, ref)
    }
)