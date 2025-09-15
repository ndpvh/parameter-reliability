testthat::test_that(
    "Check the output of parameter_grid",
    {
        # Make sequences of the parameter values
        params_1 <- seq(-10, 10, length.out = 5)
        params_2 <- seq(-5, 5, length.out = 5)
        params_3 <- seq(-0.99, 0.99, length.out = 5)

        # Make different types of references, based on the number of parameters
        ref_2 <- cbind(
            rep(params_1, each = 5),
            rep(params_2, times = 5)
        )
        ref_3 <- cbind(
            rep(params_1, each = 5^2),
            rep(rep(params_2, each = 5), times = 5),
            rep(params_2, times = 5^2)
        )
        ref_4 <- cbind(
            rep(params_1, each = 5^3),
            rep(rep(params_2, each = 5^2), times = 5),
            rep(rep(params_2, each = 5), times = 5^2),
            rep(params_2, times = 5^3)
        )

        ref_ar1 <- cbind(
            rep(params_1, each = 5),
            rep(params_3, times = 5)
        )
        ref_arx <- cbind(
            rep(params_1, each = 5^2),
            rep(rep(params_3, each = 5), times = 5),
            rep(params_2, times = 5^2)
        )



        # Test for linear model        
        tst <- paramrel::parameter_grid(paramrel::linear())
        testthat::expect_equal(tst, ref_2)

        # Test for quadratic model        
        tst <- paramrel::parameter_grid(paramrel::quadratic())
        testthat::expect_equal(tst, ref_3)

        # Test for cubic model        
        tst <- paramrel::parameter_grid(paramrel::cubic())
        testthat::expect_equal(tst, ref_4)

        # Test for main_effect model
        tst <- paramrel::parameter_grid(paramrel::main_effect())
        testthat::expect_equal(tst, ref_3)

        # Test for interaction model
        tst <- paramrel::parameter_grid(paramrel::interaction())
        testthat::expect_equal(tst, ref_4)

        # Test for ar1 model
        tst <- paramrel::parameter_grid(paramrel::ar1())
        testthat::expect_equal(tst, ref_ar1)

        # Test for arx model
        tst <- paramrel::parameter_grid(paramrel::arx())
        testthat::expect_equal(tst, ref_arx)
    }
)

testthat::test_that(
    "Check the output of parameter_grid when changing parameter bounds",
    {
        # Create new intercepts and slopes
        intercept <- c(0, 5)
        slope <- c(-3, 0)

        # Make sequences of the parameter values
        params_1 <- seq(intercept[1], intercept[2], length.out = 5)
        params_2 <- seq(slope[1], slope[2], length.out = 5)
        params_3 <- seq(-0.99, 0.99, length.out = 5)

        # Make different types of references, based on the number of parameters
        ref_2 <- cbind(
            rep(params_1, each = 5),
            rep(params_2, times = 5)
        )
        ref_3 <- cbind(
            rep(params_1, each = 5^2),
            rep(rep(params_2, each = 5), times = 5),
            rep(params_2, times = 5^2)
        )
        ref_4 <- cbind(
            rep(params_1, each = 5^3),
            rep(rep(params_2, each = 5^2), times = 5),
            rep(rep(params_2, each = 5), times = 5^2),
            rep(params_2, times = 5^3)
        )

        ref_ar1 <- cbind(
            rep(params_1, each = 5),
            rep(params_3, times = 5)
        )
        ref_arx <- cbind(
            rep(params_1, each = 5^2),
            rep(rep(params_3, each = 5), times = 5),
            rep(params_2, times = 5^2)
        )



        # Test for linear model        
        tst <- paramrel::parameter_grid(
            paramrel::linear(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_2)

        # Test for quadratic model        
        tst <- paramrel::parameter_grid(
            paramrel::quadratic(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_3)

        # Test for cubic model        
        tst <- paramrel::parameter_grid(
            paramrel::cubic(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_4)

        # Test for main_effect model
        tst <- paramrel::parameter_grid(
            paramrel::main_effect(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_3)

        # Test for interaction model
        tst <- paramrel::parameter_grid(
            paramrel::interaction(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_4)

        # Test for ar1 model
        tst <- paramrel::parameter_grid(
            paramrel::ar1(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_ar1)

        # Test for arx model
        tst <- paramrel::parameter_grid(
            paramrel::arx(),
            intercept = intercept, 
            slope = slope
        )
        testthat::expect_equal(tst, ref_arx)
    }
)

testthat::test_that(
    "Check whether generate_parameters works",
    {
        # Use the function with a fixed seed
        set.seed(1)
        params <- paramrel::generate_parameters(
            100000,
            c(4, -1, 3),
            1
        )

        # Do some tests on the structure of the output
        testthat::expect_equal(nrow(params), 100000)
        testthat::expect_equal(ncol(params), 3)

        # Do some tests on the content of the output
        testthat::expect_equal(mean(params[, 1]), 4, tolerance = 1e-2)
        testthat::expect_equal(mean(params[, 2]), -1, tolerance = 1e-2)
        testthat::expect_equal(mean(params[, 3]), 3, tolerance = 1e-2)

        testthat::expect_equal(sd(params[, 1]), 1, tolerance = 1e-2)
        testthat::expect_equal(sd(params[, 2]), 1, tolerance = 1e-2)
        testthat::expect_equal(sd(params[, 3]), 1, tolerance = 1e-2)
    }
)
