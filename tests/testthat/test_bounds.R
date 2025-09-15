testthat::test_that(
    "Check output of bounds method under defaults",
    {
        testthat::expect_equal(
            paramrel::bounds(paramrel::linear()),
            matrix(c(-10, -5, 10, 5), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(paramrel::quadratic()),
            matrix(c(-10, -5, -5, 10, 5, 5), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(paramrel::cubic()),
            matrix(c(-10, -5, -5, -5, 10, 5, 5, 5), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(paramrel::main_effect()),
            matrix(c(-10, -5, -5, 10, 5, 5), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(paramrel::interaction()),
            matrix(c(-10, -5, -5, -5, 10, 5, 5, 5), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(paramrel::ar1()),
            matrix(c(-10, -0.99, 10, 0.99), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(paramrel::arx()),
            matrix(c(-10, -0.99, -5, 10, 0.99, 5), ncol = 2)
        )
    }
)

testthat::test_that(
    "Check output when specifying different bounds",
    {
        intercept <- c(0, 5)
        slope <- c(-2, 0)

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::linear(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -2, 5, 0), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::quadratic(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -2, -2, 5, 0, 0), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::cubic(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -2, -2, -2, 5, 0, 0, 0), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::main_effect(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -2, -2, 5, 0, 0), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::interaction(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -2, -2, -2, 5, 0, 0, 0), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::ar1(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -0.99, 5, 0.99), ncol = 2)
        )

        testthat::expect_equal(
            paramrel::bounds(
                paramrel::arx(),
                intercept = intercept, 
                slope = slope
            ),
            matrix(c(0, -0.99, -2, 5, 0.99, 0), ncol = 2)
        )
    }
)
