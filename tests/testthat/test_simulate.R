testthat::test_that(
    "Check whether the single_x works, and with it simulate_x for one IV",
    {
        # More than one function has been provided
        testthat::expect_warning(
            paramrel:::single_x(
                paramrel::linear(),
                Xfun = list(\(x) rnorm(x), \(x) rnorm(x))
            )
        )

        # Result of the function is a matrix
        testthat::expect_warning(
            paramrel:::single_x(
                paramrel::linear(),
                Xfun = \(x) matrix(0, nrow = x, ncol = 2)
            )
        )

        # Content correct when using the function as intended
        ref <- matrix(1, nrow = 10, ncol = 1)
        tst <- paramrel:::single_x(
            paramrel::linear(),
            Xfun = \(x) rep(1, each = x),
            N = 10
        )

        testthat::expect_equal(tst, ref)

        # Content correct when providing multiple functions
        ref <- matrix(1, nrow = 10, ncol = 1)
        tst <- paramrel:::single_x(
            paramrel::linear(),
            Xfun = list(
                \(x) rep(1, each = x),
                \(x) rep(2, each = x)
            ),
            N = 10
        ) |>
            suppressWarnings()

        testthat::expect_equal(tst, ref)

        # Content correct when providing function with matrix output
        ref <- matrix(1, nrow = 10, ncol = 1)
        tst <- paramrel:::single_x(
            paramrel::linear(),
            Xfun = \(x) matrix(1, nrow = x, ncol = 2),
            N = 10
        ) |>
            suppressWarnings()

        testthat::expect_equal(tst, ref)
    }
)

testthat::test_that(
    "Check whether the double_x works, and with it simulate_x for two IVs",
    {
        # Less than two functions have been provided
        testthat::expect_error(
            paramrel:::double_x(
                paramrel::linear(),
                Xfun = list()
            )
        )

        # More than two functions have been provided
        testthat::expect_warning(
            paramrel:::single_x(
                paramrel::linear(),
                Xfun = list(\(x) rnorm(x), \(x) rnorm(x), \(x) rnorm(x))
            )
        )

        # A single function has been provided
        testthat::expect_warning(
            paramrel:::double_x(
                paramrel::linear(),
                Xfun = \(x) rnorm(x)
            )
        )
        testthat::expect_warning(
            paramrel:::double_x(
                paramrel::linear(),
                Xfun = list(\(x) rnorm(x))
            )
        )

        # Output of the functions is a matrix
        testthat::expect_warning(
            paramrel:::double_x(
                paramrel::linear(),
                Xfun = list(
                    \(x) matrix(1, nrow = x, ncol = 2),
                    \(x) matrix(2, nrow = x, ncol = 2)
                )
            )
        )

        # Content correct when using the function as intended
        ref <- cbind(
            rep(1, each = 10), 
            rep(2, each = 10)
        )
        tst <- paramrel:::double_x(
            paramrel::linear(),
            Xfun = list(
                \(x) rep(1, each = x),
                \(x) rep(2, each = x)
            ),
            N = 10
        )

        testthat::expect_equal(tst, ref)

        # Content correct when providing more than 2 functions
        ref <- cbind(
            rep(1, each = 10), 
            rep(2, each = 10)
        )
        tst <- paramrel:::double_x(
            paramrel::linear(),
            Xfun = list(
                \(x) rep(1, each = x),
                \(x) rep(2, each = x),
                \(x) rep(3, each = x)
            ),
            N = 10
        ) |>
            suppressWarnings()

        testthat::expect_equal(tst, ref)

        # Content correct when providing only 1 function
        ref <- matrix(1, nrow = 10, ncol = 2)
        tst <- paramrel:::double_x(
            paramrel::linear(),
            Xfun = \(x) rep(1, each = x),
            N = 10
        ) |>
            suppressWarnings()

        testthat::expect_equal(tst, ref)

        
        tst <- paramrel:::double_x(
            paramrel::linear(),
            Xfun = list(\(x) rep(1, each = x)),
            N = 10
        ) |>
            suppressWarnings()

        testthat::expect_equal(tst, ref)

        # Content correct when providing function with matrix output
        ref <- cbind(
            rep(1, each = 10),
            rep(2, each = 10)
        )
        tst <- paramrel:::double_x(
            paramrel::linear(),
            Xfun = list(
                \(x) matrix(1, nrow = x, ncol = 2),
                \(x) matrix(2, nrow = x, ncol = 2)
            ),
            N = 10
        ) |>
            suppressWarnings()

        testthat::expect_equal(tst, ref)
    }
)
