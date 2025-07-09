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

testthat::test_that(
    "Check deterministic content for the simulation of the models",
    {
        X <- 1:10
        Z <- 1:10

        # Linear model
        model <- paramrel::linear(1:2, sd = 0)

        tst <- paramrel::simulate(model, X = X)
        ref <- data.frame(
            time = 1:10,
            y = 1 + 2 * 1:10,
            x = 1:10,
            z = 0
        )

        testthat::expect_equal(tst, ref)



        # Quadratic model
        model <- paramrel::quadratic(1:3, sd = 0)

        tst <- paramrel::simulate(model, X = X)
        ref <- data.frame(
            time = 1:10,
            y = 1 + 2 * 1:10 + 3 * (1:10)^2,
            x = 1:10,
            z = 0
        )

        testthat::expect_equal(tst, ref)



        # Cubic model
        model <- paramrel::cubic(1:4, sd = 0)

        tst <- paramrel::simulate(model, X = X)
        ref <- data.frame(
            time = 1:10,
            y = 1 + 2 * 1:10 + 3 * (1:10)^2 + 4 * (1:10)^3,
            x = 1:10,
            z = 0
        )

        testthat::expect_equal(tst, ref)



        # Main effects model
        model <- paramrel::main_effect(1:3, sd = 0)

        tst <- paramrel::simulate(model, X = cbind(X, Z))
        ref <- data.frame(
            time = 1:10,
            y = 1 + 2 * 1:10 + 3 * 1:10,
            x = 1:10,
            z = 1:10
        )

        testthat::expect_equal(tst, ref)



        # Interaction model
        model <- paramrel::interaction(1:4, sd = 0)

        tst <- paramrel::simulate(model, X = cbind(X, Z))
        ref <- data.frame(
            time = 1:10,
            y = 1 + 2 * 1:10 + 3 * 1:10 + 4 * 1:10 * 1:10,
            x = 1:10,
            z = 1:10
        )

        testthat::expect_equal(tst, ref)



        # AR(1) model
        model <- paramrel::ar1(c(1, 0.9), sd = 0)

        tst <- paramrel::simulate(model, X = X)
        ref <- data.frame(
            time = 1:10,
            y = rep(1/0.1, each = 10),
            x = 1:10,
            z = 0
        )

        testthat::expect_equal(tst, ref)



        # Commented out: Most difficult model to test deterministically, needs
        # significantly more code
        #
        # # ARX model
        # model <- paramrel::arx(c(1, 0.9, 2), sd = 0)

        # tst <- paramrel::simulate(model, X = X)
        # ref <- data.frame(
        #     time = 1:10,
        #     y = rep(1/0.1, each = 10),
        #     x = 1:10,
        #     z = 0
        # )

        # testthat::expect_equal(tst, ref)
    }        
)

testthat::test_that(
    "Check stochastic content for the simulation of the models",
    {
        X <- rep(1, 100000)
        Z <- rep(1, 100000)

        # Linear model
        model <- paramrel::linear(1:2, sd = 1)
        tst <- paramrel::simulate(model, X = X)
        
        testthat::expect_equal(mean(tst$y), sum(1:2), tolerance = 1e-2)
        testthat::expect_equal(sd(tst$y), 1, tolerance = 1e-2)



        # Quadratic model
        model <- paramrel::quadratic(1:3, sd = 1)
        tst <- paramrel::simulate(model, X = X)
        
        testthat::expect_equal(mean(tst$y), sum(1:3), tolerance = 1e-2)
        testthat::expect_equal(sd(tst$y), 1, tolerance = 1e-2)



        # Cubic model
        model <- paramrel::cubic(1:4, sd = 1)
        tst <- paramrel::simulate(model, X = X)
        
        testthat::expect_equal(mean(tst$y), sum(1:4), tolerance = 1e-2)
        testthat::expect_equal(sd(tst$y), 1, tolerance = 1e-2)



        # Main effects model
        model <- paramrel::main_effect(1:3, sd = 1)
        tst <- paramrel::simulate(model, X = cbind(X, Z))
        
        testthat::expect_equal(mean(tst$y), sum(1:3), tolerance = 1e-2)
        testthat::expect_equal(sd(tst$y), 1, tolerance = 1e-2)



        # Interaction model
        model <- paramrel::interaction(1:4, sd = 1)
        tst <- paramrel::simulate(model, X = cbind(X, Z))
        
        testthat::expect_equal(mean(tst$y), sum(1:4), tolerance = 1e-2)
        testthat::expect_equal(sd(tst$y), 1, tolerance = 1e-2)



        # Commented out: Most difficult model to test deterministically, needs
        # significantly more code
        #
        # # AR(1) model
        # model <- paramrel::ar1(c(1, 0.9), sd = 0)

        # tst <- paramrel::simulate(model, X = X)
        # ref <- data.frame(
        #     time = 1:10,
        #     y = rep(1/0.1, each = 10),
        #     x = 1:10,
        #     z = 0
        # )

        # testthat::expect_equal(tst, ref)



        # Commented out: Most difficult model to test deterministically, needs
        # significantly more code
        #
        # # ARX model
        # model <- paramrel::arx(c(1, 0.9, 2), sd = 0)

        # tst <- paramrel::simulate(model, X = X)
        # ref <- data.frame(
        #     time = 1:10,
        #     y = rep(1/0.1, each = 10),
        #     x = 1:10,
        #     z = 0
        # )

        # testthat::expect_equal(tst, ref)
    }        
)
