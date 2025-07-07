# Errors
testthat::test_that(
    "Check known errors: Too few parameters",
    {
        params <- rep(1, 1)

        testthat::expect_error(paramrel::linear(params))
        testthat::expect_error(paramrel::quadratic(params))
        testthat::expect_error(paramrel::cubic(params))
        testthat::expect_error(paramrel::main_effect(params))
        testthat::expect_error(paramrel::interaction(params))
        testthat::expect_error(paramrel::ar1(params))
        testthat::expect_error(paramrel::arx(params))
    }
)

# Warnings
testthat::test_that(
    "Check known warnings: Too many parameters",
    {
        params <- rep(1, 10)

        testthat::expect_warning(paramrel::linear(params))
        testthat::expect_warning(paramrel::quadratic(params))
        testthat::expect_warning(paramrel::cubic(params))
        testthat::expect_warning(paramrel::main_effect(params))
        testthat::expect_warning(paramrel::interaction(params))
        testthat::expect_warning(paramrel::ar1(params))
        testthat::expect_warning(paramrel::arx(params))
    }
)

# Warnings
testthat::test_that(
    "Check known warnings: Too many standard deviations",
    {
        params <- 1:10
        sd <- rep(1, 10)

        testthat::expect_warning(paramrel::linear(params[1:2], sd = sd))
        testthat::expect_warning(paramrel::quadratic(params[1:3], sd = sd))
        testthat::expect_warning(paramrel::cubic(params[1:4], sd = sd))
        testthat::expect_warning(paramrel::main_effect(params[1:3], sd = sd))
        testthat::expect_warning(paramrel::interaction(params[1:4], sd = sd))
        testthat::expect_warning(paramrel::ar1(params[1:2], sd = sd))
        testthat::expect_warning(paramrel::arx(params[1:3], sd = sd))
    }
)

# Output
testthat::test_that(
    "Check output of initialization",
    {
        # Correct number of provided parameters
        ref <- rep(1, 2)
        tst <- paramrel::linear(ref)
        testthat::expect_equal(tst@parameters, ref)

        ref <- rep(1, 3)
        tst <- paramrel::quadratic(ref)
        testthat::expect_equal(tst@parameters, ref)

        ref <- rep(1, 4)
        tst <- paramrel::cubic(ref)
        testthat::expect_equal(tst@parameters, ref)

        ref <- rep(1, 3)
        tst <- paramrel::main_effect(ref)
        testthat::expect_equal(tst@parameters, ref)

        ref <- rep(1, 4)
        tst <- paramrel::interaction(ref)
        testthat::expect_equal(tst@parameters, ref)

        ref <- rep(1, 2)
        tst <- paramrel::ar1(ref)
        testthat::expect_equal(tst@parameters, ref)

        ref <- rep(1, 3)
        tst <- paramrel::arx(ref)
        testthat::expect_equal(tst@parameters, ref)



        # Too many parameters provided
        params <- 1:10

        ref <- params[1:2]
        tst <- paramrel::linear(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)

        ref <- params[1:3]
        tst <- paramrel::quadratic(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)

        ref <- params[1:4]
        tst <- paramrel::cubic(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)

        ref <- params[1:3]
        tst <- paramrel::main_effect(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)

        ref <- params[1:4]
        tst <- paramrel::interaction(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)

        ref <- params[1:2]
        tst <- paramrel::ar1(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)

        ref <- params[1:3]
        tst <- paramrel::arx(params) %>%
            suppressWarnings()
        testthat::expect_equal(tst@parameters, ref)



        # Too many standard deviations provided
        sd <- 1:10
        ref <- 1

        tst <- paramrel::linear(params[1:2], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)

        tst <- paramrel::quadratic(params[1:3], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)

        tst <- paramrel::cubic(params[1:3], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)

        tst <- paramrel::main_effect(params[1:3], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)

        tst <- paramrel::interaction(params[1:4], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)

        tst <- paramrel::ar1(params[1:2], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)

        tst <- paramrel::arx(params[1:3], sd = sd) %>%
            suppressWarnings()
        testthat::expect_equal(tst@sd, ref)
    }
)