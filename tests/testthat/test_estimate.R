testthat::test_that(
    "Check known errors",
    {
        data <- data.frame(
            T = numeric(10),
            X = numeric(10),
            Y = numeric(10),
            Z = numeric(10)
        )

        testthat::expect_error(
            paramrel::estimate(
                paramrel::linear(),
                data
            )
        )
        testthat::expect_error(
            paramrel::estimate(
                paramrel::quadratic(),
                data
            )
        )
        testthat::expect_error(
            paramrel::estimate(
                paramrel::cubic(),
                data
            )
        )
        testthat::expect_error(
            paramrel::estimate(
                paramrel::main_effect(),
                data
            )
        )
        testthat::expect_error(
            paramrel::estimate(
                paramrel::interaction(),
                data
            )
        )
        testthat::expect_error(
            paramrel::estimate(
                paramrel::ar1(),
                data
            )
        )
        testthat::expect_error(
            paramrel::estimate(
                paramrel::arx(),
                data
            )
        )
    }
)

testthat::test_that(
    "Check whether estimation works through recovery",
    {
        set.seed(1)
        N <- 50

        # Create a recovery function that will conduct our test
        recover <- function(fx, parameters) {
            # Simulate data using the parameters provided
            model <- fx(parameters, sd = 0.1) |>
                suppressWarnings()

            data <- simulate(model, N = 200000)

            # Estimate the parameters back
            estimates <- estimate(model, data)

            # Extract the parameters and subtract from the simulating ones
            return(abs(model@parameters - estimates$model@parameters))
        }


        
        ########################################################################
        # POLYNOMIAL MODELS

        # Create a bunch of parameters for the polynomial models
        params <- matrix(
            runif(N * 5, min = -10, max = 10),
            nrow = N, 
            ncol = 5
        )

        # Do the recovery for the polynomial models
        result <- matrix(FALSE, nrow = N, ncol = 5)
        for(i in 1:N) {
            # Linear model
            result[i, 1] <- all(
                recover(paramrel::linear, params[i, ]) < 1e-1
            )

            # Quadratic model
            result[i, 2] <- all(
                recover(paramrel::quadratic, params[i, ]) < 1e-1
            )

            # Cubic model
            result[i, 3] <- all(
                recover(paramrel::cubic, params[i, ]) < 1e-1
            )

            # Main effect model
            result[i, 4] <- all(
                recover(paramrel::main_effect, params[i, ]) < 1e-1
            )

            # Interaction model
            result[i, 5] <- all(
                recover(paramrel::interaction, params[i, ]) < 1e-1
            )
        }

        testthat::expect_true(all(result))

        
        
        ########################################################################
        # AUTOREGRESSIVE MODELS

        # Create a bunch of parameters for the polynomial models
        params <- matrix(
            runif(N * 3, min = -10, max = 10),
            nrow = N, 
            ncol = 3
        )
        params[, 2] <- runif(N, min = -0.99, max = 0.99)

        # Do the recovery for the polynomial models
        result <- matrix(FALSE, nrow = N, ncol = 2)
        for(i in 1:N) {
            # AR(1) model
            result[i, 1] <- all(
                recover(paramrel::ar1, params[i, ]) < 1e-1
            )

            # ARX model
            result[i, 2] <- all(
                recover(paramrel::arx, params[i, ]) < 1e-1
            )
        }

        testthat::expect_true(all(result))
    }
)