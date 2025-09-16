testthat::test_that(
    "Check whether estimated and true parameter values are prepared correctly",
    {
        # Create the test cases
        est_1 <- data.frame(
            participant = rep(1, 10),
            bin = 1:10,
            param_1 = 1:10,
            param_2 = 10:1,
            se_param_1 = rep(0.1, 10),
            se_param_2 = rep(0.2, 10)
        )
        est_2 <- est_1 |>
            dplyr::mutate(
                param_3 = 10 + param_1,
                se_param_3 = 0.2 + se_param_1
            )

        sim_1 <- data.frame(
            param_1 = 2 * est_1$param_1, 
            param_2 = 2 * est_1$param_2
        )
        sim_2 <- data.frame(
            param_1 = 2 * est_2$param_1, 
            param_2 = 2 * est_2$param_2,
            param_3 = 2 * est_2$param_3
        )



        # Case 1: Perfect matchup between the different data.frames
        prepared <- paramrel::prepare(
            est_1, 
            sim_1
        )

        testthat::expect_equal(prepared$parameters, c("param_1", "param_2"))
        testthat::expect_equal(prepared$estimated, est_1)
        testthat::expect_equal(prepared$simulated, sim_1)



        prepared <- paramrel::prepare(
            est_2, 
            sim_2
        )

        testthat::expect_equal(prepared$parameters, c("param_1", "param_2", "param_3"))
        testthat::expect_equal(prepared$estimated, est_2)
        testthat::expect_equal(prepared$simulated, sim_2)



        # Case 2: More estimated parameters than true ones
        prepared <- paramrel::prepare(
            est_2, 
            sim_1
        )

        ref <- sim_1
        ref$param_3 <- 0

        testthat::expect_equal(prepared$parameters, c("param_1", "param_2", "param_3"))
        testthat::expect_equal(prepared$estimated, est_2)
        testthat::expect_equal(prepared$simulated, ref)
        


        # Case 3: More true parameters than estimated ones
        prepared <- paramrel::prepare(
            est_1, 
            sim_2
        )

        ref <- est_1
        ref[, c("param_3", "se_param_3")] <- 0

        testthat::expect_equal(prepared$parameters, c("param_1", "param_2", "param_3"))
        testthat::expect_equal(prepared$estimated, ref)
        testthat::expect_equal(prepared$simulated, sim_2)
    }
)

testthat::test_that(
    "Check whether the computation of the descriptives is done correctly",
    {
        # Create a reference dataframe for which we know the descriptives
        est <- data.frame(
            participant = rep(1:2, each = 5),
            bin = rep(1:5, times = 2),
            param_1 = 1:10, 
            param_2 = 2 * 1:10,
            se_param_1 = rep(c(0.1, 0.2), each = 5),
            se_param_2 = rep(c(0.3, 0.4), each = 5)
        )
        sim <- data.frame(
            param_1 = c(3, 7),
            param_2 = 2 * c(3, 7)
        )

        # Create an analytic reference
        ref <- data.frame(
            parameter = rep(c("param_1", "param_2"), each = 5),
            bin = rep(1:5, times = 2),
            true_value = rep(c(5, 10), each = 5),
            mean = c(2.5 + 1:5, (2.5 + 1:5) * 2),
            sd = rep(c(sd(c(1, 6)), sd(c(2 * c(1, 6)))), each = 5),
            q025 = c(0.125 + 1:5, 0.25 + 2 * (1:5)),
            median = c(2.5 + 1:5, (2.5 + 1:5) * 2),
            q975 = c(6:10 - 0.125, 2 * (6:10) - 0.25),
            mean_se = rep(c(0.15, 0.35), each = 5),
            sd_se = rep(sd(c(0.1, 0.2)), each = 10),
            q025_se = rep(0.0025 + c(0.1, 0.3), each = 5),
            median_se = rep(c(0.15, 0.35), each = 5),
            q975_se = rep(c(0.2, 0.4) - 0.0025, each = 5)
        )

        # Use the descriptives function
        tst <- paramrel::descriptives(
            list(
                "parameters" = c("param_1", "param_2"),
                "estimated" = est, 
                "simulated" = sim
            )
        )

        # Do the test
        testthat::expect_equal(tst, ref)
    }
)

testthat::test_that(
    "Check whether the computation of the ICC is done correctly",
    {
        N <- 1000
        n_bin <- 1000

        sd_between = 1
        sd_within = 0.5

        # Create a reference of the parameters so that we can accurately compute
        # the ICC
        set.seed(1)
        param_1 <- rnorm(
            N, 
            -2, 
            sd = sd_between
        )
        param_2 <- rnorm(
            N, 
            2, 
            sd = sd_between
        )

        param_1 <- rnorm(
            N * n_bin, 
            rep(param_1, each = n_bin),
            sd = sd_within
        )
        param_2 <- rnorm(
            N * n_bin, 
            rep(param_2, each = n_bin),
            sd = sd_within
        )

        # Create the "estimated parameters" data.frame
        est <- data.frame(
            participant = rep(1:N, each = n_bin),
            bin = rep(1:n_bin, times = N),
            param_1 = param_1, 
            param_2 = param_2,
            se_param_1 = rep(0, each = N * n_bin),
            se_param_2 = rep(0, each = N * n_bin)
        )

        # Use the icc function and compute the reference ICC
        tst <- paramrel::icc(
            list(
                "parameters" = c("param_1", "param_2"),
                "estimated" = est, 
                "simulated" = data.frame()
            )
        )
        ref_icc <- sd_between^2 / (sd_between^2 + sd_within^2)

        # Do the test 
        testthat::expect_true(all(abs(tst$systematic - sd_between^2) < 0.1))
        testthat::expect_true(all(abs(tst$residual - sd_within^2) < 0.1))
        testthat::expect_true(all(abs(tst$icc - ref_icc) < 0.1))
    }
)

testthat::test_that(
    "Check whether the computation of the signal-based metrics is done correctly",
    {
        N <- 1000

        mean_1 <- -2
        mean_2 <- 2
        sd_between <- 1
        sd_within <- 0.5

        # Create a reference of the generating parameters 
        set.seed(1)
        param_1 <- rnorm(
            N, 
            mean_1, 
            sd = sd_between
        )
        param_2 <- rnorm(
            N, 
            mean_2, 
            sd = sd_between
        )
        
        sim <- data.frame(
            param_1 = param_1, 
            param_2 = param_2
        )


        # Create the reference for the estimated parameters
        param_1 <- rnorm(
            N, 
            param_1,
            sd = sd_within
        )
        param_2 <- rnorm(
            N, 
            param_2,
            sd = sd_within
        )
        
        est <- data.frame(
            participant = 1:N,
            bin = rep(-1, times = N),
            param_1 = param_1, 
            param_2 = param_2,
            se_param_1 = rep(0, each = N),
            se_param_2 = rep(0, each = N)
        )

        # Use the signal function and compute the reference CV and SNR. 
        # TO DO: Find way of getting to the SNR without using the same computation
        # as in the function itself
        tst <- paramrel::signal(
            list(
                "parameters" = c("param_1", "param_2"),
                "estimated" = est, 
                "simulated" = sim
            )
        )
        ref_cv <- c(
            (sd_between^2 + sd_within^2) / abs(mean_1),
            (sd_between^2 + sd_within^2) / abs(mean_2)
        )
        ref_snr <- c(
            mean(sim$param_1^2) / mean((sim$param_1 - est$param_1)^2),
            mean(sim$param_2^2) / mean((sim$param_2 - est$param_2)^2)
        )

        # Do the test 
        testthat::expect_true(all(abs(tst$coefficient_variation - ref_cv) < 0.1))
        testthat::expect_true(all(abs(tst$signal_to_noise - ref_snr) < 0.1))
    }
)

testthat::test_that(
    "Check whether the computation of the accuracy metrics is done correctly",
    {
        N <- 1000
        sd_between <- 1

        # Create the simulated and estimated parameters
        sim <- data.frame(
            param_1 = 1:N,
            param_2 = 1:N,
            param_3 = 1:N
        )
        est <- data.frame(
            participant = 1:N,
            param_1 = rnorm(
                N, 
                sim$param_1 - 2, 
                sd = sd_between
            ),
            param_2 = rnorm(
                N, 
                sim$param_2, 
                sd = sd_between
            ),
            param_3 = rnorm(
                N, 
                sim$param_3 + 2, 
                sd = sd_between
            ),
            se_param_1 = rep(1, each = N), 
            se_param_2 = rep(1, each = N), 
            se_param_3 = rep(1, each = N)
        )

        # Use the accuracy function and compute the metrics for the accuracy of 
        # the estimation
        tst <- paramrel::accuracy(
            list(
                "parameters" = c("param_1", "param_2", "param_3"),
                "estimated" = est, 
                "simulated" = sim
            )
        )

        # Make the different references; TO DO, add references for MSE and coverage
        true <- rep(500.5, 3)
        estimated <- true + c(-2, 0, 2)
        se <- rep(1, 3)
        bias_directed <- c(-2, 0, 2)
        bias_undirected <- c(2, 0, 2)
        bias_relative <- c(-2, 0, 2) / sum(sim$param_1)
        bias_relative_undirected <- c(2, 0, 2) / sum(sim$param_1)
        
        # Do the test 
        testthat::expect_true(all(abs(tst$true_value - true) < 0.1))
        testthat::expect_true(all(abs(tst$estimated_value - estimated) < 0.1))
        testthat::expect_true(all(abs(tst$standard_error - se) < 0.1))
        testthat::expect_true(all(abs(tst$bias_directed - bias_directed) < 0.1))
        testthat::expect_true(all(abs(tst$bias_undirected - bias_undirected) < 0.1))
        testthat::expect_true(all(abs(tst$bias_relative - bias_relative) < 0.1))
        testthat::expect_true(all(abs(tst$bias_relative_undirected - bias_relative_undirected) < 0.1))
    }
)
