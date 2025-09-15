# Purpose: Model fitting functions for parameter reliability study
# Authors: Kenny Yu & Niels Vanhasbroeck  
# Date: July 2025

# Fit Linear Model
fit_linear <- function(data) {
  
  if (!all(c("y", "x") %in% names(data))) {
    stop("Data must contain columns 'y' and 'x'")
  }
  
  complete_cases <- complete.cases(data[, c("y", "x")])
  if(sum(complete_cases) < 3) {
    stop("Insufficient complete cases for linear model fitting")
  }
  clean_data <- data[complete_cases, ]
  
  model_fit <- tryCatch({
    lm(y ~ x, data = clean_data)
  }, error = function(e) {
    stop("Linear model fitting failed: ", e$message)
  })
  
  model_summary <- summary(model_fit)
  
  coeffs <- coef(model_fit)
  se_vals <- model_summary$coefficients[, "Std. Error"]
  
  # Ensure consistent naming
  names(coeffs) <- names(se_vals) <- c("intercept", "x")
  
  list(
    model = model_fit,
    coefficients = coeffs,
    se = se_vals,
    fitted_values = fitted(model_fit),
    residuals = residuals(model_fit),
    r_squared = model_summary$r.squared,
    n_obs = nrow(clean_data)
  )
}

# Fit Quadratic Model
fit_quadratic <- function(data) {
  
  if (!all(c("y", "x") %in% names(data))) {
    stop("Data must contain columns 'y' and 'x'")
  }
  
  complete_cases <- complete.cases(data[, c("y", "x")])
  if(sum(complete_cases) < 4) {
    stop("Insufficient complete cases for quadratic model fitting")
  }
  clean_data <- data[complete_cases, ]
  
  model_fit <- tryCatch({
    lm(y ~ x + I(x^2), data = clean_data)
  }, error = function(e) {
    stop("Quadratic model fitting failed: ", e$message)
  })
  
  model_summary <- summary(model_fit)
  
  coeffs <- coef(model_fit)
  se_vals <- model_summary$coefficients[, "Std. Error"]
  
  # Ensure consistent naming
  names(coeffs) <- names(se_vals) <- c("intercept", "x", "x2")
  
  list(
    model = model_fit,
    coefficients = coeffs,
    se = se_vals,
    fitted_values = fitted(model_fit),
    residuals = residuals(model_fit),
    r_squared = model_summary$r.squared,
    n_obs = nrow(clean_data)
  )
}

# Fit Cubic Model
fit_cubic <- function(data) {
  
  if (!all(c("y", "x") %in% names(data))) {
    stop("Data must contain columns 'y' and 'x'")
  }
  
  complete_cases <- complete.cases(data[, c("y", "x")])
  if(sum(complete_cases) < 5) {
    stop("Insufficient complete cases for cubic model fitting")
  }
  clean_data <- data[complete_cases, ]
  
  model_fit <- tryCatch({
    lm(y ~ x + I(x^2) + I(x^3), data = clean_data)
  }, error = function(e) {
    stop("Cubic model fitting failed: ", e$message)
  })
  
  model_summary <- summary(model_fit)
  
  coeffs <- coef(model_fit)
  se_vals <- model_summary$coefficients[, "Std. Error"]
  
  # Ensure consistent naming
  names(coeffs) <- names(se_vals) <- c("intercept", "x", "x2", "x3")
  
  list(
    model = model_fit,
    coefficients = coeffs,
    se = se_vals,
    fitted_values = fitted(model_fit),
    residuals = residuals(model_fit),
    r_squared = model_summary$r.squared,
    n_obs = nrow(clean_data)
  )
}

# Fit Main Effects Model
fit_main <- function(data) {
  
  if (!all(c("y", "x", "z") %in% names(data))) {
    stop("Data must contain columns 'y', 'x', and 'z'")
  }
  
  complete_cases <- complete.cases(data[, c("y", "x", "z")])
  if(sum(complete_cases) < 4) {
    stop("Insufficient complete cases for main effects model fitting")
  }
  clean_data <- data[complete_cases, ]
  
  model_fit <- tryCatch({
    lm(y ~ x + z, data = clean_data)
  }, error = function(e) {
    stop("Main effects model fitting failed: ", e$message)
  })
  
  model_summary <- summary(model_fit)
  
  coeffs <- coef(model_fit)
  se_vals <- model_summary$coefficients[, "Std. Error"]
  
  # Ensure consistent naming
  names(coeffs) <- names(se_vals) <- c("intercept", "x", "z")
  
  list(
    model = model_fit,
    coefficients = coeffs,
    se = se_vals,
    fitted_values = fitted(model_fit),
    residuals = residuals(model_fit),
    r_squared = model_summary$r.squared,
    n_obs = nrow(clean_data)
  )
}

# Fit Interaction Model
fit_interaction <- function(data) {
  
  if (!all(c("y", "x", "z") %in% names(data))) {
    stop("Data must contain columns 'y', 'x', and 'z'")
  }
  
  complete_cases <- complete.cases(data[, c("y", "x", "z")])
  if(sum(complete_cases) < 5) {
    stop("Insufficient complete cases for interaction model fitting")
  }
  clean_data <- data[complete_cases, ]
  
  model_fit <- tryCatch({
    lm(y ~ x + z + x:z, data = clean_data)
  }, error = function(e) {
    stop("Interaction model fitting failed: ", e$message)
  })
  
  model_summary <- summary(model_fit)
  
  coeffs <- coef(model_fit)
  se_vals <- model_summary$coefficients[, "Std. Error"]
  
  # Ensure consistent naming
  names(coeffs) <- names(se_vals) <- c("intercept", "x", "z", "x_z")
  
  list(
    model = model_fit,
    coefficients = coeffs,
    se = se_vals,
    fitted_values = fitted(model_fit),
    residuals = residuals(model_fit),
    r_squared = model_summary$r.squared,
    n_obs = nrow(clean_data)
  )
}

# Fit AR(1) Model
fit_ar1 <- function(data) {
  
  if (!all(c("y", "x", "y_lag") %in% names(data))) {
    stop("Data must contain columns 'y', 'x', and 'y_lag'")
  }
  
  complete_cases <- complete.cases(data[, c("y", "x", "y_lag")])
  if(sum(complete_cases) < 5) {
    stop("Insufficient complete cases for AR(1) model fitting")
  }
  clean_data <- data[complete_cases, ]
  
  model_fit <- tryCatch({
    lm(y ~ x + y_lag, data = clean_data)
  }, error = function(e) {
    stop("AR(1) model fitting failed: ", e$message)
  })
  
  model_summary <- summary(model_fit)
  
  coeffs <- coef(model_fit)
  se_vals <- model_summary$coefficients[, "Std. Error"]
  
  # Ensure consistent naming
  names(coeffs) <- names(se_vals) <- c("intercept", "x", "y_lag")
  
  list(
    model = model_fit,
    coefficients = coeffs,
    se = se_vals,
    fitted_values = fitted(model_fit),
    residuals = residuals(model_fit),
    r_squared = model_summary$r.squared,
    n_obs = nrow(clean_data)
  )
}