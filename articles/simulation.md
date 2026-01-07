# Simulation

This page will introduce some of the core functions and how to use them
for the simulation. This follows the `simulation.R` file and makes some
sense out of it.

## Models

The first thing to understand are the models, which are defined in
`model.R`. These S4 classes define all models that we use in our
simulation study. A model consists of a set of parameters and a
within-person standard deviation, which can be provided values through
the attributes `parameters` and `sd`. For example, creating a linear
model and calling it `my_model`, we get:

``` r
my_model <- linear(
    parameters = c(10, 5),
    sd = 2
)
my_model
```

    ## An object of class "linear"
    ## Slot "parameters":
    ## [1] 10  5
    ## 
    ## Slot "sd":
    ## [1] 2

In this piece of code, the intercept is given the value `10` and the
slope for *x* is given the value `5`. You can retrieve the parameters
through calling its attributes:

``` r
my_model@parameters 
```

    ## [1] 10  5

``` r
my_model@sd
```

    ## [1] 2

You can also change the values of the parameters through these same
attributes:

``` r
my_model@parameters <- c(-10, -5)
my_model@sd <- 0.5

my_model@parameters 
```

    ## [1] -10  -5

``` r
my_model@sd
```

    ## [1] 0.5

Note that to initialize a model, you do not necessarily have to specify
the arguments:

``` r
my_model <- linear()

my_model@parameters 
```

    ## [1] 0 0

``` r
my_model@sd
```

    ## [1] 1

Instead, the default parameter settings are used.

## Parameter generation

The primary function to use for generating parameters is
`parameter_grid`, which will create a given number of parameter sets for
a particular model. Using this function only requires passing down the
model you are interested in. For example, for the linear model, we can
do:

``` r
my_model <- linear()

params <- parameter_grid(my_model)
head(params)
```

    ##      [,1] [,2]
    ## [1,]  -10 -5.0
    ## [2,]  -10 -2.5
    ## [3,]  -10  0.0
    ## [4,]  -10  2.5
    ## [5,]  -10  5.0
    ## [6,]   -5 -5.0

The output of this function is a matrix of size $n \times k$, where $n$
is the number of parameter sets that have been generated and $k$
represents the number of parameters in the model. In this case, we get a
$25 \times 2$ matrix.

Through additional arguments, we can control the parameter generation
process. Specifically, one can change the number of parameter sets that
are being generated, the process that is used for the generation of the
parameters, and the bounds of the parameters. Focusing first on the
number of parameters that are generated, one can change the values of
`n_int` and `n_slope`, respectively denoting the number of different
intercepts and slopes one wants to generate. For example:

``` r
params <- parameter_grid(
    my_model,
    n_int = 10, 
    n_slope = 10
)
head(params)
```

    ##      [,1]       [,2]
    ## [1,]  -10 -5.0000000
    ## [2,]  -10 -3.8888889
    ## [3,]  -10 -2.7777778
    ## [4,]  -10 -1.6666667
    ## [5,]  -10 -0.5555556
    ## [6,]  -10  0.5555556

``` r
params <- parameter_grid(
    my_model,
    n_int = 20, 
    n_slope = 20
)
head(params)
```

    ##      [,1]      [,2]
    ## [1,]  -10 -5.000000
    ## [2,]  -10 -4.473684
    ## [3,]  -10 -3.947368
    ## [4,]  -10 -3.421053
    ## [5,]  -10 -2.894737
    ## [6,]  -10 -2.368421

The first piece of code results in a $100 \times 2$ matrix while the
second piece of code results in a $400 \times 2$ matrix. How do we get
to these values? By default, the function `parameter_grid` will sample
parameter values in an ordered manner within specified bounds of the
parameters. The argument `n_int` and `n_slope` then define the number of
unique, equally spaced values you wish to sample within this interval,
where `n_int` controls this number of intercept parameters and `n_slope`
for the slope parameters.

While this type of simulation allows for a well-controlled simulation
study, it may lack some generalizability across different simulation
studies when the number of unique parameters differs greatly. We
therefore allow for another type of parameter generation process through
specifying the argument `n`, for example:

``` r
params <- parameter_grid(
    my_model,
    n = 100
)
head(params)
```

    ##            [,1]      [,2]
    ## [1,] -8.3849972  3.056800
    ## [2,]  6.6866607  3.140513
    ## [3,]  2.0152177 -0.960890
    ## [4,] -6.8558312 -2.815690
    ## [5,] -9.8520112 -0.816386
    ## [6,] -0.6721301  1.688707

This will output a $100 \times k$ parameter matrix, no matter which
model you provide to the function. In other words, you will always end
up with 100 unique parameter sets. The way in which we achieve this is
through uniform sampling across the bounds of the parameters. This does
not guarantee that the parameters cover the entire possible range, but
it gains in generalizability across different models in the simulation.

Finally, you can change the bounds of the intercepts and slopes through
the `intercept` and `slope` parameters, where you provide a minimum and
maximum value for the parameter. For example:

``` r
params <- parameter_grid(
    my_model,
    n = 10,
    intercept = c(-1, 1),
    slope = c(-1, 1)
)
head(params)
```

    ##            [,1]       [,2]
    ## [1,] -0.3014019 -0.5101440
    ## [2,]  0.8946365  0.5622451
    ## [3,] -0.5678000 -0.4235257
    ## [4,] -0.9358146  0.7507158
    ## [5,] -0.7093683 -0.4084998
    ## [6,]  0.7087678  0.9670508

``` r
params <- parameter_grid(
    my_model,
    n = 10,
    intercept = c(-100, 100),
    slope = c(-50, 50)
)
head(params)
```

    ##           [,1]      [,2]
    ## [1,] -16.54601  48.32837
    ## [2,] -72.38503 -28.14370
    ## [3,] -83.83101  16.45301
    ## [4,]  31.19653 -11.04360
    ## [5,]  20.40077 -45.39364
    ## [6,]  31.39917  11.69146

## Running a simulation

The heavy lifting for running a simulation is done by the
`execute_study` function. At minimum, this function requires a
generative and an estimation model to be defined, so that you can call:

``` r
sim_model <- linear()
est_model <- quadratic()

execute_study(
    sim_model, 
    est_model
)
```

The most important arguments that you can change in this function are
the following:

- `n_participants`, `n_outcomes`, `n_bins`: Integers denoting the number
  of participants in the study and the number of bins and outcomes per
  bin to include in the experiment.
- `parameter_sd`: Numeric denoting the standard deviation on the
  parameters across participants (i.e., individual differences).
- `ICC`: Numeric between 0 and 1 denoting the ICC you wish to simulate,
  pitting between-subject differences against within-person differences.
- `statistics`: Named list containing functions that compute the
  statistics of interest (see `statistics.R` for more details).

Through changing the values of the arguments, we can run a small
simulation study as:

``` r
sim_model <- linear()
est_model <- quadratic()

execute_study(
    sim_model, 
    est_model,
    n_participants = 10, 
    n_outcomes = 15, 
    n_bins = 5, 
    ICC = 0.8,
    statistics = list(
        "icc" = icc, 
        "signal" = signal
    )
)
```

    ## $icc
    ##   parameter        icc systematic   residual
    ## 1 intercept 0.59547450  0.6547292 0.44477914
    ## 2   slope_1 0.57431435  0.5830172 0.43213625
    ## 3   slope_2 0.06873378  0.0035012 0.04743736
    ## 
    ## $signal
    ##   parameter coefficient_variation signal_to_noise
    ## 1 intercept              1.796748        6.651522
    ## 2   slope_1              3.760290        7.209125
    ## 3   slope_2             16.963877        0.000000

The output of this function is a named list containing the output of the
functions provided to `statistics`. These serve as our primary unit of
analysis.
