<!-- badges: start -->
[![Documentation](https://img.shields.io/badge/documentation-v0.1.0-blue)](https://ndpvh.github.io/parameter-reliability)
[![R-CMD-check](https://github.com/ndpvh/parameter-reliability/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ndpvh/parameter-reliability/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ndpvh/parameter-reliability/graph/badge.svg)](https://app.codecov.io/gh/ndpvh/parameter-reliability)
[![GitHub License](https://img.shields.io/github/license/ndpvh/parameter-reliability)](https://github.com/ndpvh/parameter-reliability/blob/main/LICENSE)
<!-- badges: end -->

# Parameter reliability

## Overview

This project serves as a follow-up to the studies of Vanhasbroeck et al. (2024) and Yu et al. (2025) in which we investigated the reliability of the measurement of affect and perception respectively. Common across these two studies is the use of computational models to investigate this reliability. However, after additional thought, we both agreed that such an analysis may be faulty when models are misspecified.

Within this project, then, we investigate to which extent we can rely on parameter estimates for misspecified models with the goal of detecting misspecification based on the reliability with which parameters are estimated across different blocks of experimental stimuli and/or by making the mechanisms with which misspecified models compensate for the misspecification explicit. 

## Installation

To run the analyses in the script `simulation.R`, you need to first install the package that comes with this project. To do so, you should run:

```
remotes::install_github("ndpvh/parameter-reliability")
```

You can then load these function through calling `library`, specifically:

```
library(paramrel)
```

## Usage

The documentation for the functions contained within this package can be found [here](https://ndpvh.github.io/parameter-reliability). More detailed explanation of this project can be found under the vignette [Project](https://ndpvh.github.io/parameter-reliability/articles/project.html). A more detailed explanation of how to use the relevant functions of the package can be found under the vignette [Simulation](https://ndpvh.github.io/parameter-reliability/articles/simulation.html).

## Getting help

If you encounter a bug, please file an issue with a minimal working example on [Github](https://github.com/ndpvh/parameter-reliability/issues).

## References

Vanhasbroeck, N., Vanbelle, S., Moors, A., Vanpaemel, W., & Tuerlinckx, F. (2024). Chasing consistency: On the measurement error in self-reported affect in experiments. Behavior Research Methods, 56(4), 3009-3022. doi: 10.3758/s13428-023-02290-3

Yu, K., Lin, T.-Y., Zaman, J., Tuerlinckx, F., & Vanhasbroeck, N. (2025). Consistency of perceptual response variability in size estimation and reproduction tasks. Behavior Research Methods, 57, Article 127. doi: 10.3758/s13428-025-02650-1

