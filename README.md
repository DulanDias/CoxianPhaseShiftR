# CoxianPhaseShiftR
An R package introducing the Progressive Coxian Phase Type Model. Elevating traditional Coxian models, it offers enhanced survival analysis for time-to-event data. A game-changer for researchers and data scientists. Dive in and experience the shift!

## Description
The CoxianPhaseShiftR package provides tools for fitting data to Coxian phase type distributions. It offers functionalities to optimize the parameters of the distribution, evaluate the goodness of fit, and visualize the results. The package introduces the Progressive Coxian Phase Type Model, a novel approach to utilizing Coxian models for various tasks.

## Installation
To install the latest version of CoxianPhaseShiftR from GitHub:
```
# If you haven't installed the devtools package yet, do so with:
# install.packages("devtools")

devtools::install_github("DulanDias/CoxianPhaseShiftR")
```

## Usage
After installation, you can load the package in R:
```
library(CoxianPhaseShiftR)
```

## Example
```
data_sample <- rexp(100, rate = 0.5)
fit_results <- fitCoxian(data_sample, 2)
```

Documentation
For detailed function documentation, after installing and loading the package, use:
```
?function_name
```
Replace function_name with the name of the function you want to learn more about.
