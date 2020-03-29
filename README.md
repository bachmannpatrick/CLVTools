
[![R CMD
checks](https://github.com/bachmannpatrick/CLVTools/workflows/R-CMD-check/badge.svg)](https://github.com/bachmannpatrick/CLVTools/actions)

[![Tests](https://github.com/bachmannpatrick/CLVTools/workflows/Tests/badge.svg)](https://github.com/bachmannpatrick/CLVTools/actions)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## The CLVTools Package

Today, customer lifetime value (CLV) is the central metric for valuing
customers. It describes the long-term economic value of customers and
gives managers an idea of how customers will evolve over time. To model
CLVs in continuous non-contractual business settings such as retailers,
probabilistic customer attrition models are the preferred choice in
literature and practice.

The R package **CLVTools** provides an efficient and easy to use
implementation framework for probabilistic customer attrition models in
non-contractual settings. Building up on the learnings of other
implementations, the package adopts S4 classes to allow constructing
rich and rather complex models that nevertheless still are easy to apply
for the end user. The framework is capable to accomodate a variety of
probabilistic customer attition models for non-contractual settings in
continuous and discrete time.

Currently, CLVTools implements the following probabilistic models:

1)  Standard Pareto/NBD model (Schmittlein, Morrison & Colombo 1987)

2)  Pareto/NBD model with **time-invariant** contextual factors (Fader
    and Hardie 2007)

3)  Pareto/NBD model with **time-varying** contextual factors (Bachmann
    & Meierer XX)

In addtion the framework features an “interlayer system” to allow the
flexible addtion of model extensions during the model fitting process.
Currently these layeyers include:

  - Correlation of the purchase and the attrition process

  - L2 regularization for parameters of contextual factors

  - Equality constraints between parameters of contextual for the
    purchase and the attrition process.

## Installation Instructions

Install the stable version from CRAN:

    install.packages("CLVTools")

Install the development version from GitHub:

    devtools::install_github("CLVTools", ref = "development")

## Contributions

Feedback and contributions to this package are welcome\! Please use
[GitHub Issues](https://github.com/bachmannpatrick/CLVTools/issues) for
filing bug reports. Provide your contributions in the form of [Pull
Requests](https://help.github.com/articles/about-pull-requests/). See
also [these general
guidelines](https://guides.github.com/activities/contributing-to-open-source/#contributing)
to contribute to Open Source projects on GitHub.
