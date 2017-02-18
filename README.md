# Mutualism-NonlinearDensityDependence
Repository for code from
*Population dynamics of mutualism and intraspecific density dependence: how θ-logistic-like density dependence affects mutualistic positive feedback*
by Christopher M. Moore (me), Sam A. Catella, and Karen C. Abbott

There are two files:

1. NumericalSimulations.R
2. Figures.R

It's pretty straight forward: NumericalSimulations.R conducts the numerical local stability analysis and Figures.R creates all of the figures from the paper.

Each file has the nitty-gritty details under `DESCRPTION`.  In sum, NumericalSimulations.R takes parameters for variables b, d, mu, nu, eta, theta, and beta; loops over the stabilty analysis; and saves the run as a list.  Figures.R pruduces graphs in the device window or in a directory by generating its own data (Figures 1, 2, 4) or from saved output from the NumericaSimulations.R ourputs (Figures 3, 5, 6, 7).

If you have any questions or comments, please feel free to contact me or open an Issue.
