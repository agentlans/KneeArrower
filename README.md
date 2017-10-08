# KneeArrower
R package to find cutoff points on knee curves

## Install
Open R, install the prerequisite `signal` package, then install this package from GitHub.
```
library(devtools)

install.packages("signal")
install_github("agentlans/KneeArrower")
```
## Use
Please see package vignette for more details.
```
# Generate an example knee curve
x <- seq(0, 5, 0.05)
y <- log(1+x)

# Find the cutoff point.
# By default, it's the point at which the first derivative is half of maximum along the curve.
findCutoff(x, y)
```
## Author
Alan Tseng

## License
GPL-3
