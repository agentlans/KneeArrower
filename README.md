# KneeArrower
R package to find cutoff points on knee curves.

Example:
100 random simulated points (gray) and the knee point (red) found using this package.

![A curve with a knee point](https://github.com/agentlans/KneeArrower/blob/master/Example.png)

## Install

- [KneeArrower is available on CRAN](https://cran.r-project.org/web/packages/KneeArrower/index.html). 
In R, run `install.packages("KneeArrower")` to install.

- Alternatively, to install this package from GitHub, first install the prerequisite `signal` package and then this repository:
```r
library(devtools)

install.packages("signal")
install_github("agentlans/KneeArrower")
```
## Use
The following code shows example of finding knee points. More options are available.
Please see the package [vignette](https://cran.r-project.org/web/packages/KneeArrower/vignettes/Example.html) for more details.
```r
library(KneeArrower)

# Generate an example knee curve
x <- seq(0, 5, 0.05)
y <- log(1+x)

# Find the cutoff point.
# By default, it's the point at which the first derivative is half of maximum along the curve.
findCutoff(x, y)

# For more information on the options, view the help file for findCutoff
?findCutoff
```
## Frequently Asked Questions and Troubleshooting

**How does this work?**

1. The points are fitted to a curve using the Savitzky-Golay filter to eliminate bumps and rough edges.
2. Then the first derivative is calculated from the curve. The "first derivative cutoff method" (default) interpolates a point where the slope is equal to a given value.
3. The second derivative can also be calculated. In fact, the "maximum curvature method" searches for a knee point by optimizing an expression containing second derivatives.

**What does this mean?**

- The methods won't work well with data with many humps or extreme discontinuities.
  - But knee points won't make sense in those cases anyway.
- The first derivative method should be more robust than anything involving second derivatives
  - because class *C*<sup>1</sup> curves are a superset of class *C*<sup>2</sup> curves.
  - However, it doesn't matter much in practice. You can use either method.

**Why doesn't the output point look like a knee point?**

- Try plotting x and y coordinates on the same scale.

- Also, if you're using the first derivative cutoff method (default), you can adjust the slope to get a higher or lower point on the curve. Please see the vignettes for details.

**The output point doesn't match the knee point I found using calculus!**

- The knee points given by the package are only approximate because the derivatives have to be estimated at every point from the data. However, the output should be very close to the actual point.

**What happens if there's more than one knee point?**

- The package arbitrarily returns one of the knee points. The others won't appear in the output.

**How come I can't install vignettes?**

- `devtools` install doesn't install vignettes by default.
- You may install vignettes by cloning this repository and building the package offline.
- Note: other packages may need to be installed first in order to build the vignettes.

## Author, License

Copyright 2018, 2019, 2020-2022 Alan Tseng

GNU General Public License v3
