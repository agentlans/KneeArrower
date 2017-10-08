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
The following code shows example of finding knee points. More options are available.
Please see package vignette for more details.
```
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
## Troubleshooting

- **Why doesn't the output look like a knee point?**

Try plotting x and y coordinates on the same scale.
Also, if you're using the first derivative cutoff method (default), you can adjust the slope to get a higher or lower point on the curve. Please see vignettes for details.

- **How come I can't install vignettes?**

`devtools` install doesn't install vignettes by default.
You may install vignettes by cloning this repository and building the package offline.
Note: other packages may need to be installed first.

## Author
Alan Tseng

## License
GPL-3
