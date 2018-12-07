# Bland-Altman

## To install GitHub R packages:

```R
# Only the first time or to upgrade:
install.packages("devtools")
library(devtools)
install_github("sauthiem/BlandAltman")

# Every time before use
library(BlandAltman)
```


## Example:

```R
# Any numerical vector. x and y have to be the same length.
x <- rnorm(100)
y <- rnorm(100)

# Plot
BA.plot(x,y, title='My great title', percent=T, reference="mean", conf.int=0.95)

# Numbers
ba <- BA.analysis(x,y)
str(ba)
```

## Example 2:

![BA Example](BA.svg)


## Parameters

- title: Title of the plot
- percent: relative estimation of the differences (default F)
- reference: X axis (method A, B or mean) (default "mean")
- conf.int: confidence interval of both limits of agreements and all uncertainties (default 0.95 (~ ±1.96)

## List returned elements:

- x
- y
- bias
- bias.ci
- bias.ci.lower
- bias.ci.upper
- limit.agrmt.ci
- limit.agrmt.upper
- limit.agrmt.upper.ci.upper
- limit.agrmt.upper.ci.lower
- limit.agrmt.lower
- limit.agrmt.lower.ci.upper
- limit.agrmt.lower.ci.lower
- percentage.error
- n
- conf.int


## References:

1) Bland, J.M. & Altman, D.G., 1986. *Statistical methods for assessing agreement between two methods of clinical measurement*. **Lancet**, 1(8476), pp.307–310.
2) Giavarina, D., 2015. *Understanding Bland Altman analysis*. **Biochemia Medica**, 25(2), pp.141–151
