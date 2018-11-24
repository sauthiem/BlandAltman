# Bland-Altman

Example:

```R
x <- rnorm(100)
y <- rnorm(100)

# Plot
BA.plot(x,y, title='My great title', conf.int=0.95)

# Numbers
ba <- BA.analysis(x,y)
str(ba)
```

Calculated elements:

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

References: 
1) Bland, J.M. & Altman, D.G., 1986. Statistical methods for assessing agreement between two methods of clinical measurement. Lancet, 1(8476), pp.307–310.
2) Giavarina, D., 2015. Understanding Bland Altman analysis. Biochemia Medica, 25(2), pp.141–151
