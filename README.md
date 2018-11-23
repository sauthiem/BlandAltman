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
