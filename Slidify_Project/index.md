---
title       : Central Limit Theorem
subtitle    : Definition and Experiments
author      : M. Colosso
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax, quiz, bootstrap]     # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
--- #custbg

<style>
  #custbg {
     background-image: url(./assets/img/cltapp.png);
     background-repeat: no-repeat;
     background-size: 30%;
     background-position: 98% 98%;
   }
</style>

## Central Limit Theorem

**Central Limit Theorem** is an application which can be use to understand the application and meaning of the Central Limit theorem, which is one of the most important theorem is statistics.

This app is intended for students of first level class in statistics and, in general, to any person wishing to deepen their knowledge in this subject.

<br>

### **Central Limit Theorem in practice**

For a better understanding, read about the definition of Central Limit Theorem, distributions and help included in the application before use it and then enjoy watching the curves converging to a normal distribution by varying the parameters that define.

To access the application, click on this link:
[Central Limit Theorem](http://mcolosso.shinyapps.io/Central_Limit_Theorem/)

--- &radio

## Quiz

To validate your comprenhension of Central Limit Theorem and this application, please answer the following questions:

### Question 1

How do you "normalize" a distribution (center and reduce dispersion)?

1. $(x - \mu) / \sqrt \sigma$
2. _$(x - \mu) / \sigma$_
3. $(x - \mu) / \sigma ^2$
4. _$(x - \mu) / (\sigma / \sqrt {n})$_

*** .explanation

To normalize a distribution, you have to center it substracting the mean ($\mu$) to the values and scaling the dispersion, dividing by the standard deviation ($\sigma$) or by the standard error ($\sigma / \sqrt {n}$)

**Example**: Coin flip ($\mu=3.5$, $\sigma=\sqrt{35/12} \approx 1.707$)


```r
  n        <- 1000
  tbl      <- sample(1:6, size=n, replace=TRUE)
  tbl.norm <- (tbl - 3.5) / sqrt(35/12)
  matrix(c(mean(tbl), sd(tbl),
           mean(tbl.norm), sd(tbl.norm)),
         nrow = 2, ncol = 2, byrow=TRUE,
         dimnames=list(c("original", "normalized"),
                       c("mean", "std.dev")))
```

```
##                  mean   std.dev
## original    3.4950000 1.7073049
## normalized -0.0029277 0.9996954
```

--- &checkbox

## Question 2

Which experiments are conduced in the application? (Select all that apply)

1. Normal distribution
2. _One coin flip_
3. _One die rolling_
4. Uniform distribution
5. _Exponential distribution_
6. _Two dice rolling_
7. Two coin flip
8. Gamma distribution
9. _Poisson distribution_

*** .explanation

Distribution used in the application are: Die rolling, two dice rolling, coin flip, exponential distribution and Poisson ditribution.

--- &radio

## Question 3

Increasing the number of observations, the computed values of the
average in all groups will be distributed according to the normal distribution ("Gauss bell")?

1. _True_
2. False

*** .hint

See definition of Central Limit Theorem eslsewhere

*** .explanation

In probability theory, the **Central Limit Theorem (CLT)** states that,
given certain conditions, the arithmetic mean of a sufficiently large
number of iterates of independent random variables, each with a
well-defined expected value and well-defined variance, will be approximately
normally distributed, regardless of the underlying distribution.
