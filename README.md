
This project ..

Set working directory to source file location. Then, install and load libraries:

``` r
if(!(require(dplyr))){install.packages('dplyr')}
library(dplyr)
```

Define a function to show face images:

``` r
showFace <- function(x){
  x %>%
  as.numeric() %>%
  matrix(nrow = 64, byrow = TRUE) %>% 
  apply(2, rev) %>%  
  t %>% 
  image(col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
  }
```

The purpose of this function is to convert a vector with the image into a matrix and fix the issue concerning R function `image` which creates a grid with 90 degree counter-clockwise rotation of the conventional printed layout of a matrix.

Load data with face images:

``` r
dataX <- "olivetti_X.csv" %>% 
  read.csv(header=FALSE) %>% 
  data.frame()
str(dataX, list.len = 5)
#> 'data.frame':    400 obs. of  4096 variables:
#>  <img src="/tex/28d3ac7fccc68260161b22a0c447727c.svg?invert_in_darkmode&sanitize=true" align=middle width=262.47779579999997pt height=22.465723500000017pt/> V2   : num  0.368 0.471 0.401 0.194 0.545 ...
#>  <img src="/tex/2edb48db2705d8d6c7ac69588ae18041.svg?invert_in_darkmode&sanitize=true" align=middle width=287.13542384999994pt height=22.465723500000017pt/> V4   : num  0.442 0.558 0.529 0.194 0.624 ...
#>  <img src="/tex/c37146d5589c7f73d08e741d1ec14d49.svg?invert_in_darkmode&sanitize=true" align=middle width=928.3588677pt height=876.1643847pt/><img src="/tex/6a70f13134ea549abcc702c13dc97842.svg?invert_in_darkmode&sanitize=true" align=middle width=145.47402209999998pt height=164.20092149999996pt/><img src="/tex/441a46810ac163feaa67f9e230bac663.svg?invert_in_darkmode&sanitize=true" align=middle width=747.3996073499999pt height=836.712327pt/><img src="/tex/c7f9580f118f2acc1a9ce94c8411c508.svg?invert_in_darkmode&sanitize=true" align=middle width=128.7191532pt height=361.4611935pt/><img src="/tex/51709c221bb606c7f0a6193f462db8dd.svg?invert_in_darkmode&sanitize=true" align=middle width=8.21920935pt height=14.15524440000002pt/><img src="/tex/94232c9b66722251ba038233e18a770f.svg?invert_in_darkmode&sanitize=true" align=middle width=73.89302744999999pt height=45.84475499999998pt/><img src="/tex/cbe4745c368cb36ecf6b1c81ef1d330a.svg?invert_in_darkmode&sanitize=true" align=middle width=8.21920935pt height=14.15524440000002pt/><img src="/tex/a79f6828abaa8845ac991481fe79a1a2.svg?invert_in_darkmode&sanitize=true" align=middle width=84.7604439pt height=166.02739559999998pt/><img src="/tex/cbe4745c368cb36ecf6b1c81ef1d330a.svg?invert_in_darkmode&sanitize=true" align=middle width=8.21920935pt height=14.15524440000002pt/><img src="/tex/94232c9b66722251ba038233e18a770f.svg?invert_in_darkmode&sanitize=true" align=middle width=73.89302744999999pt height=45.84475499999998pt/><img src="/tex/cbe4745c368cb36ecf6b1c81ef1d330a.svg?invert_in_darkmode&sanitize=true" align=middle width=8.21920935pt height=14.15524440000002pt/><img src="/tex/3b85c4698c6c534a12c1b1a6366839b4.svg?invert_in_darkmode&sanitize=true" align=middle width=87.98523524999999pt height=45.84475499999998pt/>$

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot. try

one *e*<sup>*i**π*</sup> = −1 two

asd cf <img src="https://math.now.sh?inline=\log\prod^N_{i}x_{i}=\sum^N_i\log{x_i}" valign="baseline"> rsd
