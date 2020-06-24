
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
```

The loaded csv file contains data of face images taken between April 1992 and April 1994 at AT&T Laboratories Cambridge. Each row contains data of one image quantized to 256 grey levels between 0 and 1. After loading, the data are converted into the data frame format.

This project Let the data matrix **X** be *n x p* size, where *n* is the number of data rows and *p* is the number of data columns in this matrix. We can analysis the data matrix by row or column. Each data row represents a sample, and each data column represents a variable. Let us assume that it is centered, i.e. column means have been subtracted and are now equal to zero. Then the *p x p* covariance matrix is given by **C**:

**C** = **X**<sup>*T*</sup>**X**/(*n* − 1).
 As it is a square symmetric matrix, it can be diagonalised as follows:
**C** = **V****Λ****V**<sup>*T*</sup>,

where **V** is a matrix of orthogonal eigenvectors (where each column is an eigenvector) and **Λ** is a diagonal matrix with eigenvalues *λ*<sub>*i*</sub> in the decreasing order from the largest to the smallest value on the diagonal. The eigenvectors determine principal directions or principal axes of the data. The variables corresponding to these principal axes are known as principal components. Projections of the data on the principal axes gives scores of these principal components. The *j*-th principal component is given by *j*-th column of the matrix product **X****V**. The coordinates of the *i*-th row of **X****V** data point in the new PC space are given by the *i*-th row of the matrix product **XV**.

If we now perform singular value decomposition of **X**, we obtain a decomposition
**X** = **U****S****V**<sup>*T*</sup>,
 where **U** is an orthonormal matrix and **S** is the diagonal matrix of singular values. From here one can easily see that
**C** = (**U****S****V**<sup>*T*</sup>)<sup>*T*</sup>**U****S****V**<sup>*T*</sup>/(*n* − 1),

**C** = **V**<sup>*T*</sup>**S****U**<sup>*T*</sup>**U****S****V**<sup>*T*</sup>/(*n* − 1),

$$\\mathbf{C}=\\mathbf{V}^T\\frac{\\mathbf{S}^2}{n-1}\\mathbf{V}^T,$$

meaning that right singular vectors **V** are principal directions and singular values are related to the eigenvalues of covariance matrix via
*λ*<sub>*i*</sub> = *s*<sub>*i*</sub><sup>2</sup>/(*n* − 1).
 Principal components are given by
**X****V** = **U****S****V**<sup>*T*</sup>**V** = **U****S**
. If **X** = **U****S****V**<sup>*T*</sup>, then columns of **V** are orthonormal principal directions) determining principal axis. Columns of **U****S** are principal components with their scores Singular values *s*<sub>*i*</sub> are related to the eigenvalues *λ*<sub>*i*</sub> of covariance matrix **C** via
*λ*<sub>*i*</sub> = *s*<sub>*i*</sub><sup>2</sup>/(*n* − 1).

Eigenvalues *λ*<sub>*i*</sub> show varinaces of the respective PCs.

In statistics, a data point (or observation) is a set of one or more measurements on a single member of a statistical population.

the x-coordinates of projection points

The variance along the x-axis is the variance of the feature (variable?) represented by that axis, i.e. the variance of the x-coordinates of the dataset. To generalise this notion to any line in space: the x-coordinates of the data points are merely their projections on the x-axis. Using this fact, we can easily find that the variance along any line by projecting our points on the unit vector representing the line, and then taking variance of these projections.

If **A** is n X d mean-centered data matrix, where n is the number of samples (data points?) and d is the number of dimensions (features, variables?)

Including Plots
---------------

You can also embed plots, for example:

![](Figures/pressure-1.png)

$$\\mathbf{X} = \\left\[\\begin{array}
{rrr}
1 & 2 & 3 \\\\
4 & 5 & 6 \\\\
7 & 8 & 9
\\end{array}\\right\]
$$

$$\\frac{4z^3}{16}$$
$$\\sum\_{i=1}^{n}\\left( \\frac{X\_i}{Y\_i} \\right)$$
$$\\frac{4z^3}{16}$$
$$\\sqrt{b^2 - 4ac}$$

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot. try

one *e*<sup>*i**π*</sup> = −1 two

asd cf <img src="https://math.now.sh?inline=\log\prod^N_{i}x_{i}=\sum^N_i\log{x_i}" valign="baseline"> rsd
