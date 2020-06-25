Let the data matrix $\mathbf{X}$ be *n x p* size, where *n* is the number of data rows and *p* is the number of data columns in this matrix. We can analysis the data matrix by row or column. Each data row represents a sample, and each data column represents a variable.  Let us assume that it is centered, i.e. column means have been subtracted and are now equal to zero.
Then the *p x p* covariance matrix is given by $\mathbf{C}$:

$$\mathbf{C}=\mathbf{X}^T\mathbf{X}/(n-1).$$

As it is a square symmetric matrix, it can be diagonalised as follows:
$$\mathbf{C}=\mathbf{V}\mathbf{\Lambda}\mathbf{V}^T,$$

where $\mathbf{V}$ is a matrix of orthogonal eigenvectors (where each column is an eigenvector) and $\mathbf{\Lambda}$ is a diagonal matrix with eigenvalues $\lambda_i$ in the decreasing order from the largest to the smallest value on the diagonal. The eigenvectors determine principal directions or principal axes of the data. The variables corresponding to these principal axes are known as principal components. Projections of the data on the principal axes gives scores of these principal components.  The $j$-th principal component is given by $j$-th column of the matrix product $\mathbf{XV}$. The coordinates of the $i$-th row of $\mathbf{XV}$ data point in the new PC space are given by the $i$-th row of the matrix product  $\mathbf{XV}$.
