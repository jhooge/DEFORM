On the Dimension Reduction tab you have the option of applying different projection 
methods to lower the dimensionality of your data. Here you have the option between 
a Principal Component Analysis (PCA) and an Independent Component Analysis (ICA). The technique
will be applied to the data currently loaded feature matrix and in case of missing values, 
these will be imputed using a k-nearest neighbor approach. Because the PCA is a variance based projection method, the data will also be centered and scaled prior to application. As the ICA method is implemented in the fastICA it automatically does a PCA decomposition prior to finding the ICA scores, such that the data is centered and scaled as well.  
After you hit the Compute button you will be presented with a 3D and 3D projection of your data and in case of a PCA projection also with a screeplot indicating the cummulative variance still explained by the number of principal components after dimensionality reduction.

## PCA
Principle Component Analysis, or PCA is a technique that is widely used for applications such 
as dimensionality reduction, lossy data compression, feature extraction, and data visualization. There are two commonly used definitions of  PCA that give rise to the same algorithm. PCA can be defined as the orthogonal projection of the data onto a lower dimensional linear space, known as the principal subspace, such that the variance of the projected data is maximized. Equivalently, it can be defined as squared distance between the data points and their projections.

## ICA
Independent component analysis, or ICA is a computational method for separating a multivariate signal into additive subcomponents. This is done by assuming that the subcomponents are non-Gaussian signals and that they are statistically independent from each other. ICA is a special case of blind source separation. A common example application is the "cocktail party problem" of listening in on one person's speech in a noisy room.

