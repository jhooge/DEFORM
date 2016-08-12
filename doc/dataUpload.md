In Machine Learning we tend to think in terms of feature matrices and label vectors. 
A feature matrix contains all samples in its rows and all features in its columns. 
The response (or labels) is a one dimensional vector that include the measurements 
to be predicted by the features contained in the feature matrix. Depending, whether 
this label vector is discrete or continuous, you are either faced with a classification 
or a regression problem. This application expects the feature matrix and the label vector 
to be uploaded two comma separate files. You should make sure that the feature matrix file 
includes a header, an that floating point numbers are formatted with a "." and not with a ",".
The label vector should be represented as a column vector with the same number of rows as the feature
matrix.
