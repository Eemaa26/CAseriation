\name{CAseriation-package}
\alias{CAseriation-package}
\alias{CAseriation}
\docType{package}
\title{
Contingency table seriation by means of Correspondence Analysis
}
\description{
CAseriation allows to sort the rows and columns of the input contingency table according to the scores of rows and columns on the Correspondence Analysis' dimension selected by the user. The package also allows to plot the CA scatterplot of selected dimensions, and to seek for clusters in the dataset. As for seriation, two plots are returned,  displaying the sorted contingency table. The results are also exported into an Excel spreadsheet.
}
\details{
\tabular{ll}{
Package: \tab CAseriation\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-22\cr
License: \tab GPL\cr
}
In archaeology there is often the need to seriate contingency tables in order to devise a relative chronology of different types of contexts (e.g., graves). Different approaches exists in literature to achieve a best ordering. The method implemented in the 'CAseriation' package is the ordination of rows and columns of a contingency table according to their scores on the Correspondence Analysis' dimension selected by the user. 

The ideal workflow for the use of the package would be: 

(a) fed the contingency table into R; 

(b) inspect the Correspondence Analysis scatterplot in search of a seriation structure (i.e., presence of the 'horseshoe' effect); 

(c) sort the table according to the dimension the user is interesting in; 

(d) additionally, formally assess the existence of clusters in the data.


Implemented functions to achieve the above goals:

(b) check.ca.plo()

(c) sort.table()

(d) plot.clusters.rows(); plot.clusters.rows()
}
\author{
Gianmarco ALBERTI

Maintainer: Gianmarco ALBERTI <gianmarcoalberti@tin.it><gianmarco.alberti@um.edu.mt>
}
\references{
Baxter M.J. 1994, Exploratory Multivariate Analysis in Archaeology, Edinburgh, Edinburgh University Press.

Shennan S. 1997, Quantifying Archaeology, Edinburgh, Edinburg University Press.
}
\keyword{ package }
\seealso{
ca,FactoMineR, seriation,xlsx
}
\examples{

}