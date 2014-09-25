#' plot.clusters.cols
#'
#' This function returns the Correspondence Analysis scatterplot of column categories, with points' colour according to the clusters to which points belong.
#' An Excel file is also created (in the R working directory) containing the cluster membership for each colum category.
#' @param data: Dataframe containing the contingency table to be analysed.
#' @param x,y: The numbers of the dimensions to be plotted (from 1 to 5).
#' @keywords cluster, columns, correspondence analysis
#' @export
#' @examples
#' plot.clusters.cols (mydata,1,2): plot the Correspondence Analysis scatterplot of column categories (using the 'mydata' dataframe), displaying the 1 and 2 dimension.
#' 
plot.clusters.cols <- function (data, x,y){
res.ca <- CA(data, axes=c(x, y), graph=FALSE)
resclust.cols<-HCPC(res.ca, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, graph.scale="inertia", graph=FALSE, cluster.CA="columns")
plot(resclust.cols, axes=c(x,y), choice="map", draw.tree=FALSE, ind.names=TRUE, new.plot=TRUE)
write.xlsx(resclust.cols$data.clust, "CAseriation_cols_clust.xlsx", sheetName="colClusters", col.names=TRUE, row.names=TRUE, append=FALSE)
}