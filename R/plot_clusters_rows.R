#' plot.clusters.rows
#'
#' This function returns the Correspondence Analysis scatterplot of row categories, with points' colour according to the clusters to which points belong.
#' An Excel file is also created (in the R working directory) containing the cluster membership for each row category.
#' @param data: Dataframe containing the contingency table to be analysed.
#' @param x,y: The numbers of the dimensions to be plotted (from 1 to 5).
#' @keywords plot, clusters, rows, correspondence analysis
#' @export
#' @examples
#' plot.clusters.rows (mydata,1,2): plot the Correspondence Analysis scatterplot of row categories (using the 'mydata' dataframe), displaying the 1 and 2 dimension.
#' 
plot.clusters.rows <- function (data, x,y){
res.ca <- CA(data, axes=c(x, y), graph=FALSE)
resclust.rows<-HCPC(res.ca, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, graph.scale="inertia", graph=FALSE, cluster.CA="rows")
plot(resclust.rows, axes=c(x,y), choice="map", draw.tree=FALSE, ind.names=TRUE, new.plot=TRUE)
write.xlsx(resclust.rows$data.clust, "CAseriation_rows_clust.xlsx", sheetName="rowClusters", col.names=TRUE, row.names=TRUE, append=FALSE)
}