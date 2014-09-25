#' sort.table
#'
#' This function allows to sort the input contingency table according to the score of row and column categories on the selected Correspondence Analysis dimension.
#' Two plots are returned, displaying the sorted contingency table. Note that the two plots are the same, except for the fact that one is the transposed version of the other.
#' An Excel file (named CAseriation.xlsx) is also created in the R working directory, containing the original dataset, the sorted table, and the sorted table converted into presence/absence data.
#' @param data: Dataframe containing the contingency table to be analysed.
#' @param dim: The number of the dimension (from 1 to 5) to be used to sort the contingency table (i.e., to perfom the seriation).
#' @keywords sort, contingency table, scores, correspondence analysis
#' @export
#' @examples
#' sort.table (mydata,1): sort the contingency table (contained in the dataframe 'mydata') according to the score of row and column categories on the 1 dimension of Correspondence Analysis.
#' 
sort.table <- function (data, dim){
# get some details about the input table in order to calculate the table's dimensionality
nrows <- nrow(data)
ncols <- ncol(data)
numb.dim.cols<-ncol(data)-1
numb.dim.rows<-nrow(data)-1
a <- min(numb.dim.cols, numb.dim.rows) #dimensionality of the table
#get the CA dataframe after the 'ca' package, selecting a number of dimensions equal to the table's dimensionality
res.ca<-ca(data, nd=a)
#get the coordinates on the selected CA axis
row.c<-res.ca$rowcoord[,dim]
col.c<-res.ca$colcoord[,dim]
#seriation
#sort the table according to the coord of the selected CA dimension and plot seriation chart (Bertin plot)
print(sorted.table<-data[order(row.c), order(col.c)]) #sort the table
print(sorted.table.PA<-apply(sorted.table, 2, function(x) ifelse(x>0, 1, 0))) #transform the sorted table into incidence table (presence/absence) to be used for Bertin plot
bertinplot(as.matrix(sorted.table.PA), options = list(panel=panel.squares, spacing = 0,frame = TRUE)) #plot the sorted incidence table
bertinplot(as.matrix(sorted.table.PA), options = list(panel=panel.squares, spacing = 0,frame = TRUE, reverse=TRUE)) #plot the sorted incidence table reversed
#export relevant data to Excel
write.xlsx(data, "CAseriation.xlsx", sheetName="originalDATA", col.names=TRUE, row.names=TRUE, append=FALSE)
write.xlsx(sorted.table, "CAseriation.xlsx", sheetName="sorted_table", col.names=TRUE, row.names=TRUE, append=TRUE)
write.xlsx(sorted.table.PA, "CAseriation.xlsx", sheetName="sorted_table_PA", col.names=TRUE, row.names=TRUE, append=TRUE)
}