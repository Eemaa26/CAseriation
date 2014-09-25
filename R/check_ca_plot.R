#' check.ca.plot
#'
#' This function returns the Correspondence Analysis scatterplot of both row and column categories.
#' @param data: Dataframe containing the contingency table to be analysed.
#' @param x,y: The numbers of the dimensions to be plotted (from 1 to 5).
#' @keywords correspondence analysis, plot
#' @export
#' @examples
#' check.ca.plot (mydata,1,2): plot the Correspondence Analysis scatterplot of the 'mydata' dataframe, displaying the 1 and 2 dimension.
#' 
check.ca.plot <- function (data, x,y){
CA(data, axes=c(x, y))
}