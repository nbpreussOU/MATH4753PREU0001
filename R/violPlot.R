#' Create a violin ggplot for lab 1
#'
#' @param df dataframe
#' @param x column of dataframe used as the horizontal axis
#' @param y column of dataframe used as the vertical axis
#' @param z column of dataframe used as the fill for the violin plot
#' @return a ggplot object
#'
#' @export
#'
violPlot <- function(df, x, y, z)
{
    v<-ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, fill =z)) + ggplot2::geom_violin() + ggplot2::ggtitle("Violin Plot")
    print(v)
}
