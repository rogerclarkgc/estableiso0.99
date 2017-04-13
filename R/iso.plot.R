#' Draw the dot plot of mixture and sources
#'
#' This funcion is used to draw a dot plot of your source and mixture, use with function iso.loaddata usually
#' iso.plot(datalist = list(), group = 0)
#'
#' @param datalist the output of iso.loaddata
#' @export
#' @examples
#' source.matrix <- data.frame(d15NPl = c(6.50, 4.42, 11.19, 9.82), d13CPl = c(-11.17, -30.88, -10.19, -15.01), sd.N = c(1.4594632, 2.2680709, 1.1124385, 0.8271039), mean.N = c(6.488984, 4.432160, 11.192613, 9.816280), sd.C = c(1.2149562, 0.6413182, 1.9593306, 1.1724677), mean.C = c(-11.17023, -30.87984, -11.17090, -14.05701))
#' mixture.matrix <- data.frame(d15NPl = 10.30, d13CPl = -11.58)
#' iso.plot(datalist = list(mixture = mixture.matrix, sources = source.matrix))

iso.plot <-
function(datalist = list(), group = 0){
  sources <- datalist$sources
  mixture <- datalist$mixture
  snum <- nrow(sources)
  mnum <- nrow(mixture)
  xr <- max(sources[, 4] + sources[, 3], mixture[, 1]) + 3
  xl <- min(sources[, 4] - sources[, 3], mixture[, 1]) - 3
  yr <- max(sources[, 6] + sources[, 5], mixture[, 2]) + 3
  yl <- min(sources[, 6] - sources[, 5], mixture[, 2]) - 3
  legend <- c(rownames(sources), "data")
  legend_col <- c(1:snum, "darkblue")
  legend_pch <- c(1:snum+4, 16)
  legend_lty <- c(rep(1, 3), 0)
  plot(1,1,
       type = "n", 
       xlim = c(xl, xr), 
       ylim = c(yl, xr), 
       main = "Mixture and Source data dot plot",
       xlab = colnames(mixture)[1],
       ylab = colnames(mixture)[2])
  legend("topleft",
         legend = legend,
         col = legend_col,
         pch = legend_pch,
         lty = legend_lty,
         bty = "n",
         inset = 0.01)
  for(i in 1 : snum){
    points(sources[i, 4], sources[i, 6], col = i, pch = i + 4, bg = i)
    lines(c(sources[i, 4] - sources[i, 3], sources[i, 4] + sources[i, 3]), c(sources[i, 6], sources[i, 6]), col = i) #error bar at x axis
    lines(c(sources[i, 4], sources[i, 4]), c(sources[i, 6] - sources[i, 5], sources[i, 6] + sources[i, 5]), col = i) #errow bar at y axis
  }
  for(j in 1 : mnum){
    points(mixture[j, 1], mixture[j, 2], cex = 1.2, pch = 16, col = "darkblue")
  }
  #return(c(xr, xl, yr, yl))
}
