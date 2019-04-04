#' Round to integer
#' @description Create ylim value, including negative value, for barplot.
#' @usage wrange(v)
#' @param v a numeric vector, or data.frame.
#' @return a numeric vector for ylim aruguments of barplot
#' @examples
#' tN <- table(Ni <- stats::rpois(100, lambda = 5))
#' barplot(tN, col = rainbow(20), ylim = rsko::wrange(tN), main='long y-axis')
#'
#' ziris <- rsko::zconv(iris, 1:4, 1)
#' dat <- ziris[c("1", "51", "101"),-5]
#' par(mfcol=c(1,3), mar=c(6,2,1,1))
#'  res <- lapply(seq(nrow(dat)), function(i){
#'  barplot(unlist(dat[i,]), ylim = wrange(dat), las=2)
#'  })
#'
#' @export
wrange <- function(v){
  # min or max value
  mn <- range(v)[1]
  mx <- range(v)[2]

  # round function
  wr <- function(x){
    if (x < 0 ) {
      n = 10^floor(log10(abs(x)))
      res <- ceiling(abs(x)/n) * n * -1
    } else if (x > 0 ) {
      n = 10^floor(log10(x))
      res <- ceiling(x/n) * n
    } else if (x == 0) {
      res <- 0
    }
    return(res)
  }

  # return
  wmx <- wr(mx)
  if (!mn <= 0) {
    wmn <- 0
  } else {
    wmn <- wr(mn)
  }
  return(c(wmn, wmx))
}
