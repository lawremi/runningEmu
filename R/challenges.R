#' @title First function
#' @description
#' Given a square matrix, calculates the average over the sum of row averages and column averages
#' 
#' @param m a square matrix with no missing values
#' @return Single numerical value
#' @examples
#' m <- matrix(seq(16),nrow=4)
#' un(m)
#' @export
un <- function(m) {
  mean(apply(m,1,mean) + apply(m,2,mean))
}

#' @title Second
#' @description
#' Given a vector gives the longest continuous increasing subset
#' 
#' @param vec Numerical vector with no missing values
#' @return A numerical vector containing the longest continuous increasing subset
#' @export
deux <- function(x) {
    d <- diff(x) > 1
    r <- shift(ranges(Rle(d)), 1L)
    i <- r[which.max(width(r))]
    extractROWS(x, i)
}

deux2 <- function(x) {
    d <- c(FALSE, diff(x) > 0)
    dd <- diff(c(d, FALSE))
    starts <- which(dd > 0L)
    ends <- which(dd < 0L)
    m <- which.max(ends - starts)
    x[seq(starts[m], ends[m])]
}

#' @title Third
#' @description
#' Given a vector return the count of each unique element
#' 
#' Hint: Try looking into `tabulate`, `fastmatch::fastmatch`
#' @param vec Numerical vector
#' @return A single numerical vector with counts of each unique element
#'
#' @export
trois <- function(vec) {
  table(vec)
}
