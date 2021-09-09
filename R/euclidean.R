#' find the greatest common divisor of two numbers.
#'
#' @param a A number.
#' @param b A number.
#' @return The greatest common divisor of \code{a} and \code{b}.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export

euclidean <- function(a,b){
  if((is.numeric(a)==FALSE)||(is.numeric(b)==FALSE)){
    stop()
  }
  a = abs(a)
  b = abs(b)
  if(a<b){
    t = a
    a = b
    b = t
  }
  while (b != 0) {
    t = b
    b = a %% b
    a = t
  }
  return(a)
}
