#' birthday
#'
#' @param n a number
#'
#' @returns a percentage
#' @export
#'
#' @examples birthday(5)
birthday = function(n){
  1-exp(lchoose(365,n) + lfactorial(n) - n*log(365))
}
