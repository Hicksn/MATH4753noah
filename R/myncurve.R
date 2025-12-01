#' myncurve
#'
#' @param mu an integer
#' @param sigma an integer
#' @param a an integer
#'
#' @returns a plot of the density with the region less than a shaded and the probability of the shaded region.
#' @export
#'
#' @examples myncurve(5,1,5)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))


  prob = pnorm(a, mu, sigma)
  xcurve = seq(mu-3*sigma,a,length = 1000)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(x=c(mu-3*sigma, xcurve, a), y=c(0,ycurve,0),c="RED")
  prob2 = round(prob,4)
  list(mu = mu, sigma = sigma, probability = prob2)

}

