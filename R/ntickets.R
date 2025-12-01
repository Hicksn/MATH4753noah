#' ntickets
#'
#' @param N A positive integer representing the number of seats available
#' @param gamma a number between 0 and 1 representing the probability of overbooking
#' @param p a number between 0 and 1 representing the probability of a ticket buyer showing up
#'
#' @returns two plots of the discrete and continuous solutions and a list with the solutions
#' @export
#'
#' @examples n = ntickets(400,0.02,0.95)
ntickets<-function(N,gamma,p){
  # getting the x values around the amount of tickets needed
  xx <- seq(N,(N+(30)),by=1)
  # objective of the discrete distribution
  y = 1-pbinom(N,xx,p)-gamma


  index = which.min(abs(y))
  nd = (index-1) + N

  plot(xx,y,
       main=paste("Objective Vs n to find optimal tickets sold \n (",nd,") gamma=",gamma," N=",N," discete",sep=""),
       type="p", xlab="n", ylab="Objective",col="Blue", cex=0.1,lwd=5)
  lines(xx,y,lty=2,lwd=1)
  abline(h=y[index],col="Red")
  abline(v=nd, col="Red")



  f <- function(x){
    1-pnorm(N+0.5,x*p,sqrt(x*p*(1-p)))-gamma
  }

  op = optimize(function(x)
    abs(1-pnorm(N+0.5,x*p,sqrt(x*p*(1-p)))-gamma)
    ,interval=c(N,N+30))
  nc = op$minimum

  curve(f, xlim=c(N,N+30), main=paste("Objective Vs n to find optimal tickets sold \n (",nc,") gamma=",gamma," N=",N," continuous",sep=""),
        xlab="n", ylab="Objective")
  abline(v=nc,h=f(nc), col="Red")

  list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
}

