\name{nomogramEx}
\alias{nomogramEx}
\title{Extract Equations from a Nomogram}
\description{
A nomogram can not be easily applied,
  because it is difficult to calculate the points or even the survival probability.
The package, including a function of nomogramEx(),
  is to extract the polynomial equations to calculate the points of each variable,
  and the survival probability corresponding to the total points.
}
\keyword{nomogram, survival probability}
\usage{
nomogramEx(nomo,np,digit)
}
\arguments{
  \item{nomo}{a object of nomogram()}
  \item{np}{the number of predicitons in your nomogram, for example: if you predicted 3- and 6- month, np=2, default is 2}
  \item{digit}{the number of decimal digits, default is 9}
}
\value{
\item{list}{the result is a list including polynomial equations to calculate the points of each variable, and the polynomial equations to calculate the probability of points}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
The polynomial equations extracted by this package are equal and less than cubic function.

Update:

Version 1.0: 1.the order of variables in the polynomial equations is opposite. 2.the number of the demical digits can not be controled.

Version 2.0: 1.the argument 'lp' from the 'nomogram' function can not be recognized.
}
\seealso{
nothing
}
\examples{
if(require("rms")){
n <-1000
age <- rnorm(n,50,10)
sex <- factor(sample(c('female','male'),n,TRUE))
sex <- as.numeric(sex)
ddist <- datadist(age,sex)
options(datadist='ddist')
cens <- 15*runif(n)
time <- -log(runif(n))/0.02*exp(.04*(age-50)+.8*(sex=='Female'))
death <- ifelse(time <= cens,1,0)
time <- pmin(time,cens)
units(time)="month"
f <- cph(formula(Surv(time,death)~sex+age),x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
surv <- Survival(f)
nomo <- nomogram(f, fun=list(function(x) surv(3,x),function(x) surv(6,x)),
  lp=TRUE,funlabel=c("3-Month Survival Prob","6-Month Survival Prob"))
nomogramEx(nomo=nomo,np=2,digit=9)
}
}

