
nomogramEx <- function(nomo=nomo,np=2,digit=9){

  polyfitN=function(x,y,maxN){
    error <- data.frame(matrix(rep(0,2*maxN),ncol=2))
    names(error) <- c("n","error")
    for(n in 1:maxN){
      p <- polyfit(x,y,n)
      yf <- polyval(p,x)
      error[n,] <- c(n,sum((yf-y)^2))
    }
    error <- error[order(error[,2]),]
    return(error)
  }

  list0=list(RESULT="The equation of each variable as follows:")
  nv=length(names(nomo))-np-2

  for(nvi in 1:nv){

    temp=nomo[[nvi]]

    if(length(temp[[1]])<=5){
      rst=data.frame(temp$points)
      names(rst)=names(nomo)[nvi]
      list0=c(list0,list(rst))
    }

    if(length(temp[[1]])>5){
      points=data.frame(temp$points)
      x=temp[[1]]
      y=points[[1]]
      pfN=polyfitN(x,y,maxN=3)
      if(pfN[1,1]==1){
        beta <- polyfit(x,y,1)
        beta <- round(beta,digit)
        rst=paste("points =",beta[1],"*",names(nomo)[nvi],"+",
                  beta[2])
        list0=c(list0,list(rst))
      }
      if(pfN[1,1]==2){
        beta <- polyfit(x,y,2)
        beta <- round(beta,digit)
        rst=paste("points =",beta[1],"*",names(nomo)[nvi],"^2 +",
                  beta[2],"*",names(nomo)[nvi],"+",
                  beta[3])
        list0=c(list0,list(rst))
      }
      if(pfN[1,1]==3){
        beta <- polyfit(x,y,3)
        beta <- round(beta,digit)
        rst=paste("points =",beta[1],"*",names(nomo)[nvi],"^3 +",
                  beta[2],"*",names(nomo)[nvi],"^2 +",
                  beta[3],"*",names(nomo)[nvi],"+",
                  beta[4])
        list0=c(list0,list(rst))
      }
    }#if(length(temp[[1]])>5){

  }#for(nvi in 1:nv){

  tnv=length(names(nomo))
  for(tnvi in (tnv-np+1):tnv){
    temp=nomo[[tnvi]]
    x=as.numeric(temp$x)
    y=as.numeric(temp$x.real)
    pfN=polyfitN(x,y,maxN=3)
    if(pfN[1,1]==1){
      beta <- polyfit(x,y,1)
      beta <- round(beta,digit)
      rst=paste(names(nomo)[tnvi],"=",beta[1],"* points +",
                beta[2])
      list0=c(list0,list(rst))
    }
    if(pfN[1,1]==2){
      beta <- polyfit(x,y,2)
      beta <- round(beta,digit)
      rst=paste(names(nomo)[tnvi],"=",beta[1],"* points ^2 +",
                beta[2],"* points +",
                beta[3])
      list0=c(list0,list(rst))
    }
    if(pfN[1,1]==3){
      beta <- polyfit(x,y,3)
      beta <- round(beta,digit)
      rst=paste(names(nomo)[tnvi],"=",beta[1],"* points ^3 +",
                beta[2],"* points ^2 +",
                beta[3],"* points +",
                beta[4])
      list0=c(list0,list(rst))
    }
  }#for(i in (tnv-np+1):tnv){

  return(list0)
}
