# draw the normal curve with shaded area
snorm <- function(from, to, col="red", main="Area bajo la curva",method="z",df=30){
  if(method=="t"){
    curve(dt(x,df=df), xlim=c(-5,5), main=main, ylim=c(0,0.5))
  }else{
    curve(dnorm(x,0,1), xlim=c(-5,5), main=main, ylim=c(0,0.5))
  }
  
  
  for(i in 1:length(from)){
    # define shaded region
    from.z <- from[i]
    to.z <- to[i]
    S.x  <- c(from.z, seq(from.z, to.z, 0.01), to.z)
    if(method=="t"){
      S.y  <- c(0, dt(seq(from.z, to.z, 0.01),df=df), 0)
    }else{
      S.y  <- c(0, dnorm(seq(from.z, to.z, 0.01)), 0)
    }
    
    polygon(S.x,S.y, col=col)
  }
  
}
#draw normal curve with left tail shaded below "to"
Ltail <- function(to, col="red", main="Area bajo la curva",method="z",df=30){
  snorm(-5,to, col=col, main=main, method=method, df=df)
}
#draw normal curve with right tail shaded above "from"
Rtail <- function(from, col="red", main="Area bajo la curva",method="z",df=30){
  snorm(from,5, col=col, main=main, method=method, df=df)
}
#draw normal curve with left and right tail shaded beyond "z" (positive)
Ttail <- function(z, col="red", main="Area bajo la curva",method="z",df=30){
  snorm(c(z,-5),c(5,-z), col=col, main=main, method=method, df=df)
}
Btail <- function(from, to, col="red", main="Area bajo la curva",method="z",df=30){
  snorm(from,to, col=col, main=main, method=method, df=df)
}

