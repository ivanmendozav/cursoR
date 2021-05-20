plot_points <- function(c, quartiles=F, xlab="Points"){
    plot(c,rep(0,length(c)), col="blue",xlab=xlab,ylab="",yaxt="n" )
   if(quartiles){
     points(quantile(c,c(0.25,0.5,0.75)),rep(-1,3),col="red",pch=19)
     points(quantile(c,c(0.25,0.5,0.75)),rep(1,3),col="red",pch=19)
     print(quantile(c,c(0.25,0.5,0.75)))
     
     lines(c(quantile(c,0.25),quantile(c,0.25)),c(-1,1),col="red",pch=19)
     lines(c(quantile(c,0.5),quantile(c,0.5)),c(-1,1),col="red",pch=19)
     lines(c(quantile(c,0.75),quantile(c,0.75)),c(-1,1),col="red",pch=19)
   }
}