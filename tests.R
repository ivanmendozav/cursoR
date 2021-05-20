sample <- rnorm(mean=20,sd=5,n=24)
ordered_sample <- sample[order(sample)]
write.csv(sample,"cuantiles.csv")
n <- length(sample)

sample
ordered_sample

quantile(sample, c(0.25,0.5,0.75), type=6)

q1 <- (n+1)/4 
q2 <- 2*(n+1)/4 
q3 <- 3*(n+1)/4 

ordered_sample[q1] + 0.25*(ordered_sample[q1+1] - ordered_sample[q1])
ordered_sample[q2] + 0.5*(ordered_sample[q2+1] - ordered_sample[q2])
ordered_sample[q3] + 0.75*(ordered_sample[q3+1] - ordered_sample[q3])

p70 <- 70*(n+1)/100
ordered_sample[p70] + 0.5*(ordered_sample[p70+1] - ordered_sample[p70])
quantile(sample, c(0.7), type=6)

p <- 22
(p*4)/(n+1) -> q
quantile(sample, c(0.88), type=6)

