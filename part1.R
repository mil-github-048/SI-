library(ggplot2)
library(knitr)

no_simulation <- 1000   # number of simulations 
lambda <-  0.2 
n <- 40             # sample size


simulated_data <- matrix(rexp(n= no_simulation*n,rate=lambda), no_simulation, n)
sample_mean <- rowMeans(simulated_data)

actual_mean <- mean(sample_mean) 
theoretical_mean <- 1/ lambda

result1 <-data.frame("Mean"=c(actual_mean,theoretical_mean), 
                     row.names = c("Mean from the samples ","Theoretical mean"))

result1


sampleMean_data <- as.data.frame (sample_mean)

ggplot(sampleMean_data, aes(sample_mean))+geom_histogram(alpha=.5, position="identity", col="black")+geom_vline(xintercept = theoretical_mean, colour="red",show.legend=TRUE)+geom_vline(xintercept = actual_mean, colour="green", show.legend=TRUE)+ggtitle ("Histogram of the sample means ")+xlab("Sample mean")+ylab("Density")

actual_variance <- var(sample_mean) 

theoretical_variance <- (1/ lambda)^2 /n 

result2 <-data.frame("Variance"=c(actual_variance, theoretical_variance), 
                     row.names = c("Variance from the sample ","Theoretical variance"))
ggplot(sampleMean_data, aes(sample_mean))+
  geom_histogram(aes(y=..density..), alpha=.5, position="identity", fill="white", col="black")+
  geom_density(colour="red", size=1)+
  stat_function(fun = dnorm, colour = "green", args = list(mean = theoretical_mean, sd = sqrt(theoretical_variance)))+
  ggtitle ("Histogram of sample means with the fitting normal curve ")+
  xlab("Sample mean")+
  ylab("Density")
qqnorm(sample_mean, main ="Normal probability plot")
qqline(sample_mean,col = "3")