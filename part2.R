library(datasets)
data(ToothGrowth)
dim(ToothGrowth)

head(ToothGrowth)

summary(ToothGrowth)

c(round(mean(ToothGrowth$len),3) , round(sd(ToothGrowth$len),3),round(var(ToothGrowth$len),3))

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
summary(ToothGrowth)

library(ggplot2)

ggplot(ToothGrowth,aes(x=factor(dose),y=len,fill=factor(dose))) + 
  geom_boxplot(notch=F) +
  facet_grid(.~supp) +
  scale_x_discrete("Dosage (mg)") +   
  scale_y_continuous("Tooth Length") +  
  scale_fill_discrete(name="Dose (mg)") + 
  ggtitle("Effect of Supplement Type and Dosage on Tooth Growth")

x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == 0.5]
y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose == 0.5]
d05 <- c(round(mean(x),2),
         (round(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)),2)),
         round(mean(y),2),
         (round(mean(y) + c(-1,1) * qnorm(0.975) * sd(y)/sqrt(length(y)),2)))

x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == 1]
y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose == 1]
d10 <- c(round(mean(x),2),
         (round(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)),2)),
         round(mean(y),2),
         (round(mean(y) + c(-1,1) * qnorm(0.975) * sd(y)/sqrt(length(y)),2)))

x <- ToothGrowth$len[ToothGrowth$supp=="OJ" & ToothGrowth$dose == 2]
y <- ToothGrowth$len[ToothGrowth$supp=="VC" & ToothGrowth$dose == 2]
d20 <- c(round(mean(x),2),
         (round(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)),2)),
         round(mean(y),2),
         (round(mean(y) + c(-1,1) * qnorm(0.975) * sd(y)/sqrt(length(y)),2)))


