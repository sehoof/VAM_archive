install.packages("randomizr")
install.packages("estimatr")
install.packages("ri2")
install.packages("tidyverse")

library(randomizr)
library(estimatr)
library(ri2)
library(tidyverse)

rm(list=ls())

setwd("~/Dropbox/Teaching King's/Teaching/Spring 2019/Workshop LSE")

dat<-data.frame(Z = c(1, 0, 1, 0, 0, 1), Y=c(4.5, 5, 4.5, 4.5, 4, 6))

Z <- dat$Z
Y <- dat$Y

?conduct_ri

set.seed(1) #set random number seed

ra_declaration <- declare_ra(N = 6, m = 3) #declare your random assignment

ra_declaration

prob_mat <- ra_declaration$probabilities_matrix
head(prob_mat)

fit_ri <- conduct_ri(Y ~ Z, declaration = ra_declaration, sims=20, data = dat) #conduct all possible assignments and DiM under the sharp null

plot(fit_ri) #plot the sampling distribution of the estimated ATEs
summary(fit_ri) #two-tailed p-value

plot(fit_ri,p = "upper")
summary(fit_ri, p = "upper") #one-tailed p-value

#save plots
pdf(paste("ri2.pdf"),w=6,h=5)
plot(fit_ri)
dev.off()

png("ri2.png")
plot(fit_ri)
dev.off()


#Use replication data from Foos and de Rooij (AJPS, 2017)

install.packages(readstata13)
library(readstata13)

dat2<-read.dta13("canvassing.dta")

dat2$Z <- dat2$treatment

dat2$turnout12<-NA
dat2$turnout12[dat2$turnout=="voted"]<-1
dat2$turnout12[dat2$turnout=="did not vote"]<-0

dat2$Y <- dat2$turnout12

dat2$blocks <- dat2$pid

with(dat2, table(blocks, Z))

block_m<- with(dat2, tapply(Z,blocks,sum))

set.seed(1) #set random number seed

ra_declaration2 <- declare_ra(N = 3848, blocks=dat2$blocks, block_m = block_m) #declare block random assignment

ra_declaration2

fit_ri2 <- conduct_ri(Y ~ Z, declaration = ra_declaration2, sims=5000, IPW=TRUE, data = dat2)

plot(fit_ri2) #plot the sampling distribution of the estimated ITTs
summary(fit_ri2) #two-tailed p-value