library(Rmixmod)

set.seed(98)
simulated.data<- data.frame(doy= c(rnorm(1000, mean= 3, sd= 1), rnorm(2000, mean= 6, sd= 0.8)))
hist(simulated.data$doy, nclass= 50)

nclass<- 2

dat4EM1<- na.omit(simulated.data[, c("doy")]) # making a copy with just the relevant covariates (not essential)

DD.EM1 <- mixmodCluster(dat4EM1, nbCluster= nclass, 
	models = mixmodGaussianModel(), criterion= c("BIC","ICL"), 
	strategy= mixmodStrategy(algo= "EM", nbTry= 1, 
		initMethod= "smallEM", nbTryInInit= 50, 
		nbIterationInInit= 5, nbIterationInAlgo= 200, 
		epsilonInInit= 0.001, epsilonInAlgo= 0.001), 
	seed= 45)

# show a summary of the best model containing the estimated parameters , the likelihood
summary(DD.EM1)
# note that the order of the clusters is random
# but the simulated parameters are reasonably well estimated

# plot estimated model
hist(DD.EM1)


## In case of interest:


# Can do our own plot to compare estimated with data-generating model
hist(simulated.data$doy, nclass= 50, freq= F)
x.seq<- seq(-4, 15, l= 200) # create a regular sequence along the x-axis

# add the true generating distributions from which data were simulated:
lines(x.seq, dnorm(x.seq, mean= 3, sd= 1) * 1/3, col= 2)
lines(x.seq, dnorm(x.seq, mean= 6, sd= 0.8) * 2/3, col= 3)

# add the estimated distributions:
	# (really awkward parameter extraction from these models: 
	# surely there must be a better way!)
prop.Cluster1<- DD.EM1@bestResult@parameters@proportions[1] 
prop.Cluster2<- DD.EM1@bestResult@parameters@proportions[2]

lines(x.seq, dnorm(x.seq, 
					mean= DD.EM1@bestResult@parameters@mean[1, ],
					sd= DD.EM1@bestResult@parameters@variance[[1]][1, 1])
					* prop.Cluster1, 
			col= 2, lwd= 2, lty= 2)

lines(x.seq, dnorm(x.seq, 
					mean= DD.EM1@bestResult@parameters@mean[2, ],
					sd= DD.EM1@bestResult@parameters@variance[[2]][1, 1])
					* prop.Cluster2,
			col= 3, lwd= 2, lty= 2)


# extract the cluster assignment probabilities for each observation (obs in rows, clusters in columns)
head(DD.EM1@bestResult@proba)

# most likely cluster membership for each observation (as predicted by the model)
dat4EM1$EM.class.2<- (DD.EM1@bestResult@partition)
