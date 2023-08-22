#========================#
# COUNT BASED PVA MODEL  #
#========================#

# https://www.youtube.com/live/b08d1BBqf5E?feature=share

# Limitations population viability analyses:
  # No sampling error: Population size is known (no issues on detectability)
  # No density-dependence (exponential growth)
  # No info about mechanisms governing population dynamics (e.g. is fecundity affecting growth?)  
  # Only environmental stochasticity is considered in environmental variation
        # no demographic stochasticity
        # no trend in variance or mean in environmental conditions over time (variation was random) 
        # no correlation of population size among successive years (yet bad years can affect subsequent years and affect viability)

# Advantages:
  # Simple
  # Only 10 years of data are enough
  # When population is not small, we can assume no effect of demographic stochasticity
  # Assess model accuracy by hindcasting (make predictions for a period in the past where the actual outcomes are already known)



# libraries
# install.packages("popbio")
library(popbio)
library(ggplot2)

# data
rm(list=ls())

View(grizzly)
N <- grizzly$N
Years <- grizzly$year


#     Step 0: Plot the data          #
#====================================#

ggplot(data=grizzly, aes(x=year, y=N)) +
  geom_line(, size = 1, color= "#00AFBB") +
  geom_point(color="#00AFBB") +
  ylab("Population size") +
  xlab("Time steps")



# Step 1: Calculate "lambda", "mu" and "sigma" #
#==========================================#

# Population growth rate (lambda)
lambda <- N[-1]/N[-length(N)]

# Intrinsic rate of increase
logN <- log(N[-1]/N[-length(N)])  #  this is the log of lambda (population growth rate)

# mean rate of increase
mu <- mean(logN)  # lower than zero, so the population is declining and will die out

# environmental variation
sigma2 <- var(logN)   # the higher, the higher the risk of extinction



# Step 2: Project the population #
#================================#

n0 <- N[1] # accumulated (sum) number of individuals in 2000
T <- 50 # time iterations to project over
runs <- 500 # number of population trajectories
stoch.pop <- matrix(NA, T, runs)
stoch.pop[1,] <- n0 

# set the quasi-extinction threshold
Ne <- 30 # check in the literature

# projections

for (i in 1:runs){
    for (t in 2:T){
      r <- rnorm(n = 1, mean = mu, sd = sqrt(sigma2))
      lambda <- exp(r)
      stoch.pop[t,i] <- stoch.pop[(t-1),i] * lambda
      if(stoch.pop[t,i] <= Ne) {
        stoch.pop[t,i] <- 0
        i < i+1}
    }
}



# Step 3: Examine the results    #
#================================#


# Plot population size trajectories
matplot(log(stoch.pop), type="l",
        xlab="log Population size",
        ylab="Time steps")


# Plot population size at the last time step
lastN <- data.frame(pop = stoch.pop[T,])
summary(lastN)
ggplot(lastN, aes(x = pop)) +
  geom_histogram(bins = 40) +
  xlab("Population size after 50 years")


# Mean population size simulations with confidence intervals
pop.mean <- apply(stoch.pop,1,mean, na.rm=T)
log.pop.sd <- apply(log(stoch.pop+0.00001),1,sd,na.rm=T)
ucl <- exp(log(pop.mean)+1.96*log.pop.sd)  # upper confidence interval
lcl <- exp(log(pop.mean)-1.96*log.pop.sd)  # lower confidence interval

dataproj <- data.frame(years=(grizzly$year[1]:(grizzly$year[1] + (T-1))), 
                       pop.mean=pop.mean,
                       low=lcl,
                       up=ucl,
                       N=c(grizzly$N, rep(NA, T-length(grizzly$N))))

p <- ggplot(dataproj, aes(x = years)) +
  geom_line(aes(y = log(pop.mean))) +
  geom_ribbon(aes(ymin = log(low), ymax = log(up)), alpha = 0.2)

p + geom_line(aes(y=log(N), colour="red")) +
  ylab("log population size") +
  xlab("Time steps")



# Step 4: Quantify extinction risk  #
#===================================#

# probability to reach the extinction criteria
Pr.ext <- (sum(lastN <= Ne) / runs)*100  # percentage of trajectories reaching the extinction threshold before the end of the simulation
Pr.ext

# Cumulative extinction probability per time step
ex = extCDF(mu, sigma2, Nc=n0, Ne=Ne)

# Use bootstrap to get confidence intervals
CIext <- countCDFxt(mu, sigma2, nt=T-1, Nc=n0, Ne=Ne, tmax=T, Nboot=500, plot=TRUE)
Prext <- data.frame(years=(grizzly$year[1]:(grizzly$year[1] + (T-1))), 
                    m=CIext$Gbest,
                    low=CIext$Glo,
                    up=CIext$Gup)


# Plot the accumulative extinction risk
ggplot(Prext, aes(x=years)) +
    geom_point(aes(y=m)) +
      geom_line(aes(y=m)) +
        geom_ribbon(aes(ymin=low, ymax=up), alpha=0.2) +
            xlab("Years") + ylab("Quasi-extinction probability")



# Step 5: Estimate time to extinction  #
#======================================#

# Time to reach extinction for extinct population
maxt<-NULL # empty vector to store results
for(i in 1:runs){
  N = stoch.pop[,i]
  maxt[i] <- max(which(N>0))
}


# Time to reach extinction for pseudo-extinct population
time.ext <- maxt[maxt<T]
summary(time.ext)
median(time.ext)  # median time at extinction for extinct population; the mean produces overestimation of extinction time because of few populations growing fast 

# Plot time to extinction
df <- data.frame(time.ext=time.ext)
ggplot(df, aes(x = time.ext)) +
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept=median(time.ext)), colour="red") +
  xlab("Time to extinction")



# Step 6: Perturb and re-run the model #
#======================================#

# Initial population size
# Extinction threshold
# Amount of environmental variance
# Number of time steps