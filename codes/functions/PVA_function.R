# Population viability function
# based on the script extracted from the course online, link below
# https://www.youtube.com/live/b08d1BBqf5E?feature=share
# Author: Jon Went
# Date: 01.08.2023

# ----

extract_N <- function(df){
  N <- df$tot_abund
  return(N)
}

extract_Years <- function(df){
  Years <- df$year
  return(Years)
}

calculate_lambda <- function(N, Years){
  
  # Population growth rate (lambda)
  lambda <- N[-1]/N[-length(N)]
  
  # Intrinsic rate of increase
  logN <- log(N[-1]/N[-length(N)])  #  this is the log of lambda (population growth rate)
  
  # mean rate of increase
  mu <- mean(logN, na.rm = TRUE)  # lower than zero, so the population is declining and will die out
  
  # environmental variation
  sigma2 <- var(logN, na.rm = TRUE)   # the higher, the higher the risk of extinction
  
}

project_population <- function(N, mu, sigma2, 
                               t.iterations = 50, n.runs = 500,
                               quasi_extinction = 30){
  n0 <- N[1] # accumulated (sum) number of individuals in 2000
  T <- t.iterations # time iterations to project over
  runs <- n.runs # number of population trajectories
  stoch.pop <- matrix(NA, T, runs)
  stoch.pop[1,] <- n0 
  
  # set the quasi-extinction threshold
  Ne <- quasi_extinction # check in the literature
  
  # projections
  
  for (i in 1:runs){
    for (t in 2:T){
      r <- rnorm(n = 1, mean = mu, sd = sqrt(sigma2))
      lambda.now <- exp(r)
      stoch.pop[t,i] <- stoch.pop[(t-1),i] * lambda.now
      if(stoch.pop[t,i] <= Ne) {
        stoch.pop[t,i] <- 0
        i < i+1}
    }
  }
  
  return(stoch.pop)
}

calculate_extinction_risk <- function(stoch.pop_df, N, mu, sigma2,
                                      t.iterations = 50,
                                      n.runs = 500, quasi_extinction = 30){
 
  T <- t.iterations
  lastN <- data.frame(pop = stoch.pop_df[T,])
  Ne <- quasi_extinction
  n0 <- N[1]
  
  
  # probability to reach the extinction criteria
  Pr.ext <- (sum(lastN <= Ne) / n.runs)*100  # percentage of trajectories reaching the extinction threshold before the end of the simulation
  
  # Cumulative extinction probability per time step
  ex = extCDF(mu, sigma2, Nc=n0, Ne=Ne)
  
  # Use bootstrap to get confidence intervals
  CIext <- countCDFxt(mu, sigma2, nt=T-1, Nc=n0, Ne=Ne, tmax=T, Nboot=500, plot=TRUE)
  Prext <- data.frame(years=(bird.now$year[1]:(bird.now$year[1] + (T-1))), 
                      m=CIext$Gbest,
                      low=CIext$Glo,
                      up=CIext$Gup)
  
  return(Prext)
}

calculate_time_to_extinction <- function(stoch.pop, 
                                         n.runs = 500){
  
  # Time to reach extinction for extinct population
  maxt<-NULL # empty vector to store results
  for(i in 1:n.runs){
    N = stoch.pop[,i]
    maxt[i] <- max(which(N>0))
  }
  
  # Time to reach extinction for pseudo-extinct population
  time.ext <- maxt[maxt<T]
  
  df <- data.frame(time.ext=time.ext)
  return(df)
  
}


apply_PVA <- function(df) {
  df_processed <- df %>%
    group_by(animal_jetz, cluster.tmp) %>%
    filter(!any(extract_N(.) == 0) & all(lengths(extract_Years(.)) >= 10)) %>%
    summarise(N = list(extract_N(data)), Years = list(extract_Years(data)), .groups = "drop") %>%
    mutate(lambda_results = map2(N, Years, ~calculate_lambda(N = .x, Years = .y)),
           stoch_pop = map(lambda_results, ~project_population(N = .x[-1], mu = .$mu, sigma2 = .$sigma2)),
           extinction_risk = map(stoch_pop, ~calculate_extinction_risk(stoch.pop_df = .x, 
                                                                       N = .$N[-1], 
                                                                       mu = .$mu, 
                                                                       sigma2 = .$sigma2)),
           time_to_extinction = map(stoch_pop, ~calculate_time_to_extinction(stoch.pop = .x)))
  
  return(df_processed)
}
