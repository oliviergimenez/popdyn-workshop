#############################################+
## Class 3 - Live Demo 1 - Sarah Cubaynes for the Team - March 2022
## Count based PVA
## Grizzly bear example
#############################################+

## from Morris & Doak. 2002. Quantitative conservation biology: Theory and practice of population viability analysis. Massachusetts, USA.
## data originally published in from Eberhardt et al (1986) and Haroldson et al 1999.

# load package
library(popbio)
library(tidyverse)

# Look at the data 
View(grizzly)

# Rename objects
N <- grizzly$N
Years <- grizzly$year

# Plot the data
ggplot(data = grizzly, aes(x = year, y = N))+
  geom_line(color = "#00AFBB", size = 1)+
  geom_point(color = "#00AFBB", size=2) +
  theme_gray(base_size = 12)


# Calculate mean rate of increase (mu) and environmental variance (sigma2)----
# rate of increase over years
logN <- log(N[-1]/N[-length(N)]) # log(Nt+1) - log(Nt)
#mean rate of increase
mu <- mean(logN) 
mu
#environmental variance
sigma2 <- var(logN) 
sigma2

# back-trasform to get average population growth rate (lambda)
lambda <- exp(mu) 
lambda 

# Project the population----
# Here, we want to project the population from 1959 over 50 years (until 2009)
n0 <- grizzly$N[1] # initial population size in 1959
n0

T <-  50 # Time steps to project over

# number of repetitions (pop. trajectories)
runs <- 500  

# Empty matrix to store resuts       
stoch.pop <- matrix(NA,T,runs) 
stoch.pop[1,] <- n0 # initiate

# Set quasi-extinction threshold
Ne <- 30

# let's project the population
for (i in 1:runs){ #<<
  for (t in 2:T){	#<< 
    # Draw r from normal using estimates of mu and sigma2
    r <- rnorm(1,mu,sqrt(sigma2)) 
    # back-transform to get lambda and get pop. size
    lambda <- exp(r) 
    #project one time step from the current pop size
    stoch.pop[t,i]=stoch.pop[(t-1),i]*lambda 
    # leave the loop if pop <= threshold
    if(stoch.pop[t,i]<=Ne){ 
      stoch.pop[t,i] <- 0 
      i < i+1}  
  }
}

# Look at the result----
View(stoch.pop)

# Plot the population trajectories
matplot(log(stoch.pop),
        type = "l",
        ylab = "log(population size)",
        xlab = "Time steps")

# Plot population size at the last time step
lastN <- data.frame(pop = stoch.pop[T,])
summary(lastN)

ggplot(lastN, aes(x=pop)) +
  geom_histogram(bins = 40) +
  xlab("YNP grizzly population size after T=38 years")

## Mean population size with confidence interval
pop.mean <- apply(stoch.pop,1,mean, na.rm=T)
log.pop.sd <- apply(log(stoch.pop+0.00001),1,sd, na.rm=T)
ucl <- exp(log(pop.mean) + 1.96 * log.pop.sd)     #upper confidence limit
lcl <- exp(log(pop.mean) - 1.96 * log.pop.sd)     #lower confidence limit

dataproj <- data.frame(year = grizzly$year[1] : (grizzly$year[1] + (T-1)),
                       pop.mean = pop.mean,
                       low = ucl,
                       up = lcl,
                       N = c(grizzly$N, rep(NA, T-length(grizzly$N) )))

# Plot data and projected population size with 95% confidence interval
p <- ggplot(dataproj, aes(year)) + 
  geom_line(aes(y=log(pop.mean)), colour="blue") + 
  geom_ribbon(aes(ymin=log(low), ymax=log(up)), alpha=0.2)
p +  geom_line(aes(y=log(N)), colour="red") +
  geom_point(aes(y=log(N)), colour="red") +
  ylab("log(Popiulation size)") +
  xlab("Time steps")


# prob. to reach the extinction threshold
Pr.ext <- sum(lastN <= Ne) / runs 
Pr.ext

# Cumulative extinction function
# give cumulative extinction probability per time step
ex <- extCDF(mu, sigma2, Nc=n0, Ne=Ne)
ex

# use bootstrap to get confidence intervals
CIext <- countCDFxt(mu, sigma2, nt = T-1, Nc = n0, Ne = Ne, tmax = T, Nboot = 500,
                    plot = FALSE)

Prext <- data.frame(years = (grizzly$year[1] : (grizzly$year[1] + (T-1) ) ) , 
                    m = CIext$Gbest,
                    low = CIext$Glo,
                    up = CIext$Gup)
Prext # mean cumulative extinction risk with confience interval each year

# Plot of cumulative extinction risk
ggplot(Prext, aes(x= years)) +
  geom_point(aes(y=m), colour="purple") +
  geom_line(aes(y=m), colour="purple") + 
  geom_ribbon(aes(ymin=low, ymax=up), alpha=0.2) +
  xlab("Years") +
  ylab("Quasi-extinction probability")

# Time to reach extinction for extinct pop.
maxt <- NULL	# empty vector to store results
for (i in 1:runs){	# loop over repetitions
  N <- stoch.pop[,i]
  # max time N > threshold
  maxt[i] <- max(which(N>0)) } #<<
# time at extinction for pseudo-extinct populations
time.ext <- maxt[maxt<T] #<<
summary(time.ext) # time step when each extinct pop reached the quasi-extinction threshold
median(time.ext) # median time at extinction for extinct pop

# Plot time to extinction
df <- data.frame(time.ext = time.ext) 
ggplot(df,aes(x=time.ext)) +
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept = median(time.ext)), colour="red") +
  xlab("Time to extinction") +
  annotate(geom="text", x = 15, y = 15,
           label=paste( median(time.ext)),
           color="red",
           size=4) 


#############################################+
## Class 3 - Live Demo 2 - Sarah Cubaynes for the Team - March 2022
## Deterministic MPM and sensitivity analysis
## Barn swallow example
#############################################+


# Demographic parameters----
s0 <- 0.2 # first year survival of chicks
s1 <- 0.5 # yearling survival
s2 <- 0.65 # adult survival
f1 <- 1.5 # females produced per yearling female
f2 <- 3 # females produced per adult female

# Leslie matrix----  
# pre-breeding life cycle (with 2 ages classes (a1 = yearlings, a2 = adults)
A.swallow <- matrix(c(s0*f1, s0*f2, 
                      s1, s2),
                    nrow = 2, 
                    byrow = TRUE) 
A.swallow

# Project the population----
library(popbio) # load package

n0 <- c(50,30) # initial population
n0

t <- 11 # number of time steps (years here) to project over
# run the projections
results <- pop.projection(A.swallow, n0, iterations = t)

# Look at the results----
names(results)  

# pop size par âge
pop.age  <- t(results$stage.vectors) 
pop.age 

# total pop size
N <- results$pop.sizes 
N

# Plot the population size
matplot(1:t,pop.age,type='l',xlab = "Time",
        ylab = "Population size", cex = 1.5,
        cex.main = 1.5, cex.lab = 1.5, lwd=2.5,
        cex.axis = 1.5,lty=c(1,1), col = c("#3333FF","purple"),
        main = "Age- structured population projection", las=TRUE) 
legend("topleft", 
       legend = c("age 1","age 2"),
       lty = c(1,1), 
       col = c("#3333FF","purple"),
       bty = "n",cex = 1.2,lwd=2.5)  

# or directly get quantities in stable state
lambda(A.swallow) #stable population growth rate
stable.stage(A.swallow) #stabel age-structure
reproductive.value(A.swallow) # reproductive values
generation.time(A.swallow) # average time between generations
# corresponds to average age of mother at first reproduction


# Pertub the model : sensitivity analyses----
swallow.param <- list(s0 = 0.20, # list the demographic parameters
                      s1 = 0.5,
                      s2 = 0.65,
                      f1 = 3/2,
                      f2 = 6/2)

swallow.equation <- expression( s0 * f1, s0 * f2, s1, s2) # give the matrix equation
VS <- vitalsens(swallow.equation, swallow.param) #run the sensitivty analysis
VS # results of the sensitivity analysis (sensitivity and elasticty values fassociated with each demographic parameter)

# Plot sensitivity and elasticity values
barplot(t(VS[,2:3]), beside=TRUE, las=1, xlab="Demographic parameter",
        main="",
        col=c("#3333FF","purple"))
legend(x="topright", rownames(t(VS[,2:3])), fill=c("#3333FF","purple"),bty = "n")
abline(h=0)



#############################################+
### Class 3 Live Demo 3 - Sarah Cubaynes for the Team - March 2022
### MPM with environmental stochasticity
### Crested newts example----
#############################################+

# Load package
library(popbio) # package to run projections

# Define demographic parameters----
s0 <- 0.42 # first year survival of newborn
s1 <- 0.48 # immature survival
s2 <- 0.52 # subadult survival
s3 <- 0.52 # adult survival
alpha2 <- 0.8 # subadult breeding prob.
alpha3 <- 1 # adult breeding prob.
f <- 3.07 # fecundity (nb of females produced / female)

# Deterministic Leslie matrix----
# pre-breeding cycle (a1 = immatures, a2 = subadults, a3 = adults)
A.newt <- matrix( c(0, s0 * alpha2 * f, s0 * alpha3 * f,
                    s1, 0, 0, 
                    0, s2, s3) , 
                  nrow = 3, 
                  ncol= 3, 
                  byrow = TRUE)
A.newt

# Environmental stochasticity----

###### 1 - Include annual variation around mean values by drawing from normal distributions----
s0stoch <- rnorm(n=5000,mean=s0,sd=0.15)
s1stoch <- rnorm(n=5000,mean=s1,sd=0.15)
s2stoch <- rnorm(n=5000,mean=s2,sd=0.15)
s3stoch <- rnorm(n=5000,mean=s3,sd=0.15)
fstoch <- rnorm(n=5000,mean=f,sd=0.5)

# Keep only survival values between 0 and 1
s0stoch <- s0stoch[s0stoch >0 & s0stoch <1 ]
s1stoch <- s1stoch[s1stoch >0 & s1stoch <1 ]
s2stoch <- s2stoch[s2stoch >0 & s2stoch <1 ]
s3stoch <- s3stoch[s3stoch >0 & s3stoch <1 ]
ns <- min(length(s0stoch),length(s1stoch),length(s2stoch),length(s3stoch))

# Plot the values generated
par(mfrow=c(2,2))
hist(s1stoch, main="Immature survival",xlim=c(0,1))
abline(v=s1, col="blue")
hist(s2stoch, main="Subadult survival",xlim=c(0,1))
abline(v=s2, col="blue")
hist(s3stoch, main="Adult survival")
abline(v=s3, col="purple")
hist(fstoch, main="Fecundity")
abline(v=f, col="purple")

## Create stochastic transition matrices
# Create a list of matrices
A.newtSE <- list()
# generate the stochastic transition matrices by sampling from distrib. of demo. param.
for(i in 1:ns){
  A.newtSE[[i]] <- matrix( c( 0, 
                              sample(alpha2,1) * sample(fstoch,1) * sample(s0stoch,1),#<<
                              sample(alpha3,1) * sample(fstoch,1) * sample(s0stoch,1),#<<
                              sample(s1stoch,1), 0, 0,#<<
                              0, sample(s2stoch,1), sample(s3stoch,1) ) ,#<<
                           nrow = 3, 
                           ncol= 3)
}

## Run the stochastic projections
T <- 30 # nb. time steps to project over

# project
runSE <- stoch.projection(A.newtSE, # list of stochastic transitions matrices
                          n0 = c(50,50, 50), # initial population
                          tmax = T, # time steps to project over
                          nreps = 1000,  # nb of replicates
                          verbose = FALSE) 
# Look a the results
head(runSE) # last pop. size per age class  ( in column) for each replicate (in row)

# Get total population size
pop.size <- data.frame(pop = apply(runSE, 1, sum))

# Plot population size at the last time step of the projection
ggplot(pop.size, aes(x=pop)) +
  geom_histogram(bins = 40) +
  xlab("Population size at the last time step")

# Get the long-run stochastic growth rate (on log-scale)
lambdastoch <- stoch.growth.rate(A.newtSE, 
                                 maxt = 50000, 
                                 verbose = FALSE)
names(lambdastoch)

lambdastoch$approx # by Tuljapukar's approximation
lambdastoch$sim # by simulation 
lambdastoch$sim.CI # with confidence interval

exp(lambdastoch$approx) # exponentiate to get stochastic growth rate

# Extinction probability
proba.ext <- stoch.quasi.ext(A.newtSE,
                             n0 = c(50,50, 50),# initial population size
                             Nx = 30, # qusi-extinction threshold
                             nreps = 1000,# nb replicates
                             tmax = 50, #nb of time years over which to calculate extinction risk 
                             maxruns = 10, # nb of replicates
                             verbose = FALSE)

proba.ext

# Plot extinction risk through time
par(mfrow=c(1,1))
matplot(proba.ext, xlab = "Years", ylab = "Extinction probability", 
        type = "l", 
        lty = 1, 
        las=TRUE,
        col = rainbow(10))

# mean probability of extinction
proba.ext.mean <- apply(proba.ext,1,mean)
proba.ext.mean[20] # in 20 years


######   2 - Simulate the occurence of catastrophic events----

# Define demographic parameters in normal versus bad years
fgood <- 3.07 # fecundity in normal years
fbad <- 0 # fecundity in years with Spring dryness of the pond

# Create the transition matrix in normal years :
A.newtGood <- matrix( c(0, s0 * alpha2 * fgood, s0 * alpha3 * fgood,
                        s1, 0, 0, 0, s2, s3) , 
                      nrow = 3, ncol= 3, byrow = TRUE)
# Create the transition matrix in bad years :
A.newtBad <- matrix( c(0, s0 * alpha2 * fbad, s0 * alpha3 * fbad,
                       s1, 0, 0, 0, s2, s3) , 
                     nrow = 3, ncol= 3, byrow = TRUE)
#Create a list with both transition matrices
A.newtCATA <- list(good = A.newtGood, bad= A.newtBad)
A.newtCATA

# Set the frequency of occurence of catastrophic events
# Spring dyness of the pond occurs every 3 years in average
freqbad <- 1/3

## Run the projections
stochCATA <- stoch.projection(A.newtCATA,
                              prob = c( (1-freqbad), (freqbad)), # probability to choose from the different matrices
                              n0 = c(50,50,50), # initial population size
                              tmax = 100,# nb of time steps
                              nreps = 1000, # nb of replicates
                              verbose = FALSE)
# look at the results
head(stochCATA) # pop size per age class (columns) per replicate (rows)

# Get the long-run stochastic growth rate
lambdaCATA <- stoch.growth.rate(A.newtCATA, 
                                prob = c( (1-freqbad), (freqbad)), 
                                maxt = 100, 
                                verbose = FALSE)
exp(lambdaCATA$approx)

# Probability of extinction 
proba.extCATA <- stoch.quasi.ext(A.newtCATA, 
                                 prob = c( (1-freqbad), (freqbad)),
                                 n = c(50,15,50), # initial population
                                 Nx = 30,  # Quasi-extinction threshold
                                 nreps = 1000, # nb. of replicates
                                 tmax = 50, # nb. of time steps
                                 maxruns = 10, # nb of repetitions
                                 verbose = TRUE)
proba.extCATA

# Plot extinction probability with time
matplot(proba.extCATA, xlab = "Years", ylab = "Probability of extinction", 
        type = "l", 
        lty = 1, 
        las = TRUE,
        col = rainbow(10))

# Mean extinction probability with time
proba.ext.mean <- apply(proba.extCATA,1,mean)
proba.ext.mean[20] # in 20 years

