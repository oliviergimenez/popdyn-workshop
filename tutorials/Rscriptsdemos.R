################################################################
################################################################
########## R scripts for live demos on ABUNDANCE ###############
################################################################
################################################################

library(tidyverse)


## ---------------------------------------------------------------------------------
library(RMark)


## ---------------------------------------------------------------------------------
mouse <- convert.inp("dat/mouse.inp",
                    group.df = NULL,
                    covariates = NULL)


## ---------------------------------------------------------------------------------
head(mouse)
dim(mouse)[1] # number of individuals captured at least once


## ---------------------------------------------------------------------------------
mouse.proc <- process.data(mouse, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)


## ---------------------------------------------------------------------------------
run.mouse <- function() {
# List of effects 
p.dot <- list(formula =  ~ 1, share = TRUE) # M0 : p constant over time
p.dot.behav <- list(formula =  ~ 1) # Mb : p and c differ
p.time <- list(formula =  ~ time, share = TRUE) # Mt : p varies over time
p.h <- list(formula = ~ mixture, share = TRUE) # Mh : p is heterogeneous between individuals (Pledger's mixture model)
p.time.behav <- list(formula =  ~ time) # Mtb
p.h.behav <- list(formula =  ~ mixture) # Mbh
p.h.time <- list(formula = ~ time + mixture, share = TRUE) # Mth
p.h.time.behav <- list(formula =  ~ mixture + time) # Mtbh

## Build model list
mouse.model.list <- create.model.list("FullHet")

## Prepare list to send to Mark
mouse.results <- mark.wrapper(mouse.model.list,data = mouse.proc,ddl = mouse.ddl)

## Grab results
  return(mouse.results)
}


## ---------------------------------------------------------------------------------
mouse.results <- run.mouse()


## ----eval = FALSE-----------------------------------------------------------------
## mouse.results


## ---------------------------------------------------------------------------------
mouse.results


## ---------------------------------------------------------------------------------
names(mouse.results)


## ---------------------------------------------------------------------------------
mouse.results$p.dot$results$real


## ---------------------------------------------------------------------------------
mouse.results$p.dot$results$derived


## ---------------------------------------------------------------------------------
mouse <- convert.inp("dat/mouse_groups.inp",
                    group.df = data.frame(sex = c("M","F")),
                    covariates = NULL)


## ---------------------------------------------------------------------------------
head(mouse)


## ---------------------------------------------------------------------------------
mouse.proc <- process.data(mouse, 
                           begin.time = 1, 
                           model = "FullHet",
                           groups = "sex")
mouse.ddl <- make.design.data(mouse.proc)


## ---------------------------------------------------------------------------------
run.mouse <- function() {
  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)
  # Mbsex - model with sex on capture probability AND recapture probability
  p.sex.behav <- list(p = list(formula = ~ sex),c = list(formula = ~ sex))
  ## Build model list
  mouse.model.list <- create.model.list("FullHet")
  ## Prepare list to send to Mark
  mouse.results <- mark.wrapper(mouse.model.list,
                              data = mouse.proc, 
                              ddl = mouse.ddl)
  ## Grab results
  return(mouse.results)
}


## ---------------------------------------------------------------------------------
mouse.results <- run.mouse()


## ---------------------------------------------------------------------------------
mouse.results


## ---------------------------------------------------------------------------------
mouse.results$p.sex.behav$results$real


## ---------------------------------------------------------------------------------
mouse.results$p.sex.behav$results$derived


## ---------------------------------------------------------------------------------
rm(list = ls(all = TRUE))
cleanup(ask = FALSE)


## ---------------------------------------------------------------------------------
library(Distance)


## ---------------------------------------------------------------------------------
data(wren_lt)


## ---------------------------------------------------------------------------------
wren_lt


## ---------------------------------------------------------------------------------
dim(wren_lt) 
sum(is.na(wren_lt$distance))  # transect with no observations at all



## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
hist(wren_lt$distance, xlab="Distance (m)", 
     main="Winter wren line transects")



## ---------------------------------------------------------------------------------
conversion.factor <- convert_units("meter", "kilometer","hectare")


## ---------------------------------------------------------------------------------
wren.hn <- ds(data = wren_lt, 
              key = "hn", 
              adjustment = NULL,
              convert_units = conversion.factor)


## ---------------------------------------------------------------------------------
summary(wren.hn)



## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
cutpoints <- c(0,5,10,15,20,30,40,50,65,80,100)
plot(wren.hn, breaks=cutpoints, main="Half normal model, 
     wren line transects")



## ---------------------------------------------------------------------------------
str(wren.hn)


## ---------------------------------------------------------------------------------
wren.hn$ddf



## ---------------------------------------------------------------------------------
wren.hn$dht


## ---- echo=FALSE------------------------------------------------------------------
wren.unif.cos <- ds(wren_lt, key = "unif", adjustment = "cos",
                  convert_units = conversion.factor)
wren.hr.poly <- ds(wren_lt, key = "hr", adjustment = "poly", 
                  convert_units = conversion.factor)


## ---------------------------------------------------------------------------------
AIC(wren.hn, wren.hr.poly, wren.unif.cos)


## ---- echo = TRUE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
z <- gof_ds(wren.hn)


## ----echo=FALSE-------------------------------------------------------------------
z



## ---------------------------------------------------------------------------------
library(unmarked)
library(AICcmodavg)


## ---------------------------------------------------------------------------------
dat <- read.csv2('dat/ocellatedlezard.csv')


## ---------------------------------------------------------------------------------
head(dat)
dim(dat)[1] # number of quadrats


## ---------------------------------------------------------------------------------
y <- dat[,2:4]
head(y)


## ---------------------------------------------------------------------------------
colSums(y)
rowSums(y)


## ---------------------------------------------------------------------------------
covsite <- dat[,5:6]			# collect site covariates

head(covsite) # look at covariates



## ---------------------------------------------------------------------------------
temp <- dat[,7:9]			  # collect temperature at each field session
month <- dat[,10:12]     # collect month of each field session
obscov <- list(temp = temp, month = month)	# make a list with session covariates

str(obscov) # look at the covariates


## ---------------------------------------------------------------------------------
count <- unmarkedFramePCount(y, siteCovs = covsite, obsCovs = obscov)


## ---------------------------------------------------------------------------------
res0 <- pcount(~1 ~1, count, K = 100, mixture = c("P"))


## ---------------------------------------------------------------------------------
str(res0)


## ---------------------------------------------------------------------------------
summary(res0)



## ---------------------------------------------------------------------------------
lambda <- backTransform(res0, type = "state") 
lambda
confint(lambda)


## ---------------------------------------------------------------------------------
detection <- backTransform(res0, type = "det") 
detection
confint(detection)


## ---------------------------------------------------------------------------------
res0a <- pcount(~1 ~1, count, K = 100, mixture = c("P"))
res0b <- pcount(~1 ~1, count, K = 150, mixture = c("P"))
res0c <- pcount(~1 ~1, count, K = 200, mixture = c("P"))


## ---------------------------------------------------------------------------------
print(res0a@AIC, digits = 5)
print(res0b@AIC, digits = 5)
print(res0c@AIC, digits = 5)


## ---------------------------------------------------------------------------------
res_month <- pcount(~month ~1, count, K = 100, mixture = c("P"))


## ---------------------------------------------------------------------------------
summary(res_month)


## ---------------------------------------------------------------------------------
res0@AIC
res_month@AIC


## ---------------------------------------------------------------------------------
newData <- data.frame(month = c('April','May','June'))
pred <- predict(res_month, type = "det", newdata = newData, appendData = TRUE)
pred


## ---------------------------------------------------------------------------------
res_burrow <- pcount(~month ~burrow, count, K = 100, mixture = c("P"))


## ---------------------------------------------------------------------------------
res_month@AIC
res_burrow@AIC



## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
newData2 <- data.frame(burrow=seq(min(dat$burrow),max(dat$burrow),by=1))
E.p <- predict(res_burrow, type="state", newdata=newData2, appendData=TRUE)
plot(Predicted ~ burrow, E.p, type="l", ylim=c(0,10),
xlab="Number of burrow",
ylab="Expected abundance")
lines(lower ~ burrow, E.p, type="l", col=gray(0.5))
lines(upper ~ burrow, E.p, type="l", col=gray(0.5))


## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center', cache = TRUE----
obs.boot <- Nmix.gof.test(res_burrow, nsim = 100)


## ---------------------------------------------------------------------------------
print(obs.boot, digits.vals = 4, digits.chisq = 4)



## ---------------------------------------------------------------------------------
res1 <- pcount(~month ~burrow, count, K = 100, mixture = c("P"))
res2 <- pcount(~month + burrow ~1, count, K = 100, mixture = c("P"))
res3 <- pcount(~month + burrow ~burrow, count, K = 100, mixture = c("P"))
res4 <- pcount(~month ~habitat, count, K = 100, mixture = c("P"))
res5 <- pcount(~month + habitat ~1, count, K = 100, mixture = c("P"))
res6 <- pcount(~month + habitat ~habitat, count, K = 100, mixture = c("P"))


## ---------------------------------------------------------------------------------
fms <- fitList(
"lambda(.)       p(.)" = res0,
"lambda(burrow)  p(month)" = res1,
"lambda(.)       p(month+burrow)" = res2,
"lambda(burrow)  p(month+burrow)" = res3,
"lambda(habitat) p(month)" = res4,
"lambda(.)       p(month+habitat)" = res5,
"lambda(habitat) p(month+habitat)" = res6
)


## ---------------------------------------------------------------------------------
ms <- modSel(fms)
ms




################################################################
################################################################
########## R scripts for live demos on DEMO PARAM ##############
################################################################
################################################################

# rounding 2 decimals, and scientific notation
options(htmltools.dir.version = FALSE,
        scipen = 1, 
        digits = 2)


## ---------------------------------------------------------------------------------
library(RMark)
library(tidyverse)
theme_set(theme_light())

## ---------------------------------------------------------------------------------
dipper_csv <- read_csv("dat/dipper.csv")


## ---------------------------------------------------------------------------------
dipper_csv


## ---------------------------------------------------------------------------------
dipper <- data.frame(ch = unite(dipper_csv %>%
                                  select(year_1981:year_1987), 
                                col = "ch", 
                                sep = ""), 
                     sex = dipper_csv$sex,
                     wglength = scale(dipper_csv$wing_length))


## ---------------------------------------------------------------------------------
head(dipper)


## ----warning=FALSE----------------------------------------------------------------
phi.p <- mark(dipper)


## ---------------------------------------------------------------------------------
str(phi.p, max.level = 0)


## ---------------------------------------------------------------------------------
names(phi.p)


## ---------------------------------------------------------------------------------
phi.p$results$real


## ---------------------------------------------------------------------------------
dipper.proc <- process.data(data = dipper, groups = "sex")


## ---------------------------------------------------------------------------------
dipper.ddl <- make.design.data(dipper.proc)


## ---------------------------------------------------------------------------------
phit <- list(formula=~time) # time 
phi <- list(formula=~1)     # constant
pt <- list(formula=~time)   # time
p <- list(formula=~1)       # constant



## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phit.pt <- mark(dipper.proc,
                dipper.ddl,
                model.parameters = list(Phi = phit, p = pt))


## ---------------------------------------------------------------------------------
phit.pt$results$real



## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phit.p <- mark(dipper.proc,
               dipper.ddl,
               model.parameters = list(Phi = phit, p = p))



## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phi.pt <- mark(dipper.proc,
               dipper.ddl,
               model.parameters = list(Phi = phi, p = pt))


## ---------------------------------------------------------------------------------
collect.models()


## ---------------------------------------------------------------------------------
phisex <- list(formula=~sex)



## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phisex.p <- mark(dipper.proc,
                 dipper.ddl,
                 model.parameters = list(Phi = phisex, p = p))


## ---------------------------------------------------------------------------------
phisex.p$results$beta


## ---------------------------------------------------------------------------------
logitphiF <- phisex.p$results$beta[1,1]
plogis(logitphiF)


## ---------------------------------------------------------------------------------
logitphiM <- logitphiF + phisex.p$results$beta[2,1]
plogis(logitphiM)


## ---------------------------------------------------------------------------------
phisex.p$results$real


## ---------------------------------------------------------------------------------
collect.models()


## ----echo=FALSE, message=FALSE, warning=FALSE-------------------------------------
phisexpt <- list(formula=~sex+time)
phisexpt.p <- mark(dipper.proc,
                   dipper.ddl,
                   model.parameters = list(Phi = phisexpt, p = p))


## ---------------------------------------------------------------------------------
phisexpt.p$results$real


## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
df <- data.frame(year = c(1981:1986, 1981:1986),
                 survival = c(phisexpt.p$results$real[1:6,1],
                              phisexpt.p$results$real[7:12,1]),
                 sex = c(rep("female",6), rep("male", 6))
)
df %>%
  ggplot() + 
  aes(x = year, y = survival, color = sex) +
  geom_line() + 
  labs(color = NULL) + 
  theme_light(base_size = 18) + 
  scale_color_viridis_d(begin = 0, end = 0.7)




## ---------------------------------------------------------------------------------
collect.models()


## ---------------------------------------------------------------------------------
phiwl <- list(formula=~wglength)



## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phiwl.p <- mark(dipper.proc,
                dipper.ddl,
                model.parameters = list(Phi = phiwl, p = p))


## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------
phiwl.p$results$beta



## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
scaled_wl <- scale(dipper_csv$wing_length)
grid_wl <- seq(min(scaled_wl), max(scaled_wl), length = 100)
logit_predicted_survival <- phiwl.p$results$beta[1,1] + phiwl.p$results$beta[2,1] * grid_wl
predicted_survival <- plogis(logit_predicted_survival)
df <- data.frame(winglength = grid_wl,
                 survival = predicted_survival)
df %>% 
  ggplot() +
  aes(x = winglength, y = survival) + 
  geom_line()  + 
  theme_light(base_size = 18)


## ---------------------------------------------------------------------------------
# square standardised covariate and add it to dipper data
dipper$wglengthsq <- scale(dipper_csv$wing_length)^2
# process data
dipper.proc <- process.data(data = dipper, 
                            groups = ("sex"))
# build design matrices
dipper.ddl <- make.design.data(dipper.proc)


## ---------------------------------------------------------------------------------
phiwl2 <- list(formula=~wglength + wglengthsq)


## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phiwl2.p <- mark(dipper.proc,
                 dipper.ddl,
                 model.parameters = list(Phi = phiwl2, p = p))


## ----echo=TRUE, message=FALSE, warning=FALSE--------------------------------------
phiwl2.p$results$beta


## ---- echo = FALSE, fig.width = 7.5, fig.asp = 0.718, dev = "svg", message = FALSE, warning = FALSE, fig.align='center'----
# Build dataset
scaled_wl <- scale(dipper_csv$wing_length)
grid_wl <- seq(min(scaled_wl), max(scaled_wl), length = 100)
logit_predicted_survival <- phiwl2.p$results$beta[1,1] + phiwl2.p$results$beta[2,1] * grid_wl + phiwl2.p$results$beta[2,1] * grid_wl^2
predicted_survival <- plogis(logit_predicted_survival)
df <- data.frame(winglength = grid_wl,
                 survival = predicted_survival)

# Visualize
df %>% 
  ggplot() +
  aes(x = winglength, y = survival) + 
  geom_line() + 
  theme_light(base_size = 18)




## ---------------------------------------------------------------------------------
collect.models()


## ---------------------------------------------------------------------------------
head(dipper.ddl$Phi)


## ---------------------------------------------------------------------------------
dipper.ddl$Phi$Flood <- 0
head(dipper.ddl$Phi)


## ---------------------------------------------------------------------------------
dipper.ddl$Phi$Flood[dipper.ddl$Phi$time==2 | 
                       dipper.ddl$Phi$time==3] <- 1
head(dipper.ddl$Phi)


## ---------------------------------------------------------------------------------
phiFlood <- list(formula=~Flood)



## ----warning=FALSE, message=FALSE, echo = FALSE-----------------------------------
phiFlood.p <- mark(dipper.proc,
                   dipper.ddl,
                   model.parameters = list(Phi = phiFlood, p = p))


## ---------------------------------------------------------------------------------
phiFlood.p$results$real


## ---------------------------------------------------------------------------------
collect.models()


## ---------------------------------------------------------------------------------
phi.age <- list(formula =~ age) # already coded in RMark



## ----echo = FALSE-----------------------------------------------------------------
phiage.p <- mark(dipper.proc,
                 dipper.ddl,
                 model.parameters = list(Phi = phi.age, 
                                         p = p))


## ---------------------------------------------------------------------------------
phiage.p$results$real


## ---------------------------------------------------------------------------------
dipper.ddl <- add.design.data(dipper.proc,
                              dipper.ddl, 
                              "Phi",
                              type = "age",
                              bins = c(0, 1, 7), # (0, 1+)
                              name = "ageclass") # var name


## ---------------------------------------------------------------------------------
phi.age2 <- list(formula=~ageclass) # age effect on survival



## ----echo = FALSE-----------------------------------------------------------------
phiage2.p <- mark(dipper.proc,
                  dipper.ddl,
                  model.parameters = list(Phi = phi.age2, p = p))


## ---------------------------------------------------------------------------------
phiage2.p$results$real


## ----echo = FALSE, warning=FALSE, message=FALSE-----------------------------------
rm(list = ls(all = TRUE))
cleanup(ask = FALSE)


## ---------------------------------------------------------------------------------
geese_csv <- read_csv2("dat/allgeese.csv")


## ---------------------------------------------------------------------------------
geese_csv


## ---------------------------------------------------------------------------------
geese <- data.frame(ch = unite(geese_csv %>% 
                                 select(year_1984:year_1989), 
                               col = "ch", 
                               sep = ""), 
                    freq = geese_csv$N)


## ---------------------------------------------------------------------------------
head(geese)


## ---------------------------------------------------------------------------------
geese.proc <- process.data(data = geese,
                           model = "Multistrata")



## ---------------------------------------------------------------------------------
geese.ddl <- make.design.data(geese.proc)


## ---------------------------------------------------------------------------------
phi <- list(formula=~1) # survival constant 
phi.site <- list(formula=~stratum) # site-specific survival 
p <- list(formula=~1) # constant detection
psi.site <- list(formula=~-1+stratum:tostratum,
                 link='mlogit') # movements



## ----echo = FALSE-----------------------------------------------------------------
phis.psi.p <- mark(geese.proc,
                   geese.ddl,
                   model.parameters=list(S = phi.site,
                                         p = p,
                                         Psi = psi.site))


## ---------------------------------------------------------------------------------
phis.psi.p$results$real


## ---------------------------------------------------------------------------------
Psilist <- get.real(phis.psi.p, "Psi", vcv = TRUE)
Psi.values = Psilist$estimates
top.psi = TransitionMatrix(Psi.values[Psi.values$time == 1, ], 
                           vcv.real = Psilist$vcv.real)
top.psi


## ----echo = FALSE-----------------------------------------------------------------
phi.psi.p <- mark(geese.proc,
                  geese.ddl,
                  model.parameters=list(S = phi,
                                        p = p,
                                        Psi = psi.site))


## ---------------------------------------------------------------------------------
phi.psi.p$results$real


## ---------------------------------------------------------------------------------
Psilist <- get.real(phi.psi.p, "Psi", vcv = TRUE)
Psi.values = Psilist$estimates
top.psi = TransitionMatrix(Psi.values[Psi.values$time == 1, ], 
                           vcv.real = Psilist$vcv.real)
top.psi


## ---------------------------------------------------------------------------------
collect.models()


## ----echo = FALSE, warning=FALSE, message=FALSE-----------------------------------
rm(list = ls(all = TRUE))
cleanup(ask = FALSE)




################################################################
################################################################
############# R scripts for live demos on PPMS #################
################################################################
################################################################


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


