# This script was JTP's first go at SR analysis and uses a lot of re-hashed 
#  code from M. Tyers. There is a mix of both Matt and Justin's code. 
# This script is therefor in the process of being retired. The last half of it
#  has not yet been converted over to the *_JTPtidy script. 


source("code/auke_1_import.R")

startyr <- 1980

aukereturns <- aukebrood_nojacks %>%
  group_by(year) %>%
  summarise(samplecount = sum(agecount)) %>%
  inner_join(aukebrood_nojacks, by = "year", multiple = "all") %>%
  # the above 3 lines add sample count as a column
  mutate(agepropor = agecount / samplecount) %>%
  left_join(auketotalrun %>% 
              dplyr::select(returnyear, adults_total_tag, adults_total_all),
            by = c("year" = "returnyear")) %>% 
  mutate(estagecount_all = round(agepropor * adults_total_all)) %>% # double check that rounding isn't a prob
  dplyr::select(year, broodyear, age_euro, estagecount_all) %>%
  group_by(broodyear) %>%
  summarise(BYreturns = sum(estagecount_all)) %>% 
  filter(between(broodyear, startyr-3, 2018)) # return years: 1980-2022

# restrict to BY2018 because BY2019 2.1s will return in 2023 so it's incomplete

aukebrood_nojacks %>% 
  select(-year) %>%
  pivot_wider( names_from = age_euro, values_from = agecount) %>% tail(20)


auketotalrun

auke_SR <- auketotalrun %>%
  dplyr::select(returnyear, adult_esc_all) %>%
  left_join(aukereturns,
            by = c("returnyear" = "broodyear")) %>%
  rename(year = "returnyear",
         spawners = "adult_esc_all") %>%
  filter(between(year, startyr, 2018)) 
# exclude return years >2019 since we don't have a full return yet


ggplot(auke_SR, aes(x = spawners, y = BYreturns, color = year)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 2000)) +
  scale_y_continuous(limits = c(0, 2000)) + 
  theme_bw()


# Now draw a fitted curve on top of the points
# get lnalpha and beta values
fitrick_lnalpha <- fitRicker(auke_SR$spawners, auke_SR$BYreturns)$lnalpha_fit
fitrick_beta <- fitRicker(auke_SR$spawners, auke_SR$BYreturns)$beta_fit

curvedf <- data.frame(spawners = seq(from = 0, to = 2000, by = 1)) %>% 
  mutate(fitcurve = spawners*(exp(fitrick_lnalpha - (fitrick_beta*spawners))) )

ggplot() +
  geom_point(data = auke_SR, aes(x = spawners, y = BYreturns, color = year)) + 
  geom_line(data = curvedf, aes(x = spawners, y = fitcurve), 
            color = "red", lwd = 2) + 
  scale_x_continuous(limits = c(0, 2000)) +
  scale_y_continuous(limits = c(0, 2000)) + 
  theme_bw()


  
############################################
############################################



fitRicker(auke_SR$spawners, auke_SR$BYreturns)

lm_fit <- lm(log(BYreturns/spawners) ~ spawners, data = auke_SR)
summary(lm_fit)  # inspect the results

lnalpha_hat <- unname(lm_fit$coefficients[1])
beta_hat <- unname(-lm_fit$coefficients[2])

# Bias-corrected lnalpha_p
sigma_hat <- sigma(lm_fit)
lnalpha_p_hat <- lnalpha_hat + (sigma_hat^2)/2


Smax_hat <- 1/beta_hat
Seq_hat <- lnalpha_p_hat/beta_hat
Smsy_hat <- Seq_hat*(0.5-0.07*lnalpha_p_hat)
Umsy_hat <- lnalpha_p_hat*(0.5-0.07*lnalpha_p_hat)
MSY_hat <- Smsy_hat*exp(lnalpha_p_hat-beta_hat*Smsy_hat)-Smsy_hat

fits <- lm_fit$fitted.values
resids <- lm_fit$residuals

#library(car)
car::durbinWatsonTest(lm_fit)



########################

S <- auke_SR$spawners
R <- auke_SR$BYreturns



par(mfrow=c(2,2))  # plots will now be on a 2x2 matrix
plot(S, R, xlim=c(0,max(S,R)), ylim=c(0,max(S,R)))
abline(0, 1, lty=3)  # replacement line - arguments draw a line with y-int=0, slope=1, and dotted
curve(Ricker(x, lnalpha_hat, beta_hat), add=T)  # adding a Ricker curve using our Ricker function from above
plot(S, log(R/S))
abline(lm_fit)  # regression line from lm_fit
abline(h=0, lty=3)  # horizontal line at y=0
plot(S, ylim=c(0, max(S,R)), type='l', col="red")
lines(R, col="blue")
legend("topright", legend=c("S","R"), col=c("red","blue"), lty=1)
plot(resids, type='l', main="Residuals")
abline(h=0, lty=3)  # horizontal line at y=0
par(mfrow=c(1,1))




B <- 10000  # how many bootstrap replicates to do. This is upped from 1000 because computing power is cheap!
lnalpha_boot <- lnalpha_p_boot <- beta_boot <- NA  # initializing vectors to fill in within the loop
for(i in 1:B) {
  y_boot <- fits + sample(resids, replace=T)
  lm_boot <- lm(y_boot~S)
  lnalpha_boot[i] <- unname(lm_boot$coefficients[1])
  lnalpha_p_boot[i] <- lnalpha_boot[i] + 0.5*(sigma(lm_boot))^2
  beta_boot[i] <- unname(-lm_boot$coefficients[2])
}


impossible <- (lnalpha_boot<0) | (beta_boot<0)  # "|" = "or"
lnalpha_boot <- lnalpha_boot[!impossible]
lnalpha_p_boot <- lnalpha_p_boot[!impossible]
beta_boot <- beta_boot[!impossible]


Smax_boot <- 1/beta_boot
Seq_boot <- lnalpha_p_boot/beta_boot
Smsy_boot <- Seq_boot*(0.5-0.07*lnalpha_p_boot)
Umsy_boot <- lnalpha_p_boot*(0.5-0.07*lnalpha_p_boot)
MSY_boot <- Smsy_boot*exp(lnalpha_p_boot-beta_boot*Smsy_boot)-Smsy_boot

# Plotting as histograms
par(mfrow=c(2,2))  # Plots will now be on a 2x2 matrix
hist(lnalpha_boot)
hist(beta_boot)
hist(Seq_boot)
hist(Smsy_boot)
par(mfrow=c(1,1))


######################################
###### JTP: SHOW BOOTSTRAP RICKERS ###
bootrickerdf <- data.frame(lnalpha_boot, lnalpha_p_boot, beta_boot, Smax_boot, 
           Seq_boot, Smsy_boot, Umsy_boot, MSY_boot)

numcurves <- 100
bootcurve <- data.frame(spawners = seq(from = 0, to = 2000, length.out = 40))
for(j in 1:numcurves){
  .df <- sample_n(bootrickerdf, size=1, replace = TRUE)
  bootcurve[,j+1] <- bootcurve$spawners * 
                     (exp(.df$lnalpha_boot - (.df$beta_boot * bootcurve$spawners))) 
}

bootcurve %>%
  pivot_longer(-spawners, values_to = "rickerfit", names_to = "runnumber") %>% 
  ggplot() +
  #geom_point(data = auke_SR, aes(x = spawners, y = BYreturns, color = year)) + 
  geom_line(aes(x = spawners, y = rickerfit, group = runnumber), 
            color = "red", lwd = 0.5, alpha = 0.3) + 
  geom_point(data = auke_SR, aes(x = spawners, y = BYreturns), color = "black") + 
  scale_x_continuous(limits = c(0, 2000)) +
  scale_y_continuous(limits = c(0, 2000)) + 
  theme_bw()




##################################
##################################



S_max <- 2000     # Max value of prospective escapements
S_star <- seq(1, S_max, length.out=1000)  # Prospective escapements
# expanded as a matrix
# one row for every bootstrap and one column for every prospective escapement
S_star_mat <- matrix(S_star, nrow=length(beta_boot), ncol=length(S_star), byrow=T)  # expanded as a matrix
# check dimensions
dim(S_star_mat)

# initializing the R_star matrix, then filling it in one column at a time, using bootstrap vectors all at once
R_star <- matrix(nrow=length(beta_boot), ncol=length(S_star))
for(i in 1:length(S_star)) {
  R_star[,i] <- Ricker(S_star[i], lnalpha_p_boot, beta_boot)
}

SY_star <- R_star - matrix(S_star, nrow=length(beta_boot), ncol=length(S_star), byrow=T)

# Also expanding MSY_boot and Smsy as matrix
MSY_boot_star <- matrix(MSY_boot, nrow=length(beta_boot), ncol=length(S_star))
Smsy_boot_star <- matrix(Smsy_boot, nrow=length(beta_boot), ncol=length(S_star))


OYP <- colMeans(SY_star >= 0.9*MSY_boot_star)  # Optimal Yield Profile
OFP <- colMeans((SY_star < 0.9*MSY_boot_star) & (S_star_mat < Smsy_boot_star))  # Overfishing Profile

# Starting a plot...
make_OYP <- function() {  # this is a shortcut for creating the plot
  plot(S_star, OYP, type='l', col="green", ylim=0:1, ylab="Probability")
  lines(S_star, OFP, col="red")
  grid()
}
par(mfrow=c(1,1))
make_OYP()






quants <- c(0.05, 0.1, 0.5, 0.9, 0.95)  # which quantiles to extract
SY_quantiles <- apply(SY_star, 2, quantile, p=quants)

# This is the first 5 columns it returned.  There's a set of yield quantiles associated with each value of S_star.
SY_quantiles[,1:5]

make_EYP <- function() { # this is a shortcut for creating the plot
  # Making a blank plot to add lines to
  plot(NA, xlab="S", ylab="Expected Yield", xlim=range(S_star), ylim=c(0,max(SY_quantiles)))
  ltys <- c(3,2,1,2,3)  # the line type for plotting each line
  for(i in 1:5) {
    lines(S_star, SY_quantiles[i,], lty=ltys[i])
  }
  grid()
  legend("topright", legend=c("median", "80% intvl", "90% intvl"), lty=1:3)
}
make_EYP()


### Part 3 ###
# look at the OYP and EYP to come up with an escapement goal range
EG <- c(250, 600)
EGcurr <- c(200, 500)

par(mfrow=c(2,1))
make_OYP()
abline(v=EG, lwd=3, col="red")
abline(v=EGcurr, lty=3, lwd=2, col="pink")
make_EYP()
abline(v=EG, lwd=3, col="red")
abline(v=EGcurr, lty=3, lwd=2, col="pink")

# probabilities associated with EG endpoints
OYP[S_star %in% floor(EG)]

# yield associated with EG range
round(c(min(SY_quantiles[1, S_star>=EG[1] & S_star<=EG[2]]),
        max(SY_quantiles[5, S_star>=EG[1] & S_star<=EG[2]])))




newdf <- data.frame(S_star = S_star, OYP = OYP)
ggplot(newdf, aes(x=S_star, y = OYP)) + geom_line(color = "#007d1b", size = 3) +
  geom_vline(xintercept = EG[1], color = "red", size = 2) +
  geom_vline(xintercept = EG[2], color = "red", size = 2) +
  geom_vline(xintercept = EGcurr[1], color = "pink", size = 1, lty = 2) +
  geom_vline(xintercept = EGcurr[2], color = "pink", size = 1, lty = 2) +
  xlim(0, 1000) + ggtitle("OYP Profile with no Jacks, 90%") +
  theme_bw()


######

ests <- fitRicker(S, R)
newdf2 <- data.frame(S=seq(from=0, to = 3000, by = 50), 
                     fitrick = Ricker(seq(from=0, to = 3000, by = 50), 
                                      lnalpha=ests$lnalpha_fit, beta=ests$beta_fit))



ggplot() + 
  geom_line(data = newdf2, aes(x=S, y = fitrick), color = "red", size = 3) + 
  geom_abline(intercept = 0, lty = 2) +
  geom_point(data = data.frame(S=S, R=R), aes(x=S, y = R), color="dark blue", size = 3) +
  xlim(0, 3000) + 
  ylab("R") + ggtitle("Ricker curve with no Jacks") 



ggplot(data.frame(y=ests$resids), aes(x=seq(from=startyr, to = startyr+length(ests$resids)-1), y=y)) + 
  geom_line(size=1.5) + 
  theme_bw() +
  labs(title = "Residuals Over Time", x = "brood year",
       y = "Scaled residuals")







###############


bootRicker <- function(S, R, B=300) {
  firstfit <- fitRicker(S=S, R=R)
  fits <- firstfit$fits
  resids <- firstfit$resids
  lnalpha_boot <- lnalpha_p_boot <- beta_boot <- rep(NA, B)
  for(i in 1:B) {
    lmy <- fits + sample(resids, replace=T)
    lmfit <- lm(lmy~S)
    lnalpha_boot[i] <- unname(lmfit$coefficients[1])
    lnalpha_p_boot[i] <- lnalpha_boot[i] + (sigma(lmfit)^2)/2
    beta_boot[i] <- unname(-lmfit$coefficients[2])
  }
  
  impossible <- lnalpha_boot<0 | beta_boot<0  # censor the impossible
  return(list(lnalpha_boot=lnalpha_boot[!impossible],
              lnalpha_p_boot=lnalpha_p_boot[!impossible],
              beta_boot=beta_boot[!impossible]))
}






reps = 10
estimates <- cilo <- cihi <- as.data.frame(matrix(nrow=reps, ncol=6))
names(estimates) <- names(cilo) <- names(cihi) <- c("lnalpha_p","beta","Smsy","Smax","Seq","MSY")

for(i in 1:reps) {
  
  thefit <- fitRicker(S=S, R=R)
  estimates$lnalpha_p[i] <- thefit$lnalpha_p_fit
  estimates$beta[i] <- thefit$beta_fit
  theboot <- bootRicker(S=S, R=R, B=B)
  Seq_boot <- theboot$lnalpha_p_boot/theboot$beta_boot
  Smax_boot <- 1/theboot$beta_boot
  Smsy_boot <- Seq_boot*(0.5-0.07*theboot$lnalpha_p_boot)
  MSY_boot <- Ricker(Smsy_boot, theboot$lnalpha_p_boot, theboot$beta_boot) - Smsy_boot
  cilo[i,] <- sapply(list(theboot$lnalpha_p_boot, theboot$beta_boot, Smsy_boot, Smax_boot, Seq_boot, MSY_boot),
                     quantile, p=0.1)
  cihi[i,] <- sapply(list(theboot$lnalpha_p_boot, theboot$beta_boot, Smsy_boot, Smax_boot, Seq_boot, MSY_boot),
                     quantile, p=0.9)
}
estimates$Seq <- estimates$lnalpha_p/estimates$beta
estimates$Smax <- 1/estimates$beta
estimates$Smsy <- estimates$Seq*(0.5-0.07*estimates$lnalpha_p)
estimates$MSY <- Ricker(estimates$Smsy, estimates$lnalpha_p, estimates$beta) - estimates$Smsy
estimates  
cilo  
cihi





####################################
############ JTP#####################
bootrickerdf %>% 
  mutate(MSY_boot_90pct = MSY_boot * 0.9)

