# Same as script 2 but starting over and doing this all from scratch  
#  without including Matt Tyers' scripts.
# See note on other script 2 file. This one will eventually replace the other. 


source("code/auke_1_import.R")


# Decision summary for this script
#   - jacks vs no jacks: no jacks
#   - Year range: >1980
#   - Account for productivity?: No
#   - Theta or weir counts: theta
#   - Smolt:smolt SR?: No 



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
  filter(between(broodyear, 1977, 2018)) # BY1988-2018 = return years 1991-2022

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
  filter(between(year, 1980, 2018)) 
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




##########
# Now let's fit a model to this!

lm_fit <- lm(log(BYreturns/spawners) ~ spawners, data = auke_SR)
summary(lm_fit)  # inspect the results

# First check if residuals are non-stationary (linear trend)
summary(lm(lm_fit$residuals ~ seq(1:length(lm_fit$residuals))))
plot(lm_fit$residuals)
# uh oh, there's a significant trend!


# Now test model for serial autocorrelation
car::durbinWatsonTest(lm_fit)
acf(lm_fit$residuals, type = "correlation")
# We can see that there is significant autocorrelation at lag 1&2. phi = 0.55 




fits <- lm_fit$fitted.values
resids <- lm_fit$residuals

B <- 10000  # how many bootstrap replicates to do. 
lnalpha_boot <- lnalpha_p_boot <- beta_boot <- NA  # initializing vectors to fill in within the loop

bootresults <- data.frame(lnalpha_boot = rep(NA, B),
                          lnalpha_p_boot = NA,
                          beta_boot = NA)


for(i in 1:B) {
  y_boot <- fits + sample(resids, replace=T)
  lm_boot <- lm(y_boot~auke_SR$spawners)
  bootresults$lnalpha_boot[i] <- unname(lm_boot$coefficients[1])
  bootresults$lnalpha_p_boot[i] <- bootresults$lnalpha_boot[i] + 0.5*(sigma(lm_boot))^2
  bootresults$beta_boot[i] <- unname(-lm_boot$coefficients[2])
}

# censor the impossible!
bootresults <- bootresults %>% 
  filter(lnalpha_boot > 0, beta_boot > 0)


bootresults <- bootresults %>%
  mutate(Smax_boot = 1/beta_boot,
         Seq_boot = lnalpha_p_boot/beta_boot,
         Smsy_boot = Seq_boot*(0.5-0.07*lnalpha_p_boot),
         Umsy_boot = lnalpha_p_boot*(0.5-0.07*lnalpha_p_boot),
         MSY_boot = Smsy_boot*exp(lnalpha_p_boot-beta_boot*Smsy_boot)-Smsy_boot)
bootresults %>% head(50) # view the bootstrapped results


bootresults %>%
  pivot_longer(cols = c(lnalpha_boot, lnalpha_p_boot, beta_boot, Smax_boot,  
                        Seq_boot, Smsy_boot, Umsy_boot, MSY_boot),
               names_to = "parameter") %>%
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = "free")
  

###############################
# JTP: draw bootstrapped Ricker curves

numcurves <- 250
bootcurve <- data.frame(spawners = seq(from = 0, to = 2000, length.out = 40))
for(j in 1:numcurves){
  .df <- sample_n(bootresults, size=1, replace = TRUE)
  bootcurve[,j+1] <- bootcurve$spawners * 
    (exp(.df$lnalpha_boot - (.df$beta_boot * bootcurve$spawners))) 
}

bootcurve %>%
  pivot_longer(-spawners, values_to = "rickerfit", names_to = "runnumber") %>% 
  ggplot() +
  #geom_point(data = auke_SR, aes(x = spawners, y = BYreturns, color = year)) + 
  geom_line(aes(x = spawners, y = rickerfit, group = runnumber), 
            color = "red", lwd = 0.5, alpha = 0.1) + 
  geom_point(data = auke_SR, aes(x = spawners, y = BYreturns), color = "black") + 
  scale_x_continuous(limits = c(0, 2000)) +
  scale_y_continuous(limits = c(0, 2000)) + 
  theme_bw()

rm(numcurves, bootcurve)
###############################


