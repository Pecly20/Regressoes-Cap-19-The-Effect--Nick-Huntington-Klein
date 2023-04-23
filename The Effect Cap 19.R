

install.packages('causaldata')
install.packages('modelsummary')
install.packages('tidyverse')
install.packages('fixest')

#1) 2SLS

# Or dev version (currently identical):
# If necessary: install.packages('remotes')
install.packages('remotes')

remotes::install_github('NickCH-K/causaldata/R/')

# There are many ways to run 2SLS; 
# the most common is ivreg from the AER package. 
# But we'll use feols from fixest for speed and ease 
# of fixed-effects additions later
library(tidyverse); library(modelsummary); library(fixest)


d <- causaldata::social_insure

# Include just the outcome and controls first, then endogenous ~ instrument 
# in the second part, and for this study we cluster on address
m <- feols(takeup_survey ~ male + age + agpop + ricearea_2010 +
             literacy + intensive + risk_averse + disaster_prob +
             factor(village) | pre_takeup_rate ~ default, 
           cluster = ~address, data = d)

# Show the first and second stage, omitting all
# the controls for ease of visibility
msummary(list('First Stage' = m$iv_first_stage[[1]],
              'Second Stage' = m),
         coef_map = c(default = 'First Round Default',
                      fit_pre_takeup_rate = 'Friends Purchase Behavior'),         
         stars = c('*' = .1, '**' = .05, '***' = .01))


#2) GMM

install.packages('gmm')

library(modelsummary); library(gmm)

d <- causaldata::social_insure
# Remove all missing observations ourselves
d <- d %>%
  select(takeup_survey, male, age, agpop, ricearea_2010,
         literacy, intensive, risk_averse, disaster_prob,
         village, address, pre_takeup_rate, default) %>%
  na.omit()

m <- gmm(takeup_survey ~ male + age + agpop + ricearea_2010 +
           literacy + intensive + risk_averse + disaster_prob +
           factor(village) + pre_takeup_rate,
         ~ male + age + agpop + ricearea_2010 +
           literacy + intensive + risk_averse + disaster_prob +
           factor(village) + default, data = d)

# We can apply the address clustering most easily in msummary
msummary(m, vcov = ~address, stars = c('*' = .1, '**' = .05, '***' = .01))


#Finally, what if we have a lot of fixed effects in our IV model? 
#There are some technical adjustments that must be made in these cases. 
#This time it's R that has the easiest transition. 
#The feols() function we already used can easily 
#incorporate fixed effects - the +factor(village) just becomes | village.

#m <- feols(takeup_survey ~ male + age + agpop + ricearea_2010 +
 #    literacy + intensive + risk_averse + disaster_prob |  village + pre_takeup_rate,
  #   ~ male + age + agpop + ricearea_2010 +
   #  literacy + intensive + risk_averse + disaster_prob | village ~ default, data = d)



