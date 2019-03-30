library(readxl)
library(dplyr)
library(plm)       # Panel data analysis library
library(car)
#install.packages("mfx")
library(olsrr)
library(lmtest)
library(glmmML)
library(quantreg)
library("rqpd")
library(lme4)
library(sjPlot)
library(sjmisc)
library(lfe)
library(mfx)

#Read in data
urbandata <- read_excel("project data.xlsx")

panel_urbandata <- pdata.frame(urbandata, index=c("irn","year"))

#Full OLS Pooled Regression
ols_full <- lm(voteprct ~ levytype + factor(purpose) + replacement + duration + amount.dec + falladm + math + percapy + pctown_co + pct0to17 + pctwithkids_co + last.levy + factor(last.levy.pass) + factor(year), data = panel_urbandata)
summary(ols_full)

plot(ols_full)


AIC(ols_full)
BIC(ols_full)

#Fixed Effects Panel Data Regression
fixed <- plm(voteprct ~ levytype + factor(purpose) + replacement + duration + amount.dec + falladm + math + staff + pupil + admin + bldg + instruc + percapy + pctown_co + pct5to17_co + pctwithkids_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "within", effect = "twoway")
summary(fixed)

AIC(fixed)

## Random Effects Panel Data Regression
random <- plm(voteprct ~ levytype + factor(purpose) + replacement + duration + amount.dec + falladm + math + staff + pupil + admin + bldg + instruc + percapy + pctown_co + pct5to17_co + pctwithkids_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "random")
summary(random)

AIC(random)

##Durbin Wu-Hausman for testing between fixed and random (null - there are significant random effects)
phtest(fixed,random)

#Check between FE and Pooled
pFtest(fixed,ols_full)

#testing for between fixed effects and time-fixed effects
plmtest(fixed, c("time"), type=("bp")) #apparently we should be using fixed time effects

#Breusch Pagan to test for heteroskedasticity
ols_test_breusch_pagan(ols_full) #probably not relevant for FE panel model

#different models - 

fixed1 <- plm(voteprct ~ levytype + factor(purpose) + replacement + duration + amount.dec + math + bldg + instruc + percapy + pupil + pctown_co + last.levy + factor(last.levy.pass), data = panel_urbandata, effects = "twoway", model = "within")
summary(fixed1)

fixed2 <- plm(voteprct ~ levytype + factor(purpose) + replacement + duration + amount.dec + math + pupil + pctown_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "within")
summary(fixed2)

fixed3 <- plm(voteprct ~ levytype + factor(purpose) + duration + amount.dec + math + pupil + pctown_co + factor(last.levy.pass), data = panel_urbandata, model = "within")
summary(fixed3)

fixed4 <- plm(voteprct ~ levytype + factor(purpose) + duration + amount.dec + math + pupil, data = panel_urbandata, model = "within")
summary(fixed4)

pFtest(fixed, fixed1)
pFtest(fixed1,fixed2)
pFtest(fixed2, fixed3)
pFtest(fixed3, fixed4)
pFtest(fixed4, ols_full)

#Logit Regression for Specific Year
logit <- glm(passfail ~ levytype + factor(purpose) + replacement + duration + amount.dec + falladm + math + staff + pupil + admin + bldg + instruc + percapy + pctown_co + pct0to17 + pctwithkids_co + last.levy + factor(last.levy.pass) + factor(year), family=binomial("logit"), data = data)
summary(logit)

#Probit Regression for Specific Year
probit <- glm(passfail ~ levytype + factor(purpose) + replacement + duration + amount.dec + falladm + math + staff + pupil + admin + bldg + instruc + percapy + pctown_co + pct0to17 + pctwithkids_co + last.levy + factor(last.levy.pass) + factor(year), family=binomial("probit"), data = data)
summary(probit)


#5 problems: 1.checking for multicollinearity
vif(ols_full)
dwtest(ols_full) 
vif(fixed) #doesn't make sense

#2.testing for serial correlation (not needed as we don't have long time series but still)
#Breusch-Godfrey/Wooldridge test to test for serial correlation in panel models
pbgtest(fixed)

#null - there is no serial correlation. Since we don't reject the null, we're fine
# (there's no serial correlation)


##########################################################################################
#### Final Draft
##########################################################################################

#Test for functional Form

fixed_log <- plm(voteprct ~ levytype + factor(purpose) + replacement + log(duration) + log(amount.dec) + log(falladm) + math + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + pctown_co + pct5to17_co + pctwithkids_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "within")
summary(fixed_log)
summary(fixed)

fixed_log_1 <- fixed_log <- plm(voteprct ~ levytype + factor(purpose) + replacement + log(duration) + log(amount.dec) + log(falladm) + math + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + pctown_co + log(percapy)*pctown_co + pct5to17_co + pctwithkids_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "within")
summary(fixed_log_1)

fixed_log_2 <- fixed_log <- plm(voteprct ~ levytype + factor(purpose) + replacement + log(duration) + log(amount.dec) + log(falladm) + math + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + cap_squared + pctown_co + pct5to17_co + pctwithkids_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "within")
summary(fixed_log_2)

#Testing functional form of logging dependent variable. R squared went down
fixed_log_log <- plm(log(voteprct) ~ levytype + factor(purpose) + replacement + log(duration) + log(amount.dec) + log(falladm) + math + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + pctown_co + pct5to17_co + pctwithkids_co + last.levy + factor(last.levy.pass), data = panel_urbandata, model = "within")
summary(fixed_log_log)

#FIXED EFFECTS Added interaction term, increased r squared 
fixed_log_FINAL <- plm(voteprct ~ levytype + factor(purpose) +replacement + log(duration) + log(amount.dec) + log(falladm) + log(math) + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + log(pctown_co) + log(pct5to17_co) + log(pctwithkids_co) + last.levy + factor(last.levy.pass) + log(percapy)*log(pctown_co), data = panel_urbandata, model = "within", effect = "twoway")
summary(fixed_log_FINAL)

#OLS Added interaction term, increased r squared 
ols_log_FINAL <- lm(voteprct ~ levytype + factor(purpose) +replacement + log(duration) + log(amount.dec) + log(falladm) + log(math) + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + log(pctown_co) + log(pct5to17_co) + log(pctwithkids_co) + last.levy + factor(last.levy.pass) + log(percapy)*log(pctown_co), data = panel_urbandata)
summary(ols_log_FINAL)

#LOGIT 
logit_log_FINAL <- glm(passfail ~ levytype + factor(purpose) +replacement + log(duration) + log(amount.dec) + log(falladm) + log(math) + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + log(pctown_co) + log(pct5to17_co) + log(pctwithkids_co) + last.levy + factor(last.levy.pass) + log(percapy)*log(pctown_co), data = panel_urbandata, family=binomial("logit"))
summary(logit_log_FINAL)

#Maringal affect
logitmfx(passfail ~ levytype + factor(purpose) +replacement + log(duration) + log(amount.dec) + log(falladm) + log(math) + log(staff) + log(pupil) + log(admin) + log(bldg) + log(instruc) + log(percapy) + log(pctown_co) + log(pct5to17_co) + log(pctwithkids_co) + last.levy + factor(last.levy.pass) + log(percapy)*log(pctown_co), data = urbandata)



hist(urbandata$voteprct, prob=TRUE, col = "blue", border = "black")

#Between Within Model

urbandata$duration_log <- log(urbandata$duration)
urbandata$amount.dec_log <- log(urbandata$amount.dec)
urbandata$falladm_log <- log(urbandata$falladm)
urbandata$math_log <- log(urbandata$math)
urbandata$staff_log <- log(urbandata$staff)
urbandata$pupil_log <- log(urbandata$pupil)
urbandata$admin_log <- log(urbandata$admin)
urbandata$bldg_log <- log(urbandata$bldg)
urbandata$instruc_log <- log(urbandata$instruc)
urbandata$percapy_log <- log(urbandata$percapy)
urbandata$pctown_co_log <- log(urbandata$pctown_co)
urbandata$pct5to17_co_log <- log(urbandata$pct5to17_co)
urbandata$pctwithkids_co_log <- log(urbandata$pctwithkids_co)

urbandata <- de_mean(urbandata, duration_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, amount.dec_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, falladm_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, math_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, staff_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, pupil_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, admin_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, bldg_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, instruc_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, percapy_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, pctown_co_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, pct5to17_co_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, pctwithkids_co_log, voteprct, grp = irn)
urbandata <- de_mean(urbandata, last.levy, voteprct, grp = irn)

m3 <- lmer(
  voteprct ~ year + levytype + factor(purpose) + replacement + duration_log_dm + duration_log_gm + amount.dec_log_dm + amount.dec_log_gm + falladm_log_dm + falladm_log_gm + math_log_dm + math_log_gm + staff_log_dm + staff_log_gm + pupil_log_dm + pupil_log_gm + admin_log_dm + admin_log_gm + bldg_log_dm + bldg_log_gm + instruc_log_dm + instruc_log_gm + percapy_log_dm + percapy_log_gm + pctown_co_log_dm + pctown_co_log_gm + pct5to17_co_log_dm + pct5to17_co_log_gm + pctwithkids_co_log_dm + pctwithkids_co_log_gm + last.levy_dm + last.levy_gm + factor(last.levy.pass) +
    (1 + year | irn) ,
  data = urbandata
)

summary(m3)

tab_model(
  m3,
  show.ci = FALSE, 
  show.se = TRUE, 
  auto.label = FALSE, 
  string.se = "SE",
  show.icc = FALSE,
  dv.labels = c("Random Effects Between Within")
)




