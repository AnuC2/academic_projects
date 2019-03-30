# Core Tidyverse
library(tidyverse)
library(lubridate)
library(glue)
library(forcats)
library(magrittr)
library(scales)

# Visualization
library(ggplot2)
library(cowplot)
library(scales)
library(vcd)
library(grid)


df<- read.csv("foreign_citizen.csv")
head(df)
df1<- df[,c("student_id","age","full_part","Entry.Term","classification","response","gpa","educ","exp","sex","gre_total","country")]

head(df1)
summary(df1)

######## cleaning gpa variable ######
sum(is.na(df1$gpa))
# Check distribution of one field
df1["gre_total"] %>%
  gather() %>% 
  ggplot() +
  geom_point(mapping = aes(x = value,y=df1$gre_total)) +
  facet_wrap(~ key, scales = "free")
## replace na with mean value if any ,but not needed this case
df1$gpa[is.na(df1$gpa)] = mean(df$gpa, na.rm=TRUE)
## now remove outliers
df1$gpa <- squish(df1$gpa,round(quantile(df1$gpa,c(0.05,0.95))))
summary(df1$gpa)
#plot again to check if you want

#clean other variables, check plots and summary for all variables
sum(is.na(df1$age))
summary(df1$gre_total)
sum(is.na(df1$gre_total))
sum(is.na(df1$gre_total))
## clean gre variable
df1$gre_total <- squish(df1$gre_total,round(quantile(df1$gre_total,c(0.05,0.95))))
summary(df1$gpa)
## cleaning educ variable
summary(df1$educ)
summary(df1$classification)

## converting to factors
df1<-transform(df1, full_part.f = as.factor(full_part))
df1<-transform(df1, classification.f = as.factor(classification))
df1<-transform(df1, response.f = as.factor(response))
df1<-transform(df1, educ.f = as.factor(educ))
df1<-transform(df1, sex.f = as.factor(sex))
## remove classification = 2 which are incomplete applications
##df1[df1$classification!=2,]
##summary(df1$classification)
## lpm run
lpm=lm( classification~ age+gpa+educ.f+sex.f+exp+gpa, data=df1)
summary(lpm)

## logit run
logit <- glm(classification~ age+gpa+educ.f+sex.f+exp+gpa, family = binomial(link = "logit"), data = df1)
summary(logit)
### probit run
probit <- glm(classification~ age+gpa+educ.f+sex.f+exp+gpa, family = binomial(link = "probit"), data = df1)
summary(probit)
