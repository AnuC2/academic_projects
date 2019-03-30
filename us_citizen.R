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


df<- read.csv("usa.csv")
df1<- df[,c("student_id","age","full_part","classification","appl_response","gpa","educ","exper","Sex")]

head(df1)
summary(df1)
######## cleaning gpa variable ######
sum(is.na(df1$gpa))
# Check distribution of one field
df1["gpa"] %>%
  gather() %>% 
  ggplot() +
  geom_point(mapping = aes(x = value,y=df1$gpa)) +
  facet_wrap(~ key, scales = "free")
## replace na with mean value
df1$gpa[is.na(df1$gpa)] = mean(df$gpa, na.rm=TRUE)
##outlier of gpa variable
##plot the above plot again - saw two outliers, moving them to mean
??squish

df1$gpa <- squish(df1$gpa,round(quantile(df1$gpa,c(0.05,0.95))))
summary(df$gpa)

####clean level of education variable
summary(df$education)
# Check distribution of one field
df["education"] %>%
  gather() %>% 
  ggplot() +
  geom_point(mapping = aes(x = value,y=df$education)) +
  facet_wrap(~ key, scales = "free")
## number of empty cells
sum(is.na(df1))
## change all categorical variables to factors
df1<-transform(df1, full_part.f = as.factor(full_part))
df1<-transform(df1, classification.f = as.factor(classification))
df1<-transform(df1, appl_response.f = as.factor(appl_response))
df1<-transform(df1, educ.f = as.factor(educ))
df1<-transform(df1, sex.f = as.factor(Sex))
## check for str
str(df1)
## lpm run
lpm=lm( classification~ age+gpa+educ.f+sex.f+exper, data=df1)
summary(lpm)

## logit run
logit <- glm(classification~ age+gpa+educ.f+sex.f+exper, family = binomial(link = "logit"), data = df1)
summary(logit)
### probit run
probit <- glm(classification~ age+gpa+educ.f+sex.f+exper, family = binomial(link = "probit"), data = df1)
summary(probit)

## logit with only one variable
df2<- df[,c("classification","gpa")]
df2$gpa[is.na(df2$gpa)] = mean(df$gpa, na.rm=TRUE)
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.05,0.95))))
logit1 <- glm(classification~ gpa, family = binomial(link = "logit"), data = df2)
summary(logit1)

## Plotting all regressions:-
## logit curve with gpa variables:-
quartz(title="admission vs. gpa")
plot(df2$gpa,df2$classification,xlab="gpa",ylab="Probability") 
curve(predict(logit1,data.frame(gpa=x),type="resp",col="blue"),add= TRUE)
# 2-way contingency tables
xtabs(~education + gpa, data = df)


df1<- cbind(df$gpa,df$education)
cotab_mosaic(x=df1,condvars = NULL)
??mosaic
??cotab_mosaic


## decision variable classification
summary(df$classification)
## removing rows with classification = 2


## Sample regression:
ols<-lm(df$classification~df$age + df$gpa + df$education)
summary(ols)
logit1 <- glm(df$classification~df$age + df$gpa + df$education,family ="binomial")

