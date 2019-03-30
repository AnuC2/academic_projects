options("scipen"=10,digits=5)
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(magrittr)

eda <- read_xlsx("Datasets_Final.xlsx", sheet = "HamCo_IPUMS")
View(eda)
str(eda)
as_tibble(eda)

SSS_1 <- read_xlsx("Datasets_Final.xlsx", sheet = "SSS")
## Calculating the Self Sufficiency Standard & Validating its correct class
SSS <- SSS_1[1,19]* SSS_1[1,22]
class(SSS)
View(SSS)
str(SSS)
as_tibble(gather(SSS))

## Question 1

eda$GENDER <- as.factor(eda$GENDER)
eda$BCHELOR <- as.factor(eda$BCHELOR)
eda$HS <- as.factor(eda$HS)
eda$EMP <-as.factor(eda$EMP)

#Clean the data to an appropriate format  
ourdata <- eda %>%
select(GENDER, AGE, Age.Groupings, BCHELOR, HS, EMP, INCTOT, INCWAGE,
        INCWELFR, INCINVST, Income.Groupings) %>%
  mutate(PER_INVT = INCINVST/INCTOT) %>%
  filter(INCWAGE != 999999) %>%
  filter(INCWELFR != 99999) %>%
  filter(INCINVST != 999999) %>%
  arrange(desc(INCINVST)) 
ourdata <-as.tibble(ourdata)
 
## Filtering for Baby Boomers
ourdata2 <- ourdata %>%
  filter(Age.Groupings=="65 years and older")
ourdata2 <-as.tibble(ourdata2)

## Plotting the distribution of age by income group
ggplot(ourdata2,aes(x=Income.Groupings,fill=GENDER))+geom_bar(position=position_dodge())+theme_minimal()

## Providews the data points distinguishing this filtered group now by gender to analyze male versus female data
f <-  ourdata2 %>%  select( INCTOT, INCWAGE, INCWELFR,INCINVST,GENDER) %>%
  filter(GENDER==0)
af <- data.frame(mean(f$INCTOT), mean(f$INCWAGE),mean(f$INCWELFR),mean(f$INCINVST))
names(af) <- c('TotalIncome', 'Wage', 'Welfare','Investment')
af <-as_tibble(af)
m <-  ourdata2 %>%  select( INCTOT, INCWAGE, INCWELFR,INCINVST,GENDER) %>%
  filter(GENDER==1)

## We are making the data points for the overall average total income and other financial characteristics for the age group 65+ by gender
am <- data.frame(mean(m$INCTOT), mean(m$INCWAGE),mean(m$INCWELFR),mean(m$INCINVST))
names(am) <- c('TotalIncome', 'Wage', 'Welfare','Investment')
am <- as_tibble(am)
d <- am/af
names(d) <- c('TotalIncome', 'Wage', 'Welfare','Investment')
rownames(d) <- 'Men vs Women'
g1 <-ggplot(ourdata2,aes(x=Income.Groupings,fill=GENDER))+geom_bar(position=position_dodge())+theme_minimal()
g1 + theme(axis.text.x=element_text(angle=-90))
d

## National Comparison

## Filtering the Consumer Expenditure Survey by AGe Groupings
Fem <- read_xlsx("Datasets National Data.xlsx", sheet = "SF by Age")
str(Fem)

Mal <- read_xlsx("Datasets National Data.xlsx", sheet = "SM by Age")
str(Mal)

## Choosing the rows with comparable variables to local Hamilton County data for both males and females
m<-  as.data.frame(Mal[163:170,8])
m
nm <- c(
  'PreTaxInc',
  'Wages',
  'Self-employment',
  'Social Security etc',
  'Interest, dividends& property',
  'Public assistance etc',
  'Unemployment and workers etc' ,
  'Other income')
rownames(m) <-nm
colnames(m) <- c( 'Single Men')

f<-  as.data.frame(Fem[163:170,8])
nf <- c(
  'PreTaxInc',
  'Wages',
  'Self-employment',
  'Social Security etc',
  'Interest, dividends& property',
  'Public assistance etc',
  'Unemployment and workers etc' ,
  'Other income')
rownames(f) <-nf
colnames(f) <- c( 'Single Women')

## Combining the table so male and female are in individual columns
mf <- cbind(m,f)
mf
x1 <-as.numeric(mf$`Single Men`)
x2<-as.numeric(mf$`Single Women`)

## Choosing the 6th and 7th variables in the rows (Public Assistance/Unemployment)
p <- x1[6]+x1[7]
q<- x2[6]+x2[7]

y1 <-c(x[1,1],x[2,1],x[3,1],x[5,1],p)
y2 <-c(x[1,2],x[2,2],x[3,2],x[5,2],q)

ny <- c(
  'PreTaxInc',
  'Wages',
  'Self-employment',
  'Interest, dividends& property',
  'Welfare' )

## Plots

par(mar=c(1,1,1,1))
y1 <-as.numeric(y1)
y2 <-as.numeric(y2)
pie(y1,main = "Single Men ",labels = ny, col = rainbow(length(ny)))
pie(y2,main = "Single Women ",labels = ny, col = rainbow(length(ny)) )


## Results
#Hamilton County

ggplot(ourdata2,aes(x=Income.Groupings,fill=GENDER))+geom_bar(position=position_dodge())+theme_minimal()
## Table showing comparion of men & women
d

#National Level Comparison

par(mar=c(1,1,1,1))
y1 <-as.numeric(y1)
y2 <-as.numeric(y2)
pie(y1,main = "Single Men ",labels = ny, col = rainbow(length(ny)))
pie(y2,main = "Single Women ",labels = ny, col = rainbow(length(ny)) )

## 1.  Compared to Men, the number of women earning some sort of 
#      income is higher in the Hamilton county . Signalling probably higher life expectancy of women

## 2.  At all levels Men earn more total income, wages and income from investments like stocks and real estate
#      Proving that women most probably 1) dont negotiate wages 2)do not invest 3) are not proficient at getting results from investment 
#      This trend is seen both in the county and at the national level.

## 3.  Women claim almost double the welfare funds than men in the Hamilton County. 
#      However this not the case at the national level

#  4.  Men seem to be greater risk takers- Higher investment income and Higher income of self employment


## Question 2

## Comparing Data to SSS & Filtering based  on age Groups
AbvSSS <- eda %>%
  select(GENDER, AGE, Age.Groupings, BCHELOR,HS, EMP, INCTOT, INCWAGE,
         INCWELFR ,INCINVST,Income.Groupings) %>%
  filter(INCTOT >= SSS[1,1]) %>%
  filter(Age.Groupings=="65 years and older")

BlwSSS <- eda %>% 
  select(GENDER, AGE, Age.Groupings, BCHELOR,HS, EMP, INCTOT, INCWAGE,
         INCWELFR ,INCINVST,Income.Groupings) %>%
  filter(INCTOT < SSS ) %>%
  filter(Age.Groupings=="65 years and older")
SSS

## Analysis for Below SSS
ggplot(BlwSSS,aes(x=INCTOT))+geom_histogram(fill = "green", col ="red", bins = 10)+scale_x_continuous("Total Income Distribution when below SSS")+scale_y_continuous("Population")
ggplot(BlwSSS,aes(x=INCWAGE))+geom_histogram(fill = "green", col ="red", bins = 10)+scale_x_continuous("Wage Distribution for Income < SSS")+scale_y_continuous("Population")

##Results
## The Total income shows a local peak around $10,000. 
#The Wages however peak only towards the lower end
# the higher earnings that lead to an income in the middle segment
#are on account of increased welfare and investment income

## Analysis for Above SSS
par(mar=c(1,1,1,1))
ggplot(AbvSSS,aes(x=INCTOT))+geom_histogram(fill = "blue", col ="orange", bins = 10)+scale_x_continuous("Total Income Distribution when above SSS")+scale_y_continuous("Population")+xlim(19650, 300000)
ggplot(AbvSSS,aes(x=INCWAGE))+geom_histogram(fill = "blue", col ="orange", bins = 10)+scale_x_continuous("Wage Distribution for Income >= SSS")+scale_y_continuous("Population") + xlim(19650, 300000)

## Results
## both the wage and total income distributions are in sync
# Wages for the biggest chunk of total income and all other forms are nominal

#Run regression models for analyses
WageB2 <-BlwSSS$INCWAGE
WelfrB2<- BlwSSS$INCWELFR
model1 <- lm(INCTOT~GENDER+HS+WageB2+INCINVST+WelfrB2, data = BlwSSS)

options(useFancyQuotes = FALSE)
summary(model1)


Wage2 <-AbvSSS$INCWAGE
Welfr2<- AbvSSS$INCWELFR
model1 <- lm(INCTOT~GENDER+HS+Wage2+INCINVST+Welfr2, data = AbvSSS)

options(useFancyQuotes = FALSE)
summary(model1)

## We find that Gender is irrelevant when income is above SSS.
#It is however moderately significant when income is below SSS

## Education  and wages are quite strongly related to Total income as expected


######### Question 3 - Needs Re-Labeling

## Start of Q.4
ourdata$Age.Groupings

#Compare younger and older age groups
dataforQ4 <- ourdata %>%
  filter(AGE <= 34 | AGE >= 55)
dataforQ4
str(dataforQ4)
pos.j <- position_jitter(width =0.3)
ggplot(dataforQ4, aes(x= INCTOT, fill = Age.Groupings))+geom_density(alpha = 0.3)
ggplot(dataforQ4, aes(x= INCWAGE, fill = Age.Groupings))+geom_density(alpha = 0.3)
ggplot(dataforQ4, aes(x= Age.Groupings,y = INCINVST, col = GENDER))+geom_point(position =pos.j)

FemSpending <- read_xlsx("Datasets National Data.xlsx", sheet = "SF by Age")
MalSpending <- read_xlsx("Datasets National Data.xlsx", sheet = "SM by Age")

#we compare age groups 25-34 and over 65 years olds as sample
#Then compare those groups by gender
MalSpending$'Under 25 Years' <-as.numeric(MalSpending$'Under 25 Years')
MalSpending$'65 years and older' <-as.numeric(MalSpending$'65 years and older')

MaleMil <-  c(MalSpending$X__2[48], MalSpending$X__2[76], MalSpending$X__2[78],
              MalSpending$X__2[120], MalSpending$X__2[133], MalSpending$X__2[139],
              MalSpending$X__2[151], MalSpending$X__2[159])

lbls <- c("Food", "Alcohol", "Housing", "Transportation", "Health Care",
          "Entertainment", "Education", "Personal Insurance and Pension")
pie(MaleMil,main = "Male Youngsters" ,labels = lbls, col = rainbow(length(lbls)))

MaleBoomer <-  c(MalSpending$X__7[48], MalSpending$X__7[76], MalSpending$X__7[78],
                 MalSpending$X__7[120], MalSpending$X__7[133], MalSpending$X__7[139],
                 MalSpending$X__7[151], MalSpending$X__7[159])
pie(MaleBoomer,main = "Male Boomers" ,labels = lbls, col = rainbow(length(lbls)))


FemSpending$X__2<-as.numeric(FemSpending$X__2)
FemSpending$X__7<-as.numeric(FemSpending$X__7)

FemMil <-  c(FemSpending$X__2[48], FemSpending$X__2[76], FemSpending$X__2[78],
             FemSpending$X__2[120], FemSpending$X__2[133], FemSpending$X__2[139],
             FemSpending$X__2[151], FemSpending$X__2[159])
pie(FemMil, main = "Female Youngsters",labels = lbls, col = rainbow(length(lbls)))

FemBoomer <-  c(FemSpending$X__7[48], FemSpending$X__7[76], FemSpending$X__7[78],
                FemSpending$X__7[120], FemSpending$X__7[133], FemSpending$X__7[139],
                FemSpending$X__7[151], FemSpending$X__7[159])
pie(FemBoomer, main = "Female Baby Boomers",labels = lbls, col = rainbow(length(lbls)))
