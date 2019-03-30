 library(ggplot2)
 
## logit run
logit <- glm(classification~ age+gpa+educ.f+sex.f+exp_n+distance_n, family = binomial(link = "logit"), data = df1)
summary(logit)
model <- glm(classification~ age+gpa+exp, family = binomial(link = "logit"), data = df1)
summary(model)

summary(df1$distance)
summary(df1$age)
summary(df1$exper)
summary(df1$gpa)
## predict values with temp data
temp.data =  data.frame(0,100,200,300,400,500,600,700,800,900,1000,1100,1200)




# Create a temporary data frame of hypothetical values
temp.data = data.frame(gpa = c(3,3,3,3,3,3,3,3,3,3,3,3,4),
                       age = c(21,22,23,21,22,23,21,22,23,21,22,23,21),
                       exp= c(0,10,20,30,40,50,60,70,90,110,130,140,170))

# Predict the fitted values given the model and hypothetical data
predicted.data <- as.data.frame(predict(model, newdata = temp.data, 
                                        type="link", se=TRUE))

# Combine the hypothetical data and predicted values
new.data <- cbind(temp.data, predicted.data)

# Calculate confidence intervals
std <- qnorm(0.95 / 2 + 0.5)
new.data$ymin <- model$family$linkinv(new.data$fit - std * new.data$se)
new.data$ymax <- model$family$linkinv(new.data$fit + std * new.data$se)
new.data$fit <- model$family$linkinv(new.data$fit)  # Rescale to 0-1

# Plot everything
p <- ggplot(df1, aes(x=exp, y=classification)) 
p + geom_point() + 
  geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax, 
                                 fill=as.factor(age)), alpha=0.5) + 
  geom_line(data=new.data, aes(y=fit, colour=as.factor(age))) + 
  labs(x="exp", y="classification") 
