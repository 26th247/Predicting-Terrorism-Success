ML.terrorism.data <- read_csv("C:/Users/Harun/Documents/Data Sets/globalterrorism.database.csv")

library(tidyverse)
library(readr)
library(dplyr)
library(caret)
library(olsrr)
library(Boruta)
library(mice)

## Select specific columns of interest and rename them
ML.terrorism.data <- ML.terrorism.data %>%
  select( region_txt, success, suicide, attacktype1_txt, weaptype1_txt, targtype1_txt, nkill, nwound) %>%
  filter(nkill > 0) %>%
  rename(
    region = region_txt,
    killed = nkill,
    attacktype = attacktype1_txt,
    weapon = weaptype1_txt,
    target = targtype1_txt,
    wounded = nwound
  )


#change categorical to factor
ML.terrorism.data <- ML.terrorism.data %>%
  mutate(across(.cols = c(
  "attacktype", "weapon", "target", "region", "success"), .fns = factor))

summary(ML.terrorism.data)
str(ML.terrorism.data)

output <- lm(success~region+attacktype+weapon+killed+suicide+target, data = ML.terrorism.data)

plot

summary(output)

                                                          # MEAN IMPUTATION # 
head(ML.terrorism.data)

# Checking out the N/A's that we are working with...
sum(is.na(ML.terrorism.data))

# under 5% of the data is missing '6792' and all in wounded variable 
# this is good for the data as any more than 5% and the variable should be thrown

unique(ML.terrorism.data$wounded)

#dealing with mice
#lets take a look at the missing data patterns
md.pattern(ML.terrorism.data)

#an even better representation
library(VIM)
aggr_plot <- aggr(ML.terrorism.data, col=c('navyblue', 'red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(ML.terrorism.data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram for Missing Data", "Pattern"))


#check out the regression methods for the imputation
methods(mice)

#go ahead and try mean imputation as we are only dealing with numbers here
imputed_data <- mice(ML.terrorism.data, m=5, method = "2l.lmer")
summary(imputed_data)

imputed_data$imp$wounded

#complete data set with imputed data

terrorism.imputed.data <- complete(imputed_data, 1)

ML.terrorism.data <- terrorism.imputed.data

#neat check for any missing values

sapply(ML.terrorism.data, function(x) sum(is.na(x))) #looks good


                                      # different variable selection methods #



# BACKWARD REGRESSION USING P-VALUES - BACKWARD ELIMINATION METHOD
model <- lm(success ~ region+attacktype+weapon+killed+suicide+target+wounded, data = ML.terrorism.data)

BWDfit.p <- ols_step_backward_p(model, prem = .08, details = TRUE) 
# in this case we identify variables that are insignificant contributors to the model
# select predictors that have the least incremental predictive power to the model
# essentially removing predictors and if the r-square changes in the new model then this one is preferred

BWDfit.p


# USING BORUTA
set.seed(111)

boruta1 <- Boruta(success ~., data = ML.terrorism.data, doTrace=2)

decision<- boruta1$finalDecision

signif <- decision[boruta1$finalDecision %in% c("confirmed")]

print(signif)

plot(boruta1, xlab = "", main="Boruta Method")

print(boruta1)
getConfirmedFormula(boruta1)

# logistic regression 

#split data
library("caTools")
split <- sample.split(ML.terrorism.data, SplitRatio = 0.75)
split

#subset training and testing
set.seed(1234)
training  <- subset(ML.terrorism.data, split == "TRUE")
testing <-subset(ML.terrorism.data, split == "FALSE")

#create the log method
model <- glm(success ~ attacktype+year+suicide+wounded, data = training, family="binomial")
summary(model)

#put response in 'res'
res <- predict(model, training, type = "response")
head(res)
head(training)

#confusion matrix table
pred1 <- ifelse(res>0.8, 1, 0)
tab1 <- table(Predicted =pred1, Actual = training$success)
tab1


# analysis of logistic regression model and test of model fit

modelSummary <- summary(model)
modelSummary$coefficients

# use AIC/BIC functions

AIC(model)
BIC(model)


