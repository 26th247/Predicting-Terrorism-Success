ML.terrorism.data <- read.csv("C:/Users/Harun/Documents/Data Sets/globalterrorism.database.csv")

library(tidyverse)
library(readr)
library(dplyr)
library(caret)
library(olsrr)
library(Boruta)
library(mice)
library(e1071)
library(caTools)
library(ggplot2)
library(naivebayes)
library(psych)

## Select specific columns of interest and rename them
ML.terrorism.data <- ML.terrorism.data %>%
  select(iyear, region_txt, success, suicide, attacktype1_txt, weaptype1_txt, targtype1_txt, nkill, nwound) %>%
  filter(nkill > 0) %>%
  rename(
    year = iyear,
    region = region_txt,
    killed = nkill,
    attacktype = attacktype1_txt,
    weapon = weaptype1_txt,
    target = targtype1_txt,
    wounded = nwound
  )

#categorical as factors
ML.terrorism.data <- ML.terrorism.data %>%
  mutate(across(.cols = c(
    "attacktype", "weapon", "target", "region","success", "suicide"), .fns = factor))
str(ML.terrorism.data)

#impute wounded quickly 
imputed_data <- mice(ML.terrorism.data, m=5, method = "2l.lmer")
summary(imputed_data)

imputed_data$imp$wounded

terrorism.imputed.data <- complete(imputed_data, 1)

ML.terrorism.data <- terrorism.imputed.data


#Naive bayes

check.correl <- ML.terrorism.data %>%
  select(killed,suicide,wounded,region,year)

#visualation
pairs.panels(check.correl)

check.correl %>% ggplot(aes(x=year, fill =success)) +
  geom_density(alpha=0.8, color = 'black') +
  ggtitle("Density Plot")

#data parition
split <- sample.split(ML.terrorism.data, SplitRatio = 0.75)
split

nb.training  <- subset(ML.terrorism.data, split == "TRUE")
nb.testing <-subset(ML.terrorism.data, split == "FALSE")

nb.model <- naive_bayes(success ~., data = nb.training)
nb.model

plot(nb.model)

#predict
p <- predict(nb.model, nb.training, type = "prob")
predicted.nb<-(cbind(p, train))
predicted.nb

#confusion matrix - train data
p1 <- predict(nb.model, nb.training)
(tab1 <- table(p1, nb.training$success))

#misclassification error percentage
1 - sum(diag(tab1)) / sum(tab1)

#confusion matrix - test data
p2 <- predict(nb.model, nb.testing)
(tab2 <- table(p2, nb.testing$success))

1 - sum(diag(tab2)) / sum(tab2)

