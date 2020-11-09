# Md Karim Uddin, 02.11.2020, 
#Exercise 3
math<-read.table("student-mat.csv",sep=";",header=TRUE)
por<-read.table("student-por.csv",sep=";",header=TRUE)
#Joining 2 datasets

library(dplyr)
# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

# see the new column names
colnames(math_por)

# glimpse at the data
glimpse(math_por)
#The if-else structure

# dplyr, math_por, join_by are available

# print out the column names of 'math_por'
math_por

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining

notjoined_columns
# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data

glimpse(alc)


#Mutations
# alc is available

# access the 'tidyverse' packages dplyr and ggplot2
library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex") + geom_bar()

#So many plots
# alc is available

# access the tidyverse libraries tidyr, dplyr, ggplot2
 library(dplyr); library(ggplot2)
install.packages("devtools")
devtools::install_github("tidyverse/tidyverse")
library(tidyr)
# glimpse at the alc data
glimpse(alc)
# save the dataset alc-382 observations of 35 variables
write.table(alc, file = "alc.csv", sep=",")

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

# draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")+ geom_bar()
#------------------------------
#The pipe: summarising by group
# produce summary statistics by group
alc %>% group_by(sex) %>% summarise(count = n())

alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

#Box plots by groups

# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")

#Learning a logistic regression model

# alc is available 

# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)

#From coefficients to odds ratios
# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI<-confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

#Binary predictions (1)

# fit the model
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

#Binary predictions (2)

# alc is available

# access dplyr and ggplot2
library(dplyr); library(ggplot2)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% addmargins

#Accuracy and loss functions

# the logistic regression model m and dataset alc with predictions are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

#Cross Validation
# the logistic regression model m and dataset alc (with predictions) are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]
