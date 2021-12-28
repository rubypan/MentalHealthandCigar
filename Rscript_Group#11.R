# The Relationship Between Mental Health and the Number of Cigarette Consumed
# Group: 11
# Name: Yichen Pan, Aidan Medina

#### Install packages needed for this code
# The step below will install the ggplot2 package, click Yes to install
install.packages('ggplot2') # install the package
# the code below will install the leaps package for best-subsets
install.packages('leaps')

###### Make a new dataframe
group11data <- data.frame(addhealth_public4$h4to6, 
                    addhealth_public4$h4to4,
                    addhealth_public4$h4to30,
                    addhealth_public4$h4mh2,
                    addhealth_public4$h4mh22,
                    addhealth_public4$h4mh23,
                    addhealth_public4$h4id5j,
                    addhealth_public4$h4pe22,
                    addhealth_public4$h4to27)

###### Rename the variables
names(group11data) <- c('num_cigar',
                  'age_first_smoke',
                  'times_quit_smoke',
                  'feel_isolated',
                  'feel_depressed',
                  'feel_tired',
                  'anxiety',
                  'stress',
                  'try_quit_smoke')

###### Determine sample size 
# 0.5 point deduction for lacking this part from the comments on Preliminary Proposal
# 1. num_cigar
group11data$num_cigar[group11data$num_cigar > 100] <- NA # replace codes
sum(!is.na(group11data$num_cigar)) # find sample size

# 2. age_first_smoke
group11data$age_first_smoke[group11data$age_first_smoke > 31] <- NA
sum(!is.na(group11data$age_first_smoke))

# 3. times_quit_smoke
group11data$times_quit_smoke[group11data$times_quit_smoke > 25] <- NA
sum(!is.na(group11data$times_quit_smoke))

# 4. feel_isolated
group11data$feel_isolated[group11data$feel_isolated > 3] <- NA
sum(!is.na(group11data$feel_isolated))

# 5. feel_depressed
group11data$feel_depressed[group11data$feel_depressed > 3] <- NA
sum(!is.na(group11data$feel_depressed))

# 6. feel_tired
# do not need to replace the code with NA because all of them are meaningful
sum(!is.na(group11data$feel_tired))

# 7. anxiety
group11data$anxiety[group11data$anxiety > 1] <- NA
sum(!is.na(group11data$anxiety))

# 8. stress (added after reviewing related work)
group11data$stress[group11data$stress > 5] <- NA
sum(!is.na(group11data$stress))

# 9. try_quit_smoke
group11data$try_quit_smoke[group11data$try_quit_smoke > 1] <- NA
sum(!is.na(group11data$try_quit_smoke))

#### Final Proposal
##### recode the categorical variables according to the codebook
### Below are quantatative variables do not need to recode
# 1. num_cigar
# 2. age_first_smoke
# 3. times_quit_smoke

### recode for categorical variables 
# 4. feel_isolated
group11data$feel_isolated[group11data$feel_isolated==0] <- "never"
group11data$feel_isolated[group11data$feel_isolated==1] <- "rarely"
group11data$feel_isolated[group11data$feel_isolated==2] <- "sometimes"
group11data$feel_isolated[group11data$feel_isolated==3] <- "often"

# 5. feel_depressed
group11data$feel_depressed[group11data$feel_depressed==0] <- "never or rarely"
group11data$feel_depressed[group11data$feel_depressed==1] <- "sometimes"
group11data$feel_depressed[group11data$feel_depressed==2] <- "a lot of the time"
group11data$feel_depressed[group11data$feel_depressed==3] <- "most of the time or all of the time"

# 6. feel_tired
group11data$feel_tired[group11data$feel_tired==0] <- "never or rarely"
group11data$feel_tired[group11data$feel_tired==1] <- "somtimes"
group11data$feel_tired[group11data$feel_tired==2] <- "a lot of the time"
group11data$feel_tired[group11data$feel_tired==3] <- "most of the time or all of the time"

# 7. anxiety
group11data$anxiety[group11data$anxiety==0] <- "no"
group11data$anxiety[group11data$anxiety==1] <- "yes"

# 8. stress
group11data$stress[group11data$stress==1] <- "strongly agree"
group11data$stress[group11data$stress==2] <- "agree"
group11data$stress[group11data$stress==3] <- "neither agree nor disagree"
group11data$stress[group11data$stress==4] <- "disagree"
group11data$stress[group11data$stress==5] <- "strongly disagree"

# 9. try_quit_smoke
group11data$try_quit_smoke[group11data$try_quit_smoke==0] <- "no"
group11data$try_quit_smoke[group11data$try_quit_smoke==1] <- "yes"

##### Create a new dataframe for which the response variable has no missing values
# response variable: 1. num_cigar
myproject <- group11data[!is.na(group11data$num_cigar),]

##### Calculate the sample size for each variables
nrow(myproject) # 1847
# 1. num_cigar
sum(!is.na(myproject$num_cigar)) # 1847
# 2. age_first_smoke
sum(!is.na(myproject$age_first_smoke)) # 1649
# 3. times_quit_smoke
sum(!is.na(myproject$times_quit_smoke)) # 341
# 4. feel_isolated
sum(!is.na(myproject$feel_isolated)) # 1847
# 5. feel_depressed
sum(!is.na(myproject$feel_depressed)) # 1847
# 6. feel_tired
sum(!is.na(myproject$feel_tired)) # 1847
# 7. anxiety
sum(!is.na(myproject$anxiety)) # 1846
# 8. stress (added after reviewing related work)
sum(!is.na(myproject$stress)) # 1845
# 9. try_quit_smoke
sum(!is.na(myproject$try_quit_smoke)) # 1465


#### Exploratory Data Analysis

### Response Variable
# response variable - number of cigarette
nrow(myproject) # sample size
mean(myproject$num_cigar) # mean
median(myproject$num_cigar) # median
sd(myproject$num_cigar) # standard deviation
hist(myproject$num_cigar, xlab = 'Number of Cigarettes',
     main='Histogram of Number of Cigarettes Consumed')
# shape, center, and spread of this variable

### Explanatory Variables
### 1. quantitative variable - age of first smoke
hist(myproject$age_first_smoke, xlab = 'Age of first smoke',
     main='Histogram of Age of First Smoke')
sum(!is.na(myproject$age_first_smoke)) # sample size
mean(myproject$age_first_smoke, na.rm = T) # mean
sd(myproject$age_first_smoke, na.rm = T) # standard deviation

# Analyze relationship between response variable and age of first smoke
plot(myproject$age_first_smoke, myproject$num_cigar, 
     xlab='Age of first smoke', ylab='Number of cigar')
cor(myproject$age_first_smoke, myproject$num_cigar,
    use='complete.obs')
# With ggplot2
library(ggplot2)
ggplot(data=myproject) + 
  geom_jitter(data=myproject, aes(x=age_first_smoke, y=num_cigar),
              alpha=0.2, color='blue') +
  labs(caption = 'Correlation = -0.1966',
       x='Age of first smoke',
       y='Number of Cigar')


### 2. categorical variable - feel_depressed
# keep new feel_depressed categories in order
myproject$feel_depressed2 <- factor(myproject$feel_depressed,
                                    levels=c('most of the time or all of the time',
                                             'a lot of the time',
                                             'sometimes',
                                             'never or rarely'), ordered=TRUE)
# numerical and graphical summary
table(myproject$feel_depressed2)
barplot(table(myproject$feel_depressed2),
        main = 'Feel Depressed', ylab = 'Frequency')

sum(!is.na(myproject$feel_depressed2)) # sample size

# Analyze relationship between response variable and feel depressed
# side by side boxplot for comparing groups
boxplot(myproject$num_cigar ~ myproject$feel_depressed2,
        xlab='Frequency of Feeling Depressed', ylab='Number of Cigarettes')
# compare mean response in different categories
tapply(myproject$num_cigar, myproject$feel_depressed2,
       mean, na.rm=T)
# # compare standard deviation response in different categories
tapply(myproject$num_cigar, myproject$feel_depressed2,
       sd, na.rm=T)

### 3. categorical variable - anxiety
# numerical and graphical summary
table(myproject$anxiety)
barplot(table(myproject$anxiety),
        main = 'Diagnosed with Anxiety', ylab = 'Frequency')
sum(!is.na(myproject$anxiety)) # sample size

# Analyze relationship between response variable and anxiety
# side by side boxplot for comparing groups
boxplot(myproject$num_cigar ~ myproject$anxiety,
        xlab='Diagnosed with Anxiety', ylab='Number of Cigarettes')
# compare mean response in different categories
tapply(myproject$num_cigar, myproject$anxiety,
       mean, na.rm=T)
# compare standard deviation response in different categories
tapply(myproject$num_cigar, myproject$anxiety,
       sd, na.rm=T)


### 4. quantitative variable - times quit smoke
hist(myproject$times_quit_smoke, xlab = 'Times Tried to Quit Smoking',
     main='Histogram of Times Tried to Quit Smoking')
sum(!is.na(myproject$times_quit_smoke)) # sample size
mean(myproject$times_quit_smoke, na.rm = T) # mean
sd(myproject$times_quit_smoke, na.rm = T) # standard deviation

# Analyze relationship between response variable and age of first smoke
plot(myproject$times_quit_smoke, myproject$num_cigar, 
     xlab='Times Tried to Quit Smoking', ylab='Number of cigar')
cor(myproject$times_quit_smoke, myproject$num_cigar,
    use='complete.obs')
# With ggplot2
library(ggplot2)
ggplot(data=myproject) + 
  geom_jitter(data=myproject, aes(x=times_quit_smoke, y=num_cigar),
              alpha=0.2) +
  labs(caption = 'Correlation = 0.01486',
       x='Times Tried to Quit Smoking',
       y='Number of Cigar')

### 5. categorical variable - feel isolated
myproject$feel_isolated2 <- factor(myproject$feel_isolated,
                                    levels=c('never',
                                             'rarely',
                                             'sometimes',
                                             'often'), ordered=TRUE)
# numerical and graphical summary
table(myproject$feel_isolated2)
barplot(table(myproject$feel_isolated2),
        main = 'Frequency of Feeling Isolated', ylab = 'Frequency')
sum(!is.na(myproject$feel_isolated2)) # sample size

# Analyze relationship between response variable and feel depressed
# side by side boxplot for comparing groups
boxplot(myproject$num_cigar ~ myproject$feel_isolated2,
        xlab='Frequency of Feeling Isolated', ylab='Number of Cigarettes')
# compare mean response in different categories
tapply(myproject$num_cigar, myproject$feel_isolated2,
       mean, na.rm=T)
# # compare standard deviation response in different categories
tapply(myproject$num_cigar, myproject$feel_isolated2,
       sd, na.rm=T)

### 6. categorical variable - feel_tired
myproject$feel_depressed2 <- factor(myproject$feel_depressed,
                                    levels=c('most of the time or all of the time',
                                             'a lot of the time',
                                             'sometimes',
                                             'never or rarely'), ordered=TRUE)
# numerical and graphical summary
table(myproject$feel_tired)
barplot(table(myproject$feel_tired),
        main = 'Feel Tired', ylab = 'Frequency')
sum(!is.na(myproject$feel_tired)) # sample size

# Analyze relationship between response variable and feel depressed
# side by side boxplot for comparing groups
boxplot(myproject$num_cigar ~ myproject$feel_tired,
        xlab='Feel Tired', ylab='Number of Cigarettes')
# compare mean response in different categories
tapply(myproject$num_cigar, myproject$feel_tired,
       mean, na.rm=T)
# # compare standard deviation response in different categories
tapply(myproject$num_cigar, myproject$feel_tired,
       sd, na.rm=T)

### 7. categorical variable - stress
myproject$stress2 <- factor(myproject$stress,
                                    levels=c('strongly disagree',
                                             'disagree',
                                             'neither agree nor disagree',
                                             'agree',
                                             'strongly agree'), ordered=TRUE)
# numerical and graphical summary
table(myproject$stress2)
barplot(table(myproject$stress2),
        main = 'Easy to stress out', ylab = 'Frequency')
sum(!is.na(myproject$stress2)) # sample size

# Analyze relationship between response variable and feel stress
# side by side boxplot for comparing groups
boxplot(myproject$num_cigar ~ myproject$stress2,
        xlab='Easy to stress out', ylab='Number of Cigarettes')
# compare mean response in different categories
tapply(myproject$num_cigar, myproject$stress2,
       mean, na.rm=T)
# # compare standard deviation response in different categories
tapply(myproject$num_cigar, myproject$stress2,
       sd, na.rm=T)

### 8. categorical variable - try_quit_smoke
# numerical and graphical summary
table(myproject$try_quit_smoke)
barplot(table(myproject$try_quit_smoke),
        main = 'Try to quit smoke', ylab = 'Frequency')
sum(!is.na(myproject$try_quit_smoke)) # sample size

# Analyze relationship between response variable and feel stress
# side by side boxplot for comparing groups
boxplot(myproject$num_cigar ~ myproject$try_quit_smoke,
        xlab='Try to quit smoke', ylab='Number of Cigarettes')
# compare mean response in different categories
tapply(myproject$num_cigar, myproject$try_quit_smoke,
       mean, na.rm=T)
# # compare standard deviation response in different categories
tapply(myproject$num_cigar, myproject$try_quit_smoke,
       sd, na.rm=T)

#### Find Correlation between two quantitative variables to test colinearity
# added this part after points deduction for exploratory data analysis
cor(myproject$age_first_smoke, myproject$times_quit_smoke, 
    use = "complete.obs")
cor(myproject[c(1,2,3)], use="complete.obs")

########### Final
#### Data Cleaning
myproject$age_first_smoke[is.na(myproject$age_first_smoke)] <-
  mean(myproject$age_first_smoke, na.rm=T)
myproject$times_quit_smoke[is.na(myproject$times_quit_smoke)] <-
  mean(myproject$times_quit_smoke, na.rm=T)

#### Find outliers with boxplot and histogram
boxplot(myproject$num_cigar, horizontal = T)
hist(myproject$num_cigar)
boxplot(myproject$age_first_smoke, horizontal = T)
hist(myproject$age_first_smoke) 
boxplot(myproject$times_quit_smoke, horizontal = T)
hist(myproject$times_quit_smoke)

#### Split Data into Training and Testing
set.seed(11) # choose a seed so results can be reproduced, 11 is group num
# take 30% of n=1847 cases for the test set
1847 * 0.3 # test set
1847 * 0.7 # training
test.cases <- sample(1:1847, 554)
test.set <- myproject[test.cases,]
training.cases <- setdiff(1:1847, test.cases)
training.set <- myproject[training.cases,]

library(leaps)
# plot the best-subset
plot(regsubsets(num_cigar ~ `age_first_smoke` + `times_quit_smoke` +
                  `feel_isolated` + `feel_depressed` + `feel_tired` +
                  `anxiety` + `stress` + `try_quit_smoke`, 
                data=training.set), scale="adjr2")

#### Backward Stepwise Regression
# contain each variable
summary(lm(`num_cigar` ~ `age_first_smoke` + `times_quit_smoke` +
              `feel_isolated` + `feel_depressed` + `feel_tired` +
              `anxiety` + `stress` + `try_quit_smoke`, data=training.set))
# detele times_quit_smoke
summary(lm(`num_cigar` ~ `age_first_smoke` +
             `feel_isolated` + `feel_depressed` + `feel_tired` +
             `anxiety` + `stress` + `try_quit_smoke`, data=training.set))
# delete stress
summary(lm(`num_cigar` ~ `age_first_smoke` +
             `feel_isolated` + `feel_depressed` + `feel_tired` +
             `anxiety` + `try_quit_smoke`, data=training.set))
# delete try_quit_smoke
summary(lm(`num_cigar` ~ `age_first_smoke` +
             `feel_isolated` + `feel_depressed` + `feel_tired` +
             `anxiety`, data=training.set))
# delete anxiety
summary(lm(`num_cigar` ~ `age_first_smoke` +
             `feel_isolated` + `feel_depressed` + `feel_tired`, 
           data=training.set))
# delete feel_depressed
summary(lm(`num_cigar` ~ `age_first_smoke` +
             `feel_isolated` + `feel_tired`, 
           data=training.set))
# delete feel_tired
summary(lm(`num_cigar` ~ `age_first_smoke` + `feel_isolated`, 
           data=training.set))

#### Check Assumptions
final_model <- lm(num_cigar ~ age_first_smoke + feel_isolated, 
                  data=training.set)
final_model
plot(final_model)
hist(residuals(final_model))

#### Apply Transformation
log_trans_response <- lm(log(num_cigar) ~ age_first_smoke + feel_isolated, 
                         data=training.set)
plot(log_trans_response)
log_trans_explanatory <- lm(num_cigar ~ log(age_first_smoke) + feel_isolated, 
                            data=training.set)
plot(log_trans_response)
log_trans <- lm(log(num_cigar) ~ log(age_first_smoke) + feel_isolated, 
                data=training.set)
plot(log_trans)
hist(residuals(log_trans))
sqrt_trans <- lm(num_cigar^0.5 ~ age_first_smoke + feel_isolated, 
                 data=training.set)
plot(sqrt_trans)
sqrt_trans_response <- lm(num_cigar^2 ~ age_first_smoke + feel_isolated, 
                          data=training.set)
plot(sqrt_trans_response)

# Because transformation doesn't seem to very helpful to improve the orginal model
# we use the original model
final_model <- lm(num_cigar ~ age_first_smoke + feel_isolated, 
                  data=training.set)
summary(final_model)

# 95% confident interval
confint(final_model, level = 0.95)

# example prediction and prediction interval: case 1674, the first row in test.set
predict.lm(final_model, test.set[1,])
predict.lm(final_model, test.set[1,], interval = 'prediction')

# Compare the mean absolute error between training and test set
mean(abs(residuals(final_model))) # mean absolute error for training set
predicted.cigar <- predict(final_model, test.set) 
predicted.cigar
mean(abs(predicted.cigar - test.set$num_cigar), na.rm = T) # mean absolute error for test set
# R-squared for the testing set
cor(test.set$num_cigar, predicted.cigar, use = "complete.obs") ^2





