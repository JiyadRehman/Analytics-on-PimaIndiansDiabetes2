install.packages("mlbench")
library(mlbench)

install.packages("randomForest")
library(randomForest)

#The Pima Indians Diabetes Data
data(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)

#Look at the data description
?PimaIndiansDiabetes2

#Remove missing values
DiabetesDat=na.omit(PimaIndiansDiabetes2)
dim(DiabetesDat)

#Divide the data in training and test sets
set.seed(1)
train = sample(1:nrow(DiabetesDat),0.7*nrow(DiabetesDat))
DiabetesTest = DiabetesDat$diabetes[-train]

length(train)
#The training set has 274 observations and test set has 118 observations.

#1.a) Fit a logistic regression model for diabetes status using all other variables
#   as predictors. Save the model and produce the model summary.

trainset <- DiabetesDat[train,]
testset <- DiabetesDat[-train,]

model <- glm(diabetes~., data = trainset, family = "binomial") 

summary(model)

'Call:
  glm(formula = diabetes ~ ., family = "binomial", data = trainset)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.6614  -0.6221  -0.3689   0.5992   2.2289  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -10.358795   1.433870  -7.224 5.03e-13 ***
  pregnant      0.066859   0.066798   1.001    0.317    
glucose       0.042347   0.007036   6.018 1.76e-09 ***
  pressure      0.015532   0.015175   1.024    0.306    
triceps       0.004652   0.021738   0.214    0.831    
insulin      -0.001177   0.001513  -0.778    0.437    
mass          0.053347   0.033385   1.598    0.110    
pedigree      1.064630   0.504847   2.109    0.035 *  
  age           0.013907   0.020692   0.672    0.502    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 344.01  on 273  degrees of freedom
Residual deviance: 236.13  on 265  degrees of freedom
AIC: 254.13

Number of Fisher Scoring iterations: 5
'



#b) Based on the summary produced in a), 
#    i. Which variables significantly affect the diabetes status? 
#    ii. How does a 1 unit increase in plasma glucose concentration affect the odds of having diabetes?

# i.
# Based on the p-values and coefficients it seems "glucose" to be the most important variable.

anova(model,test="Chisq")


# After anova test we can confirm that "glucose" is has the greatest significant.

# ii.

# the one unit increase in plasma gluscose will increase the log odds by 0.042347
exp(0.042347)
# One unit will increase in glucose will increase the odds by around 40% to be in a pos class.

#c) Predict the probabilities of diabetes on the test set. Predict the diabetes
#    status on the test set to be positive if the probability is > 0.5 and negative otherwise.
#    Calculate the test misclassification error.

chk <- predict(model, testset[-9], type = "response")

chk1 <- ifelse(chk>0.5,'pos','neg')

t <- table(DiabetesTest,chk1)

1-(sum(diag(t))/sum(t))

# Misclassification error = 0.220339

#2. a) Fit a random forest to predict diabetes status based on all the other variables.
#    Grow the random forest using 4 randomly chosen variables at each split and 5000 individual trees. 


rf <- randomForest(diabetes~.,data = trainset, mtry = 4, ntree = 5000)


# b) Which one is the most important variable for predicting diabetes status. 
#    Does it match with the logistic regression summary in 1.a?

varImpPlot(rf)

# Based on the importance variable plot "glucose" seems to be the most important variable.
# In 1.a based on the p-values we identified that "glucose" is the most important variable and it is same here as well.

# c) Predict the diabetes status on the test set based on the fitted random forest model. Calculate the
#   test misclassification error.

chkrf <- predict(rf,testset[-9])

t1 <- table(chkrf,DiabetesTest)

1-(sum(diag(t1))/sum(t1))

# Misclassification error = 0.2542373

