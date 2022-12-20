# CSTAT       | Case 22-145
# Client      | Ban Al-Sahab
# Code Author | Jordan Tait
# Modified    | Barry DeCicco


#### Materials from 'R_ImputationExample_Ban.R'    ####

# From P:\Consulting\FY22\Al-Sahab_Ban\22-145\Analyses

#### Load Libraries                                 ####
library(dplyr)
library(mice)
library(mediation)
library(magrittr)


#### Create and prepare data                       ####

# Simulate data
df <- data.frame(matrix(data = sample(100,200,replace=TRUE), ncol = 5))
# Replace some values of X5 as NA's
df[,5][df[,5]>90] <- NA

# Name dataset "mydata"
mydata<-df

# check for missing
summary(mydata)

# Barry's additions:
#   Make column three a binary treatment / control indicator.
#   Make column 4 depend on C1,-C3 in a strong mediator model.
#   X1 will be the causal variable
#   X2 will be the outcome
#   X3 will be the mediator
#   X4 and X5 will not be used in this equation.

mydata$X3 <- rbinom(40,1,0.5)
mydata$X2 <- mydata$X1 + 50*mydata$X3


mydata %>%
  group_by(X3) %>%
  summary()

#
# # Removes column by position (column 2)
# mydata.subset<-subset(mydata,select = -c(2))
# mydata.subset
#
# # Removes column by name ( column named X3)
# mydata.subset<-subset(mydata,select = -c(X3))
# mydata.subset
#
# # Subsets by column position (column 2 and 3)
# mydata.subset<-subset(mydata,select = c(2,3))
# mydata.subset
#
# # Subsets by column name (columns called )
# mydata.subset<-subset(mydata,select = c(X1,X4))
# mydata.subset
#


#### Set-up variable and imputation method                      ####
#
#
########################################################################################


# create dataset with imputation options to toggle
mydata.imputsetup<-mice(mydata,maxit=0)

# Extract predictor matrix and method of imputations
predmatrix<-mydata.imputsetup$predictorMatrix
imputemethods<-mydata.imputsetup$method




####     Choose Predictors for imputation                      ####

# View default predictor matrix
predmatrix

# All variable selected by default for prediction step
# change value of variables to "0" in order to leave them out

# Lines 37-42


predmatrix[, c("X1")] <- 0
predmatrix[, c("X2")] <- 0
#predmatrix[, c("X3")] <- 0
#predmatrix[, c("X4")] <- 0
predmatrix[, c("X5")] <- 0

# View updated prediction matrix
predmatrix

# Columns used will have 1's except one row
# Columns not used will have all 0's


####    Choose variables to impute, and method for imputation  ####

# Check method matrix
imputemethods

# Turn off imputation for all variables
mydata.names<-names(mydata)
imputemethods[mydata.names]<-""

# List variables to be imputed
imputemethods[c("X1")]<-""
imputemethods[c("X2")]<-""
imputemethods[c("X3")]<-""
imputemethods[c("X4")]<-""
imputemethods[c("X5")]<-"pmm"

# See list of possible methods of imputation using the link below:
# https://search.r-project.org/CRAN/refmans/mice/html/mice.html



# Varify imputation methods
imputemethods




####             Imputation Step                ####


# Run imputation 20 times
# "m" is the number of imputations
# Uses pre-defined prediction variables from the predmatrix
# Imputes variables using defined method from the imputemethods matrix
mydata.imputed <- mice(mydata,
                       m=20,
                       maxit = 5,
                       seed = 4 ,
                       predictorMatrix = predmatrix,
                       method = imputemethods,
                       print =  TRUE,
                       ridge=0.0001,
                       threshold=1)





# Define each imputed dataset with it's own name
# "1" means first datat set
completedData.1 <- complete(mydata.imputed,1)
completedData.2 <- complete(mydata.imputed,2)
completedData.3 <- complete(mydata.imputed,3)
completedData.4 <- complete(mydata.imputed,4)
completedData.5 <- complete(mydata.imputed,5)
completedData.6 <- complete(mydata.imputed,6)
completedData.7 <- complete(mydata.imputed,7)
completedData.8 <- complete(mydata.imputed,8)
completedData.9 <- complete(mydata.imputed,9)
completedData.10 <- complete(mydata.imputed,10)
completedData.11 <- complete(mydata.imputed,11)
completedData.12 <- complete(mydata.imputed,12)
completedData.13 <- complete(mydata.imputed,13)
completedData.14 <- complete(mydata.imputed,14)
completedData.15 <- complete(mydata.imputed,15)
completedData.16 <- complete(mydata.imputed,16)
completedData.17 <- complete(mydata.imputed,17)
completedData.18 <- complete(mydata.imputed,18)
completedData.19 <- complete(mydata.imputed,19)
completedData.20 <- complete(mydata.imputed,20)


####       Making a variable binary                                        ####

# Defines a new variable, X6, based on X1
# If X1 is less than 7, X1 = 1
# If X1 is more than or equal to 7, X1 = 0
# using "Find and Replace" is a fast way to change this chunk of code
# to fit your data (variable names)
completedData.1$X6  <- ifelse(completedData.1$X1 < 7, 1, 0)
completedData.2$X6  <- ifelse(completedData.2$X1 < 7, 1, 0)
completedData.3$X6  <- ifelse(completedData.3$X1 < 7, 1, 0)
completedData.4$X6  <- ifelse(completedData.4$X1 < 7, 1, 0)
completedData.5$X6  <- ifelse(completedData.5$X1 < 7, 1, 0)
completedData.6$X6  <- ifelse(completedData.6$X1 < 7, 1, 0)
completedData.7$X6  <- ifelse(completedData.7$X1 < 7, 1, 0)
completedData.8$X6  <- ifelse(completedData.8$X1 < 7, 1, 0)
completedData.9$X6  <- ifelse(completedData.9$X1 < 7, 1, 0)
completedData.10$X6 <- ifelse(completedData.10$X1 < 7, 1, 0)
completedData.11$X6 <- ifelse(completedData.11$X1 < 7, 1, 0)
completedData.12$X6 <- ifelse(completedData.12$X1 < 7, 1, 0)
completedData.13$X6 <- ifelse(completedData.13$X1 < 7, 1, 0)
completedData.14$X6 <- ifelse(completedData.14$X1 < 7, 1, 0)
completedData.15$X6 <- ifelse(completedData.15$X1 < 7, 1, 0)
completedData.16$X6 <- ifelse(completedData.16$X1 < 7, 1, 0)
completedData.17$X6 <- ifelse(completedData.17$X1 < 7, 1, 0)
completedData.18$X6 <- ifelse(completedData.18$X1 < 7, 1, 0)
completedData.19$X6 <- ifelse(completedData.19$X1 < 7, 1, 0)
completedData.20$X6 <- ifelse(completedData.20$X1 < 7, 1, 0)

#### Materils from
# single list of above datasets
datasets.df <- list(
  D1=completedData.1,
  D2=completedData.2,
  D3=completedData.3,
  D4=completedData.4,
  D5=completedData.5,
  D6=completedData.6,
  D7=completedData.7,
  D8=completedData.8,
  D9=completedData.9,
  D10=completedData.10,
  D11=completedData.11,
  D12=completedData.12,
  D13=completedData.13,
  D14=completedData.14,
  D15=completedData.15,
  D16=completedData.16,
  D17=completedData.17,
  D18=completedData.18,
  D19=completedData.19,
  D20=completedData.20
)



#### Materials from 'MulipleMediationExample_JordanTait.R'    ####

# From P:\Consulting\FY22\Al-Sahab_Ban\22-145\Analyses



# define mediator(s)
mediators.df <- c("X2")

# define outcome(s)
outcome.df <- c("X3")

# define treament variables
treatment.df <- c("X6") # note how the treatment indicator is repeated

# define pre-treatment covariates
# covariates.df <- c("X1+X2")

# Perform medation on all datasets in list
mymediations <- mediations(datasets=datasets.df,
                           treatment=treatment.df,
                           mediators=mediators.df,
                           outcome=outcome.df,
#                           covariates=covariates.df,
                           families=c("gaussian","gaussian"),
                           interaction=FALSE,
                           conf.level=.95,
                           sims=1000)

# Pool results from mediations
mymediations.results <- amelidiate(mymediations)

# Output from pooled mediation
summary(mymediations.results)
plot(mymediations.results)




#### Barry's Materials                     ####



