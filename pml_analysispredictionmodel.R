# Practical Machine Learning Course project: Peer assesment
# Analysis script

# Upload training data
datatraining <- read.csv("data/pml-training.csv")
str(datatraining)
summary(datatraining)

# Variable to predict: "classe"
# Choosing predictors...
# Too complicate to visuaize...
# check correlation between variables to eliminate variables to much correlate
library(caret)

# eliminate the first 7 variables and the last: "classe" the one to predict
subdatatraining <-datatraining[, 8:159] 

# eliminate varibles with nan
subdatatraining <- subdatatraining[ , colSums(is.na(subdatatraining)) == 0]

# to calculate the covariance matrix I need to have only number not factors, then 
# I have to convert (nothing is working!)
factorsdata <- subdatatraining[, c(5:13, 36:41, 45:53, 67:75)]
numericdata <- as.numeric(levels(factorsdata))[factorsdata]
# as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
#numericdata <- seq_along(levels(x))[x]
# as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

#subdatatraining[, c(5:13, 36:41, 45:53, 67:75)] <- sapply(subdatatraining[, c(5:12, 33:38, 41:49, 63:71)], as.numeric(levels(c(5:12, 33:38, 41:49, 63:71)))[c(5:12, 33:38, 41:49, 63:71)])
#df2[, c(11, 17, 23)] <- sapply(df2[, c(11, 17, 23)], as.numeric)
# vsubdatatraining[sapply(subdatatraining, is.factor)] <- lapply(subdatatraining[sapply(subdatatraining, is.factor)], function(x) as.numeric(as.character(x)))
# This function put factors to NAN
# subdatatraining[sapply(subdatatraining, is.factor)] <- lapply(subdatatraining[sapply(subdatatraining, is.factor)], function(x) as.numeric(levels(x))[x])
# This function put factors to 1???
subdatatraining[sapply(subdatatraining, is.factor)] <- lapply(subdatatraining[sapply(subdatatraining, is.factor)], function(x) seq_along(levels(x))[x])

# Calculate the covariance matrix
covmatrix <- cor(subdatatraining[,-1])

# Find indices to eliminate
indextoremove <- findCorrelation(covmatrix, cutoff = .90, verbose = FALSE)
indextoremove
# remove features!
finaltrainingdata <- subset(subdatatraining, select = -c(6,9,46,49,68,71, 52,12,72,73,4,44,51,11,18,17,16,27,78,54) )
str(finaltrainingdata)
classecol <- datatraining[, 160] 
finaltrainingdata <- cbind(finaltrainingdata, classecol)

############################
###########################
# INSTEAD OF THIS TRY PCA!!!!
# Models to fit!
# glm
modelFit1 <-train(finaltrainingdata$classecol ~ ., method="glm", data=finaltrainingdata)
# NOT WORK!!

# try another one!
modelFit2 <- train(finaltrainingdata$classecol ~ .,method="rpart",data=finaltrainingdata) 
library("rattle")
fancyRpartPlot(modelFit2$finalModel) # it seems ok!

# let tests it!
# Upload testing data
datatesting <- read.csv("data/pml-testing.csv")

#same preprocessing as in training for select features! OR NOT???
subdatatesting <-datatesting[, 8:160] 

#this might eliminate different columns! 
subdatatesting <- subdatatesting[ , colSums(is.na(subdatatesting)) == 0]

subdatatesting[sapply(subdatatesting, is.factor)] <- lapply(subdatatesting[sapply(subdatatesting, is.factor)], function(x) seq_along(levels(x))[x])
subdatatesting <- subset(subdatatesting, select = -c(6,9,46,49,68,71, 52,12,72,73,4,44,51,11,18,17,16,27,78,54) )

testModel <- predict(modelFit2, newdata=subdatatesting)
# (not work...it might be )
#########################################
#########################################

## try pca
#datatraining <- read.csv("data/pml-training.csv")
#subdatatraining <-datatraining[, 8:159]
#subdatatraining <- subdatatraining[ , colSums(is.na(subdatatraining)) == 0]
#subdatatraining[sapply(subdatatraining, is.factor)] <- lapply(subdatatraining[sapply(subdatatraining, is.factor)], function(x) seq_along(levels(x))[x])
#pca <- preProcess(subdatatraining, method = "pca",thresh = 0.80, pcaComp = NULL,na.remove = TRUE)
#pca
#trainPC <- predict(pca, subdatatraining)
#classe <- datatraining[, 160]
#finaltrainingdata <- cbind(subdatatraining, classe)

datatraining <- read.csv("data/pml-training.csv")

# Data partitioning for training and testing
intrain <- createDataPartition(y=datatraining$classe, p=0.7, list=FALSE)
training <- datatraining[intrain,] 
testing <- datatraining[-intrain,] 
dim(training)
dim(testing)

#data cleaning
#derived features
subdatatraining <-training[, 8:159]
subdatatraining <- subdatatraining[,-grep("^var|^avg|^stddev|^min|^max|^ampl|^skew|^kurt", colnames(subdatatraining))]

#subdatatraining <- subdatatraining[ , colSums(is.na(subdatatraining)) == 0]
#subdatatraining[sapply(subdatatraining, is.factor)] <- lapply(subdatatraining[sapply(subdatatraining, is.factor)], function(x) seq_along(levels(x))[x])

classe <- training[, 160]
finaltrainingdata <- cbind(classe, subdatatraining)
str(finaltrainingdata)

#finaltrainingdata <- finaltrainingdata[ , colSums(is.na(finaltrainingdata)) == 0]
pca <- preProcess(finaltrainingdata[, -1], method = "pca",thresh = 0.80, pcaComp = NULL,na.remove = TRUE)
pca
trainPC <- predict(pca, finaltrainingdata[, -1])
# modelFit <-train(finaltrainingdata$classe ~ ., method="glm", data=trainPC) #not converge!
modelFit <-train(finaltrainingdata$classe ~ ., method="rpart", data=trainPC)
modelFit <-train(finaltrainingdata$classe ~ ., method="rf", data=trainPC)

# TEST
subdatatesting <-testing[, 8:159]

#datatesting <- read.csv("data/pml-testing.csv")

subdatatesting <- subdatatesting[,-grep("^var|^avg|^stddev|^min|^max|^ampl|^skew|^kurt", colnames(subdatatesting))]
#subdatatesting <- subdatatesting[ , colSums(is.na(subdatatesting)) == 0]
#subdatatesting[sapply(subdatatesting, is.factor)] <- lapply(subdatatesting[sapply(subdatatesting, is.factor)], function(x) seq_along(levels(x))[x])
classe <- testing[, 160]
finaldatatesting <- cbind(classe, subdatatesting)

testPC <- predict(pca, finaldatatesting[, -1])
#testPC <- predict(modelFit, finaldatatesting[, -1]) # not work!

confusionMatrix(finaldatatesting$classe, predict(modelFit, testPC))
Confusion Matrix and Statistics

#Reference
#Prediction    A    B    C    D    E
#A       1268  342    0    0   64
#B  511  510    0    0  118
#C  682  328    0    0   16
#D  331  401    0    0  232
#E  317  403    0    0  362

#Overall Statistics

#Accuracy : 0.3636          
#95% CI : (0.3513, 0.3761)
#No Information Rate : 0.5283          
#P-Value [Acc > NIR] : 1               

#Kappa : 0.1624          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
#Sensitivity            0.4078  0.25706       NA       NA  0.45707
#Specificity            0.8537  0.83876   0.8257   0.8362  0.85863
#Pos Pred Value         0.7575  0.44776       NA       NA  0.33457
#Neg Pred Value         0.5628  0.68942       NA       NA  0.91047
#Prevalence             0.5283  0.33713   0.0000   0.0000  0.13458
#Detection Rate         0.2155  0.08666   0.0000   0.0000  0.06151
#Detection Prevalence   0.2845  0.19354   0.1743   0.1638  0.18386
#Balanced Accuracy      0.6308  0.54791       NA       NA  0.65785

############# RANDOM FOREST
modelFit <-train(finaltrainingdata$classe ~ ., method="rf", data=trainPC)
modelFit
Random Forest 

13737 samples
11 predictor
5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Bootstrapped (25 reps) 

Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 

Resampling results across tuning parameters:
        
        mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
2    0.9414959  0.9259591  0.003594893  0.004559782
7    0.9336697  0.9160634  0.003975603  0.005042955
12    0.9216179  0.9008225  0.005532700  0.007010648

Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 2. 

#testPC <- predict(modelFit, finaldatatesting[, -1]) #not working!
testPC <- predict(pca, finaldatatesting[, -1])
confusionMatrix(finaldatatesting$classe, predict(modelFit, testPC))

####################### RANDOM FOREST with cross-validation with pca
modelFit <- train(finaltrainingdata$classe ~., data = trainPC, method = "rf", prox = TRUE, trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE))
modelFit
Random Forest 

13737 samples
11 predictor
5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Cross-Validated (4 fold) 

Summary of sample sizes: 10303, 10302, 10304, 10302 

Resampling results across tuning parameters:
        
        mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
2    0.9464952  0.9323211  0.003474273  0.004402064
7    0.9402343  0.9243994  0.001725063  0.002166214
12    0.9331006  0.9153835  0.003220880  0.004060384

Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 2. 

confusionMatrix(finaldatatesting$classe, predict(modelFit, testPC))

####################### RANDOM FOREST NO pca very slowly!!
modelfirRFnopca <-train(finaltrainingdata$classe ~ ., method="rf", data=finaltrainingdata)
Random Forest 

13737 samples
52 predictor
5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Bootstrapped (25 reps) 

Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 

Resampling results across tuning parameters:
        
        mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
2    0.9880288  0.9848469  0.001796190  0.002275496
27    0.9875394  0.9842294  0.002209928  0.002788884
52    0.9785707  0.9728804  0.005124213  0.006470756

Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 2. 

modeltest <- predict(modelfirRFnopca, finaldatatesting)prediction <- predict(modelfirRFnopca, newdata = finaldatatesting)
accuracy <- confusionMatrix(prediction, finaldatatesting$classe); print(accuracy)

Confusion Matrix and Statistics

Reference
Prediction    A    B    C    D    E
A 1674    6    0    0    0
B    0 1133    8    0    0
C    0    0 1017   17    0
D    0    0    1  946    4
E    0    0    0    1 1078

Overall Statistics

Accuracy : 0.9937          
95% CI : (0.9913, 0.9956)
No Information Rate : 0.2845          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.992           
Mcnemar's Test P-Value : NA              

Statistics by Class:

Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.9947   0.9912   0.9813   0.9963
Specificity            0.9986   0.9983   0.9965   0.9990   0.9998
Pos Pred Value         0.9964   0.9930   0.9836   0.9947   0.9991
Neg Pred Value         1.0000   0.9987   0.9981   0.9964   0.9992
Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
Detection Rate         0.2845   0.1925   0.1728   0.1607   0.1832
Detection Prevalence   0.2855   0.1939   0.1757   0.1616   0.1833
Balanced Accuracy      0.9993   0.9965   0.9939   0.9902   0.9980
##################################
We could estimate the in-sample error by

```{r}
modelpredictionTrS <- predict(modelfitRFnopca, data=finaltrainingdata)
table(finaltrainingdata$classe, modelpredictionTrS)
nright = table(modelpredictionTrS == finaltrainingdata$classe)
ise = as.vector(100 * (1-nright["TRUE"] / sum(nright)))
ise

```
So the is-sample error is ? .

table(finaldatatesting$classe, modelpredictionTeS)
nright = table(modelpredictionTeS == finaldatatesting$classe)
oose = as.vector(100 * (1-nright["TRUE"] / sum(nright)))
#oose <- 1 - as.numeric(confusionMatrix(finaldatatesting$classe, modelprediction)$overall[1])
oose
