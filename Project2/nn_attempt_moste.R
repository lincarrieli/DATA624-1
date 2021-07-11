## Nueral Network Attempt
library(plyr)
library(caret)
library(nnet)
library(ggplot2)

#### Read in Data ####
data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA624/main/Project2/StudentDataTOMODEL.csv")

summary(data)

#### Transform Data ####
data[,1] <- mapvalues(data[,1],
                      from = c("A","B","C","D",""),
                      to = c(1,2,3,4,NA))
data[,1] <- as.integer(data[,1])

# Removing the response variable since I don't want to impute or transform these values
drops <- c("PH")
features <- data[,!(names(data) %in% drops)]

na_to_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
features[] <- lapply(features, na_to_mean)

trans <- preProcess(features,
                    method = c("center", "scale"))
transformed_feat <- predict(trans, features)

nzv <- nearZeroVar(transformed_feat, saveMetrics = TRUE)
nzv[nzv[,"nzv"] == TRUE,]

# Removing Hy.Pressure1 since it has near zero variance
drops <- c("Hyd.Pressure1")
transformed_feat <- transformed_feat[,!(names(transformed_feat) %in% drops)]

tooHigh <- findCorrelation(cor(transformed_feat), cutoff = .75)
processed <- transformed_feat[, -tooHigh]

processed <- cbind(data[,26], processed)
names(processed)[1] <- ("PH")

processed <- processed[complete.cases(processed),]

set.seed(12345)
train_ind <- sample(seq_len(nrow(processed)),
                    size = floor(0.75*nrow(processed)))

train <- processed[train_ind,]
test <- processed[-train_ind,]

nnGrid <- expand.grid(.mtry = c(5:10))

nnTune <- train(train[,-1],
                train[,1],
                tuneGrid = nnGrid,
                trControl = trainControl(method="cv", number=5),
                linout = TRUE,
                trace = FALSE,
                MaxNWts = 10 * (ncol(train) + 1) + 10 + 1,
                maxit = 500)

nnetTune <- nnet(train[,-1],
                 train[,1],
                 size = 4,
                 linout = TRUE,
                 decay = 0,
                 maxit = 100,
                 MaxNWts = 10*(ncol(train)+1)+10+1)

nnetPred <- predict(nnetTune, test[,-1])
        
mape <- mean(abs(test[,1] - nnetPred)/test[,1])*100
rmse <- sqrt(mean((test[,1] - nnetPred)^2))

results <- data.frame(matrix(nrow = 0, ncol = 4))
for(d in c(0.00, 0.01, 0.10)){
  for(s in 1:15){
    print(s)
    nnetTune <- nnet(train[,-1],
                     train[,1],
                     size = s,
                     linout = TRUE,
                     decay = d,
                     maxit = 100,
                     MaxNWts = 20*(ncol(train)+1)+10+1)
    
    nnetPred <- predict(nnetTune, test[,-1])
    
    mape <- mean(abs(test[,1] - nnetPred)/test[,1])*100
    rmse <- sqrt(mean((test[,1] - nnetPred)^2))
    
    results <- rbind(results ,c(d,s,mape,rmse))
  }
}

colnames(results) <- c("d", "s", "MAPE", "RMSE")
results[,1] <- as.factor(results[,1])
results[,2] <- as.integer(results[,2])
results[,3] <- as.numeric(results[,3])
results[,4] <- as.numeric(results[,4])

ggplot(data = results, aes(x = s, y = MAPE, color = d)) +
  geom_line() +
  geom_point()
