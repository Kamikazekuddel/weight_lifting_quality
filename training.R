require(tidyverse)
require(lubridate)
require(caret)

training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')

cs <- colSums(is.na(testing))
features <- names(cs[cs != 20])
exlist <- c('X','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp','new_window','num_window','problem_id')
features <- features[!features %in% exlist]

training <- training[c(features,'classe')]
testing <- testing[features]

# load the iris dataset
#data(iris)
# define training control
#train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
#grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
#model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# summarize results
#print(model)



{
  ptm <- proc.time()
  folds <- 3
  cvIndex <- createFolds(factor(paste(as.character(training$classe),as.character(training$user_name))),
                         folds, returnTrain = TRUE)
  #tc <- trainControl(index = cvIndex,
  #                   method = 'cv', 
  #                   number = folds) 
  grid <- expand.grid(mtry=c(10,20,30,40))
  tc <- trainControl(method="cv",number = 3)
  mdl <- train(classe~.,data = training,method='rf',trControl = tc,maxnodes=512,ntree=50,tuneGrid = grid)
  print(mdl)
#  for (f in cvIndex) {
#    Xt <- training[f,features]
#    yt <- training[f,'classe']
#    Xv <- training[-f,features]
#    yv <- training[-f,'classe']
#    mdl <- train(Xt,yt,method='rf',maxnodes=32,ntree=30)
#    pred <- predict(mdl,Xv)
#    print('foo')
#    print(mean(pred == yv))
#    #print(mdl)
#  }
  print(proc.time() -ptm)
}

idx <- createDataPartition(training$classe,p=0.7,list=FALSE)

X_train <- training[idx,features]
y_train <- training[idx,'classe']

X_val <- training[-idx,features]
y_val <- training[-idx,'classe']

#gbm 0.9641
#naive_bayes 0.737 36s
#'svmRadialWeights'
#rf 0.9932031 3524.21s defaults
#rf 0.7418862 248s ntree = 50 maxnodes = 32
#rf 0.8538658 282s ntree 50 maxnodes = 64
#rf 0.9284622 208s ntree 30 maxnodes 128
#rf 0.9928632 383 ntree = 50 maxnodes = 512 perfect accuracy on test set


{
ptm <- proc.time()
mdl<-train(X_train,y_train,method='rf',preProcess = c('center','scale'),ntree=20,maxnodes = 16)
print(mean(predict(mdl,X_val) == y_val))
print(proc.time() -ptm)
}

model_predictions <- predict(mdl,testing)
