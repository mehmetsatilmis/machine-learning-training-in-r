 
# eigenvalue analizini yaparak kaç vektör seçileceğine bakılan fonksiyon

eigen_value_analysis <- function(local_data){
  library(FactoMineR)
  # apply PCA
  pca3 = PCA(local_data, graph = FALSE)
  # matrix with eigenvalues
  eig = pca3$eig
  return(eig)
} 

# split function for create training data by given value(percent)
split_data_percentage_train = function(data , value){
  new_data  = split( data , data$V43 )
  index1 = as.integer((nrow(new_data[[1]])*value/100)+0.5)  
  index2 = as.integer((nrow(new_data[[2]])*value/100)+0.5)  
  data1 = new_data[[1]]
  data2 = new_data[[2]]
  
  traindata1 = data1[1:index1,]
  traindata2 = data2[1:index2,]
  traindata = rbind(traindata1,traindata2)
  return( traindata )  
}

#split function for create testing data by given value(percent)
split_data_percentage_test = function(data , value){
  new_data  = split( data , data$V43 )
  index1 = as.integer((nrow(new_data[[1]])*value/100)+0.5)  
  index2 = as.integer((nrow(new_data[[2]])*value/100)+0.5)  
  data1 = new_data[[1]]
  data2 = new_data[[2]]
  
  testdata1 = data1[(index1+1):nrow(data1),]
  testdata2 = data2[(index2+1):nrow(data2),]
  testdata = rbind(testdata1,testdata2)
  return(testdata)  
}

# incoming testing data moved from simple to pca space (center and rotation working)
transform_pca_space <- function(input_data,pca,rotation_size){
  
  resultMatrix =  matrix(-1,nrow(input_data),rotation_size) 
    
  for(i in 1:rotation_size){
    coef = pca$rotation[,i]
    for(k in 1:nrow(input_data)){
      temp = 0
      for(j in 1:length(coef)){        
        x = input_data[k,j] - pca$center[j]
        x = x * coef[j]
        temp = temp + x
      }
      resultMatrix[k,i] = temp 
    }
    
  }
  
  return(resultMatrix)
}

create_Confusion_Matris <- function(testClass,mainClass){

  tbl = table(mainClass,testClass)
  
  print("Correctly Classified Instances : ")
  print(tbl[1,1] + tbl[2,2])
  print("Incorrectly Classified Instances : ")
  print(tbl[1,2]+tbl[2,1])
  print('Percentage % : ')
  print((tbl[1,1] + tbl[2,2])*100/(length(testClass)))
  
}

#Recursive Partitioning and Regression Trees , try to find bias on our sample data 
try_rpart <- function(data,class,test,testClass){
  library("rpart")
  fit <- rpart(V11 ~ V10 + V9 + V8 + V7 + V6 + V5 + V4 + V3 + V2 + V1, data=data, method="class")
  pred <- predict(fit, test, type = "class")
  print("**********************");
  print("| Regression Trees : |")
  create_Confusion_Matris(pred,testClass)
  return(pred)
}

run_rpart <- function(data,class,test,rdf){
  library("rpart")
  fit <- rpart(V11 ~ V10 + V9 + V8 + V7 + V6 + V5 + V4 + V3 + V2 + V1, data=data, method="class")
  saveRDS(fit,rdf)
  pred <- predict(fit, test, type = "class")
  print(pred)
  return(pred)
}

try_knn <- function(data,class,test,testClass){
  library(class)
  model = knn(train = data, test = test, class, k = 3)
  print("********************");
  print("| KNN | : ")
  create_Confusion_Matris(model,testClass)
  return(model)
}

try_svm <- function(data,V11,test,testClass){
  library(e1071)
  model  = svm(data,V11)
  pred <- predict(model, test)
  print("*******************");
  print('|SVM| :')
  create_Confusion_Matris(pred,testClass)
  return(pred)
}

run_svm <- function(data,class,test,rdf){
  library(e1071)
  model  = svm(data,class)
  saveRDS(model,rdf)
  
  pred <- predict(model, test)
  #create_Confusion_Matris(pred,V11)
  print(pred)
  return(pred)
}

# working on external testing data 
runMain <- function(path,testSet_path,percent=67,svmRdf = "svm_model.rdf", outputPath = "~/Desktop/classifier_output.txt" ,s_svmRdf = "second_svm.rdf", rpartRdf = "rpart.rdf"){
  hw2train <- read.csv(path, header=FALSE)
  testSet <- read.csv(testSet_path,header=FALSE)
  hw2train[,43] = factor(hw2train[,43])
  pca = prcomp(hw2train[,1:42], scale. = FALSE)
  #list[traindata,testdata] =  split_data_percentage(hw2train,60)

  if( is.na(testSet[1,2])){
    new_testSet = testSet[2:nrow(testSet),]
  }else{
    new_testSet = testSet
  }
  
  traindata =  split_data_percentage_train(hw2train,percent)
  testSet = transform_pca_space(new_testSet[,1:42],pca,10)
  trainset = transform_pca_space(traindata,pca,10)
  
  testSet = as.data.frame(testSet)
  trainset = as.data.frame(cbind(trainset,traindata$V43))
  trainset$V11 = factor(trainset$V11)
# iris_ctree <- ctree(V11 ~ V10 + V9 + V8 + V7 + V6 + V5 + V4 + V3 + V2 + V1 , data=trainset)
#  plot(iris_ctree) 
#  View(trainset)
#  model =  glm(formula = V10 ~ V1 + V2 + V3, family = binomial, data=trainset)
#  summary(model)
  
  pred_1 =  run_svm(trainset[,1:10],trainset$V11,testSet,svmRdf)
  pred_2 = run_rpart(trainset,trainset[,11],testSet,rpartRdf)
  pred = numeric(length(pred_1))

  model  = svm(pca$x[,1:10],factor(hw2train$V43))
  saveRDS(model,s_svmRdf)


  for(i in 1:length(pred_1)){
    if(pred_1[i] != pred_2[i]){      
      #model = knn(train = trainSet[,1:10], test = testSet[i,], trainClass, k = 3)
      result <- predict(model, testSet[i,])    
      pred[i] = result
     }else{
      pred[i]=pred_1[i]
     }
  }
  
  testSet=as.data.frame(cbind(testSet, pred))
  write.table(testSet, file = outputPath , sep = ",", col.names = NA,
            qmethod = "double") 

}

# working on our sample data
runMain_test <- function(path,percent){
 
  hw2train <- read.csv(path, header=FALSE)  
  hw2train[,43] = factor(hw2train[,43],label="")
  pca = prcomp(hw2train[,1:42], scale. = FALSE)
  
  traindata =  split_data_percentage_train(hw2train,percent)
  testdata = split_data_percentage_test(hw2train,percent)
  
  trainSet = transform_pca_space(traindata[,1:42],pca,10)
  testSet = transform_pca_space(testdata[,1:42],pca,10)
  
  trainSet = as.data.frame(trainSet)
  trainClass =  factor(traindata[,43])
  testSet = as.data.frame(testSet)
  testClass = factor(testdata[,43])
 
  library(e1071)
  model  = svm(pca$x[,1:10],factor(hw2train$V43))
  
  pred_1 =  try_svm(trainSet,trainClass,testSet,testClass)
  pred_2 =  try_knn(trainSet,trainClass,testSet,testClass)
  trainSet = cbind(trainSet,V11=trainClass)
  trainSet = as.data.frame(trainSet)
  pred_3 =  try_rpart(trainSet,trainClass,testSet,testClass)
  
  pred = numeric(length(pred_1))
  
  
  for(i in 1:length(pred_1)){
    if(pred_1[i] != pred_2[i]){      
      #model = knn(train = trainSet[,1:10], test = testSet[i,], trainClass, k = 3)
      result <- predict(model, testSet[i,])    
      pred[i] = result
    }else{
      pred[i]=pred_1[i]
    }
  }
  print("******************")
  print("|Total| :")
  create_Confusion_Matris(pred,testClass)
}

runWithRdsFiles <- function(svmRdf = "svm_model.rdf", outputPath = "~/Desktop/classifier_output.txt" ,s_svmRdf = "second_svm.rdf", rpartRdf = "rpart.rdf"){
  
   # burada modeller doğrudan okunarak işlem yapılacak.
}

#result = eigen_value_analysis(iris[,1:4])
#show(result)










