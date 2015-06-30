#  Usage : knn(data : training data (doesn't contain class attribute)
#              classData : iris$Species (class attribute)
#              k    : k nearest number  
#              fold_num : k fold cross validation num
#              distance_type: "e" or "m" (euclidean distance or manhattan distance)

# If you run example working flow, please delete comment at the begginig of line where end of file 

knn <- function(data,classData,k,fold_num,distance_type){
  
  cvclass = split(classData,rep(1:fold_num,lentgh=nrow(classData)))  # k fold cross validation 
  cvdata = split(data,rep(1:fold_num,lentgh=nrow(data)))
  
  allResult_confusion_matris = nrow(classData)
  
  for(i in 1:fold_num){
    temp_data = matrix()
    temp_class = matrix()
    for(j in 1:fold_num){
      if(j != i){
        if((i == 1 && j == 2) ||(i != 1 && j == 1)){
          temp_class = cvclass[[j]]
          temp_data = cvdata[[j]]
        }else{  
          temp_data  = rbind(temp_data,cvdata[[j]])  # k-1 piece concataneted
          temp_class = cbind(temp_class,cvclass[[j]])
        }
      }
    }
    
    # finding k nearest number point's class 
    resultList = knn_helper(temp_data,temp_class,cvdata[[i]],k,distance_type)
   
    
    # finding class attributes names
    log =  names(sort(summary(as.factor(cvclass[[fold_num]]), decreasing=T)))  
    for(index in 1:length(log)){
       resultList[resultList == index] = log[index]
    }
    if(i == 1){
      allResult_confusion_matris =resultList
    }else{
      allResult_confusion_matris = rbind(allResult_confusion_matris,resultList)
    }
  
    # confusion matrix
     print(paste(toString(i) , " . fold :" , sep = " "))
     print(table(cvclass[[i]],resultList))

  }
  
  # concatenated all result about confusion matris
  print("[Result] : ")
  print(table(classData,allResult_confusion_matris))
    
}


knn_helper <- function(trainingSet,classSet,testSet,k,distance_type){
  
  
  resultMatrix =  matrix(-1,nrow(trainingSet),nrow(testSet))  # nxn matris for keep distance point each points 
  returnVector  = vector(mode = "numeric" , length = nrow(testSet)) 
  
  if (ncol(trainingSet) != ncol(testSet))
    return -1
  
  #print(trainingSet)
  #print(testSet)
  
  #calculate distance 
  for(i in 1:nrow(trainingSet)){
    for(j in 1:nrow(testSet)){   
      if(distance_type == "m") #distance type
        resultMatrix[i,j] = dist(rbind(trainingSet[i,],testSet[j,]), method = "manhattan")      
      if(distance_type == "e")
        resultMatrix[i,j] = dist(rbind(trainingSet[i,],testSet[j,]), method = "euclidean")
    }
  }
  
  #View(resultMatrix)

  #make  prediction about class from distance for each test point   
  returnList = nrow(testSet)
  for(i in 1:nrow(testSet)){
    returnValue =  minimum_k(k,resultMatrix[,i])

    temp_vector = numeric(length(returnValue))
    for(j in 1:length(returnValue)){
      temp_vector[j] =  classSet[returnValue[j]]
    }
    # sorting as a most voiting class
    log =  names(sort(summary(as.factor(temp_vector), decreasing=T)[1:k]))  
    returnList[i] = log[1] # choosing best choice
    
  }
  return(returnList)
}

#find closest k point from distance  vector
minimum_k = function(k,vector){
  
  vec = numeric(k)
  for(i in 1:length(vec)){
    vec[i] = i
  }
  
  for(i in 1:length(vector)){ 
     for(j in 1:length(vec)){
        if(j == 1){
          biggiestNum = j
        }else{
          if(vector[vec[biggiestNum]] < vector[vec[j]]){
            biggiestNum = j
          }
        }
      }
     
     if(vector[vec[biggiestNum]] >= vector[i]){
       if(!is.element(i,vec))
          vec[biggiestNum] = i
     }
  }
  
  return(vec);
}


class = iris[,5]
data  = iris[,1:4]
#knn(data,class,3,2,"e")
#knn(data,class,3,2,"m")

