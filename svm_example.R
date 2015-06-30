#  Usage : run_svm <- function(data : iris (working data)
#                        fold_num : k cross validation number
#                        type     : "linear" for linear kernel , "polynomial" for poly. kernel
#                        gamma=0.1:  gamma parameter for polynomial kernel (default 0.1)
#                        degree=1 : polynomial kernel degree parameter
#                        coef     : polynomial kernel parameter 
#                        roc_curve_enable : plot roc curve we must be creat a binary classifier. (Enable flag)    
#                          )
#
#  If you run example working flow, please delete comment at the begginig of line where end of file

run_svm <- function(data,fold_num,type,gamma=0.1,degree=1,coef=0, roc_curve_enable = FALSE){
  
  if(roc_curve_enable == TRUE){
    data$Species = as.numeric(data$Species == "setosa")
  }
  
  cvclass = split(data$Species,rep(1:fold_num,lentgh=nrow(data))) # k cross validation
  cvdata = split(data,rep(1:fold_num,lentgh=nrow(data)))          # k cross validation for class
  
  allResult_confusion_matris = nrow(data)   # total confusion matris (sum of all fold result)
  
  for(i in 1:fold_num){
    temp_data = matrix()
    temp_class = matrix()
    for(j in 1:fold_num){
      if(j != i){
        if((i == 1 && j == 2) ||(i != 1 && j == 1)){
          temp_class = cvclass[[j]]
          temp_data = cvdata[[j]]
        }else{  
          temp_data  = rbind(temp_data,cvdata[[j]])      #concatenated training partition
          temp_class = rbind(temp_class,cvclass[[j]])
        }
      }
    }
    
   
    
    library(e1071)
    if(type == "linear")
      model <- svm( temp_data$Species~. , temp_data , kernel ="linear")
    if(type == "polynomial"){
      model <- svm( temp_data$Species~. , temp_data , kernel ="polynomial", gamma = gamma, degree = degree,coef0 = coef)
    }
    res <- predict( model, newdata=cvdata[[i]]) 
  
    if(i == 1){
      allResult_confusion_matris =res
      allcvclass = cvclass[[i]]
    }else{
      allResult_confusion_matris = rbind(allResult_confusion_matris,res)
      allcvclass = rbind( allcvclass, cvclass[[i]])
    }
    
    if(roc_curve_enable ==  TRUE){
        library(ROCR)  
        print(paste(toString(i) , " . fold :" , sep = " "))
       
        pred1 <- prediction(res, cvclass[[i]])
        perf1 <- performance(pred1,"tpr","fpr")
        plot(perf1)
    }else{
        print(paste(toString(i) , " . fold :" , sep = " "))
        print(table(cvclass[[i]],res))
    }  
  }
  
  
  log =  names(sort(summary(as.factor(data$Species), decreasing=T)))  
  for(index in 1:length(log)){
    allResult_confusion_matris[allResult_confusion_matris == index] = log[index]
  }
  if(roc_curve_enable == FALSE){
    print("[Result] : ")
    print(table(data$Species,allResult_confusion_matris))
  }
#   else{
#     pred1 <- prediction(allResult_confusion_matris, allcvclass)
#     perf1 <- performance(pred1,"tpr","fpr")
#     plot(perf1)
#   }
}

# Sample usage for run_svm function
#run_svm(iris,3,"linear")
#run_svm(iris,3,"polynomial",0.1,1)









