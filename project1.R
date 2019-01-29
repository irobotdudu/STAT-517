Nsim = 116
i = 1
mydata<-read.csv("~/tear_osmolarity_data.csv",header = TRUE)
attach(mydata)
mydata$newDevice.AL <-as.numeric(as.character(mydata[,2]))
mydata$newDevice.AR <-as.numeric(as.character(mydata[,3]))
mydata$newDevice.BL <-as.numeric(as.character(mydata[,4]))
mydata$newDevice.BR <-as.numeric(as.character(mydata[,5]))

mydata$newDevice.AL
mydata$newDevice.AR
mydata$newDevice.BL
mydata$newDevice.BR

j = 1
 result_A = result_B = matrix(ncol=1,nrow=Nsim)
  while(i < 116 ){
  
    while(is.na(mydata$newDevice.AL[i]) || is.na(mydata$newDevice.AR[i]) || is.na(mydata$newDevice.BL[i]) || is.na(mydata$newDevice.BR[i])){
      
      i = i + 1
    }
      if(max(mydata$newDevice.AL[i],mydata$newDevice.AR[i]) >= 308){
        result_A[j] = 1
      
      }
      if(max(mydata$newDevice.AL[i],mydata$newDevice.AR[i]) < 308){
        result_A[j] = 0
      
      }
      if(max(mydata$newDevice.BL[i],mydata$newDevice.BR[i]) >= 299){
        result_B[j] = 1
      
      }
      if(max(mydata$newDevice.BL[i],mydata$newDevice.BR[i]) < 299){
        result_B[j] = 0
     
      }
    
    #cat("index",j," = ")
    #cat("",result_A[j],"\n")
   # cat("index",i," = ")
    #cat("",result_B[i],"\n")
    i = i + 1
    j = j + 1
  }
 index = j - 1
 
 result_A
 result_B
  #parameter metrix
  p_matrix = matrix(ncol = 1,nrow = 5)
  p_matrix <- rep(0.5,5)
  #1. pi_est
  #2. pa0_est
  #3. pb0_est
  #4. pa1_est
  #5. pb1_est
  di_est <- function(p_m, A, B){
    i = 1
    di = matrix(ncol=1,nrow=index-1)
    while (i < index + 1){
      temp = (p_m[4]^A[i])*((1-p_m[4])^(1-A[i]))*(p_m[5]^(B[i]))*((1-p_m[5])^(1-B[i]))*p_m[1]
      temp_1 = (1-p_m[1])*(p_m[2]^(1-A[i]))*((1-p_m[2])^(A[i]))*(p_m[3]^(1-B[i]))*((1-p_m[3])^(B[i]))
      di[i] = temp / (temp + temp_1)
      i = 1 + i
       }
    
    
    return (di)
  }
  di_obt <- di_est(p_matrix, result_A, result_B)
  di_obt
  #EM algrithm
  
  
  
  #Expectation Step
  #pi
  p_matrix[1] = sum(di_obt)/113
  cat("estimator for pi: ",pi_est, "\n")
  # p a1 est
  pa1_estimate <- function(p_m, A){
    temp = 0
    i = 1
    while(i < index + 1){
     ini = p_m[i]*A[i]
      temp = ini + temp
   
      i = i + 1
    }
    
    return (temp/sum(di_obt))
  }
  p_matrix[4] = pa1_estimate(di_obt,result_A)
  cat("estimator for pa1:", p_matrix[4], "\n")
  
  #p a0
  pa0_estimate <- function(p_m, A){
    i = 1
    temp = 0
    temp_1 = 0
    while(i < 113 + 1){
    ini = (1-p_m[i])*A[i]
    ini_1 = 1-p_m[i]
    temp = ini + temp
    temp_1 = temp_1 + ini_1
    #cat(i,": ", temp,"\n")
    i = i + 1
    }
   return (temp/temp_1) 
  }
  p_matrix[2] = pa0_estimate(di_obt,result_A)
  cat("estimator for pa0:", p_matrix[2], "\n")
  
  # p b1
  pb1_estimate <- function(p_m, B){
  i = 1
  
  temp = 0
  while(i < 113 + 1){
    ini = p_m[i]*B[i]
    temp = ini + temp
    #cat(i,": ", temp,"\n")
    i = i + 1
    }
  return(temp / sum(p_m))
  }
 p_matrix[5] = pb1_estimate(di_obt, result_B)
  cat("estimator for pb1:", p_matrix[5], "\n")
  
  #pb0
  pb0_estimate <- function(p_m, B){
  i = 1
  temp = 0
  temp_1 = 0
  while(i < 113 + 1){
    ini = (1-p_m[i])*B[i]
    ini_1 = 1-p_m[i]
    temp = ini + temp
    temp_1 = temp_1 + ini_1
    
    i = i + 1
  }
  return (temp/temp_1)
  }
  p_matrix[3] = pb0_estimate(di_obt,result_B) 
  cat("estimator for pb0:", p_matrix[3], "\n")
  
  p_matrix
  
  #Maximization Step
  i = 1
  N = 1000
  while(i < N+1){
    di_max <- di_est(p_matrix, result_A, result_B)
    p_matrix[1] = sum(di_max)/113
    p_matrix[2] = pa0_estimate(di_max,result_A)
    p_matrix[3] = pb0_estimate(di_max,result_B)
    p_matrix[4] = pa1_estimate(di_max,result_A)
    p_matrix[5] = pb1_estimate(di_max, result_B)
    i = i + 1
  }
    p_matrix
    di_max
  
