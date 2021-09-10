### insert library
library(sweep)

### 1. Making Linkage effects function
Making_linkage_effects <- function(COUNTRY, RESULT, YEAR, DS) {

  COUNTRY1_56 <- paste0(COUNTRY,c(1:56))
  IO_Matrix <- as.matrix(DS[DS$Country==COUNTRY,COUNTRY1_56])
  GO <- as.matrix(DS[DS$IndustryCode=="GO",COUNTRY1_56])
  VA <- as.matrix(DS[DS$IndustryCode=="VA",COUNTRY1_56])
  VA[is.nan(VA)]<-0
  VA_R <- sweep(VA,MARGIN = 2,GO,'/')
  VA_R[is.nan(VA_R)]<-0
  VA_R_I <- diag(VA_R[1:56])
  
  A.alternative <- sweep(IO_Matrix,MARGIN = 2,GO,'/')
  A.alternative[is.nan(A.alternative)]<-0
  
  IminusA <- diag(56)-A.alternative
  
  L <- solve(IminusA)
  
  Ind_Sum <- sum(L)
  Ind_Aver <- Ind_Sum/56
  
  ##backward linkage##
  
  L_sum_col<-c()
  
  for (i in 1:56){
    L_sum_col<-cbind(L_sum_col,(matrix(sum(L[,i]))))
  }
  
  BL <- sweep(L_sum_col,2,matrix(Ind_Aver,ncol=56,nrow=1),'/')
  
  ##forward linkage##
  
  L_sum_row<-c()
  
  for (i in 1:56){
    L_sum_row<-rbind(L_sum_row,(matrix(sum(L[i,]))))
  }
  
  FL <- sweep(L_sum_row,1,matrix(Ind_Aver,ncol=1,nrow=56),'/')
  

  
  ##BL VA##
  
  VA_M <- VA_R_I%*%L
  ##
  VA_Ind_Sum <- sum(VA_M)
  VA_Ind_Aver <- VA_Ind_Sum/56
  ##
  
  VA_sum_col<-c()
  
  for (i in 1:56){
    VA_sum_col<-cbind(VA_sum_col,(matrix(sum(VA_M[,i]))))
  }
  
  BL_VA <- sweep(VA_sum_col,2,matrix(VA_Ind_Aver,ncol=56,nrow=1),'/')
  
  ##FL VA##
  
  VA_sum_row<-c()
  
  for (i in 1:56){
    VA_sum_row<-rbind(VA_sum_row,(matrix(sum(VA_M[i,]))))
  }
  
  FL_VA <- sweep(VA_sum_row,1,matrix(VA_Ind_Aver,ncol=1,nrow=56),'/')
  
  ##
  
  Country <- matrix(COUNTRY,nrow = 56, ncol = 1)
  R1_56 <- matrix(paste0("R",c(1:56)),nrow = 56, ncol = 1)
  Year <- matrix(YEAR,nrow = 56, ncol = 1)
  
  LE <- data.frame(cbind(Country,Year,R1_56,t(BL),FL),t(BL_VA),FL_VA)
  colnames(LE) <- c("Country","Year","BL","FL","BL_VA","FL_VA")
  #write.table(LE,paste0(COUNTRY,"_LE"))
  RESULT <- rbind(RESULT,LE)
  return(RESULT)
}
