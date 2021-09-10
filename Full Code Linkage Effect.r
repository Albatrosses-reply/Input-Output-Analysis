#install.packages("sweep")
library(sweep)

# Load data

iotable2014<-get(load("WIOT2014_October16_ROW.RData"))
iotable2013<-get(load("WIOT2013_October16_ROW.RData"))
iotable2012<-get(load("WIOT2012_October16_ROW.RData"))
iotable2011<-get(load("WIOT2011_October16_ROW.RData"))
iotable2010<-get(load("WIOT2010_October16_ROW.RData"))
iotable2009<-get(load("WIOT2009_October16_ROW.RData"))
iotable2008<-get(load("WIOT2008_October16_ROW.RData"))
iotable2007<-get(load("WIOT2007_October16_ROW.RData"))
iotable2006<-get(load("WIOT2006_October16_ROW.RData"))
iotable2005<-get(load("WIOT2005_October16_ROW.RData"))
iotable2004<-get(load("WIOT2004_October16_ROW.RData"))
iotable2003<-get(load("WIOT2003_October16_ROW.RData"))
iotable2002<-get(load("WIOT2002_October16_ROW.RData"))
iotable2001<-get(load("WIOT2001_October16_ROW.RData"))
iotable2000<-get(load("WIOT2000_October16_ROW.RData"))

dim(iotable2000)

unique(iotable2000$IndustryDescription)

unique(iotable2000$IndustryCode)

#####################
#COUNTRY <- "AUS"
#YEAR <- 2014
#####################

# Making empty data.frame with 5 variables

result_LE <- data.frame(Country=NA, Year=NA, R_num=NA, BL=NA, FL=NA)
result_LE <- result_LE[-1,]

COUNTRY<-c("KOR")
RESULT<-Linkage_effects
YEAR<-c("2000")
DS<-iotable2000



##############################################
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

###################
result_LE <- data.frame(Country=NA, Year=NA, R_num=NA, BL=NA, FL=NA)
Country_list
result_LE <- result_LE[-1,]

year <- 2000 ; ds <- iotable2000
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2001 ; ds <- iotable2001
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2002 ; ds <- iotable2002
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2003 ; ds <- iotable2003
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2004 ; ds <- iotable2004
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2005 ; ds <- iotable2005
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2006 ; ds <- iotable2006
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2007 ; ds <- iotable2007
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2008 ; ds <- iotable2008
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2009 ; ds <- iotable2009
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2010 ; ds <- iotable2010
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2011 ; ds <- iotable2011
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2012 ; ds <- iotable2012
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2013 ; ds <- iotable2013
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}
year <- 2014 ; ds <- iotable2014
for(j in 1:44){
  result_LE <- Making_linkage_effects(COUNTRY = Country_list[j],
                                            RESULT = result_LE,
                                            YEAR = year, DS = ds )
}

write.csv(result_LE,"result_VA222_190709")

###################
####################
############

Making_VA_rate <- function(COUNTRY, RESULT, YEAR, DS) {
  
  COUNTRY1_56 <- paste0(COUNTRY,c(1:56))
  IO_Matrix <- as.matrix(DS[DS$Country==COUNTRY,COUNTRY1_56])
  
  GO <- as.matrix(DS[DS$IndustryCode=="GO",COUNTRY1_56])
  GO_2 <- t(c(sum(GO[c(12,17,47)]),
              sum(GO[c(11,18,19,20,21,37,40)]),
              sum(GO[c(13,14,15,22,23)]),
              sum(GO[c(4,5,6,7,8,9,10,16,39,45,46,48,49)]),
              sum(GO[c(1,2,3,24,25,26,27,28,29,30,31,32,33,34,35,
                       36,38,41,42,43,44,50,54,55,56)]),
              sum(GO[c(51,52,53)])))
  
  VA <- as.matrix(DS[DS$IndustryCode=="VA",COUNTRY1_56])
  VA_2 <- t(c(sum(VA[c(12,17,47)]),
              sum(VA[c(11,18,19,20,21,37,40)]),
              sum(VA[c(13,14,15,22,23)]),
              sum(VA[c(4,5,6,7,8,9,10,16,39,45,46,48,49)]),
              sum(VA[c(1,2,3,24,25,26,27,28,29,30,31,32,33,34,35,
                       36,38,41,42,43,44,50,54,55,56)]),
              sum(VA[c(51,52,53)])))
  
  VA_R <- sweep(VA_2,MARGIN = 2,GO_2,'/')
  
  ##BL VA##
  
  Country <- matrix(COUNTRY,nrow = 6, ncol = 1)
  rnd_devide <- matrix(c("high","medium-high","medium","medium-low","low","etc"))
  Year <- matrix(YEAR,nrow = 6, ncol = 1)
  
  VA_rate <- data.frame(cbind(Country,Year,rnd_devide,t(VA_R)))
  colnames(VA_rate) <- c("Country","Year","rnd_devide","VA_rate")
  #write.table(LE,paste0(COUNTRY,"_LE"))
  RESULT <- rbind(RESULT,VA_rate)
  return(RESULT)
}

result_VA_rate <- data.frame(Country=NA, Year=NA, rnd_devide=NA, VA_rate=NA)
result_VA_rate <- result_VA_rate[-1,]


##

Country_list <- unique(iotable2000$Country)

year <- 2000 ; ds <- iotable2000
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2001 ; ds <- iotable2001
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2002 ; ds <- iotable2002
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2003 ; ds <- iotable2003
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2004 ; ds <- iotable2004
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2005 ; ds <- iotable2005
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2006 ; ds <- iotable2006
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2007 ; ds <- iotable2007
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2008 ; ds <- iotable2008
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2009 ; ds <- iotable2009
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2010 ; ds <- iotable2010
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2011 ; ds <- iotable2011
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2012 ; ds <- iotable2012
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2013 ; ds <- iotable2013
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}
year <- 2014 ; ds <- iotable2014
for(j in 1:44){
  result_VA_rate <- Making_VA_rate(COUNTRY = Country_list[j],
                                      RESULT = result_VA_rate,
                                      YEAR = year, DS = ds )
}

write.csv(result_VA_rate,"result_rnd_VA_rate_190709")
