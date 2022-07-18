## Set of packages and functions required.
## use source() to load them as required

#Packages
library(dplyr)
library(rlang)
library(tidyverse)
library(gridExtra) #for grid.arrange()
library(toolmaRk)
library(dtw)
library(proxy)
library(dtwclust)
library(TSclust)
library(ggdendro)
library(dendextend)
library(warbleR) #pour some correlations
library(correlation)
library(writexl) #export as excel file
library(fitdistrplus)#fit distribution 
library(logspline)
library(gtools)#compute permutation table
library(plyr) 
library(writexl)
library(fitdistrplus)
library(logspline)
library(manipulate)
library(multipanelfigure)
library(philentropy)
library(TSdist)
library(caret)
library(MASS)
library(car)
library(randomForest)
library(ggcorrplot)
library(ROCR)
library(caretEnsemble)
library(MLmetrics)
library(mlbench)
library(Hmisc)
library(data.table)
library(AppliedPredictiveModeling)
#library(conflicted)

#The function below will read all .txt files 
#in a folder and prepare a dataframe with the relevant information.
ImportToolmarksData <- function(PathToDataFolder){
  
  #We get the list of files in the folder set by PathToDataFolder
  files <- list.files(path = PathToDataFolder,
                      pattern = list.files(pattern = "\\.txt$"))
  
  #Define dataframe of results
  Res.df <- data.frame(File=1:length(files),
                       Tool=1:length(files),
                       Zone=1:length(files),
                       Side=1:length(files),
                       Cut=1:length(files),
                       ROI=1:length(files))
  
  # We go through the entire list of results files
  # We read the results from the file and add it to Res.df as a list
  
  for(i in 1:length(files)){
    ReadResultsFile <- read_delim(paste0(PathToDataFolder,files[i]), 
                                  "\t", 
                                  escape_double = FALSE, 
                                  col_names = c("x","y"), 
                                  trim_ws = TRUE)
    #Assign the x and y  
    Res.df$x[i] <- list(ReadResultsFile$x)
    Res.df$y[i] <- list(ReadResultsFile$y)
    #Assign the other variables
    Res.df$File[i] <- files[i]
    Res.df$Tool[i] <- substr(files[i], 1, 2)
    Res.df$Zone[i] <- substr(files[i], 4, 4)
    Res.df$Side[i] <- substr(files[i], 5, 6)
    Res.df$Cut[i] <- substr(files[i], 8, 9)
    Res.df$ROI[i] <- substr(files[i], 11, 11)
    
    #Print to check when running
   # print(i)
    #print(files[i])
  }
  
  #Adjust variables to factors
  Res.df$Tool <- as.factor(Res.df$Tool)
  Res.df$Zone <- as.factor(Res.df$Zone)
  Res.df$Side <- as.factor(Res.df$Side)
  Res.df$Cut <- as.factor(Res.df$Cut)
  Res.df$ROI <- as.factor(Res.df$ROI)
  
  #Output
  return(Res.df)
}



ImportToolmarksDataLS <- function(PathToDataFolder){
  
  #We get the list of files in the folder set by PathToDataFolder
  files <- list.files(path = PathToDataFolder,
                      pattern = list.files(pattern = "\\.txt$"))
  
  #Define dataframe of results
  Res.df <- data.frame(File=1:length(files),
                       Tool=1:length(files),
                       Operator=1:length(files),
                       Angle=1:length(files),
                       Side=1:length(files),
                       Cut=1:length(files)
                       )
  
  # We go through the entire list of results files
  # We read the results from the file and add it to Res.df as a list
  
  for(i in 1:length(files)){
    ReadResultsFile <- read_delim(paste0(PathToDataFolder,files[i]), 
                                  "\t", 
                                  escape_double = FALSE, 
                                  col_names = c("x","y"), 
                                  trim_ws = TRUE)
    #Assign the x and y  
    Res.df$x[i] <- list(ReadResultsFile$x)
    Res.df$y[i] <- list(ReadResultsFile$y)
    #Assign the other variables
    Res.df$File[i] <- files[i]
    Res.df$Tool[i] <- substr(files[i], 1, 2)
    Res.df$Operator[i] <- substr(files[i], 3, 3)
    Res.df$Angle[i] <- substr(files[i], 4, 6)
    Res.df$Side[i] <- substr(files[i], 10, 11)
    Res.df$Cut[i] <- substr(files[i], 8, 9)
    
    
    #Print to check when running
    # print(i)
    #print(files[i])
  }
  
  #Adjust variables to factors
  Res.df$Tool <- as.factor(Res.df$Tool)
  Res.df$Side <- as.factor(Res.df$Side)
  Res.df$Operator <- as.factor(Res.df$Operator)
  Res.df$Cut <- as.factor(Res.df$Cut)
  Res.df$Angle <- as.factor(Res.df$Angle)
  
  #Output
  return(Res.df)}
ImportToolmarksDataDaan <- function(PathToDataFolder){
  
  #We get the list of files in the folder set by PathToDataFolder
  files <- list.files(path = PathToDataFolder,
                      pattern = list.files(pattern = "\\.txt$"))
  
  #Define dataframe of results
  Res.df <- data.frame(File=1:length(files),
                       Tool=1:length(files),
                       Shape=1:length(files),
                       Material=1:length(files),
                       Side=1:length(files),
                       Zone=1:length(files),
                       Cut=1:length(files)
  )
  
  # We go through the entire list of results files
  # We read the results from the file and add it to Res.df as a list
  
  for(i in 1:length(files)){
    ReadResultsFile <- read_delim(paste0(PathToDataFolder,files[i]), 
                                  "\t", 
                                  escape_double = FALSE, 
                                  col_names = c("x","y"), 
                                  trim_ws = TRUE)
    #Assign the x and y  
    Res.df$x[i] <- list(ReadResultsFile$x)
    Res.df$y[i] <- list(ReadResultsFile$y)
    #Assign the other variables
    Res.df$File[i] <- files[i]
    Res.df$Tool[i] <- substr(files[i], 1, 2)
    Res.df$Shape[i] <- substr(files[i], 3,3)
    Res.df$Material[i] <- substr(files[i], 4, 5)
    Res.df$Side[i] <- substr(files[i], 6, 7)
    Res.df$Zone[i] <- substr(files[i], 8, 9)
    Res.df$Cut[i] <- substr(files[i], 11, 12)
    
    
    #Print to check when running
    # print(i)
    #print(files[i])
  }
  
  #Adjust variables to factors
  Res.df$Tool <- as.factor(Res.df$Tool)
  Res.df$Shape <- as.factor(Res.df$Shape)
  Res.df$Material <- as.factor(Res.df$Material)
  Res.df$Side <- as.factor(Res.df$Side)
  Res.df$Zone <- as.factor(Res.df$Zone)
  Res.df$Cut <- as.factor(Res.df$Cut)
  
  #Output
  return(Res.df)}

#To extract a given profile (by its index in the selection df)
Extract_One_profile <- function(selection, index){
  a <- as.data.frame(cbind(x=unlist(selection[index,]$x),
                           y= unlist(selection[index,]$y)))
  comment(a) <- selection[index,]$File
  return(a)
}

#To plot profile using ggplot
Plot_profile <- function(Profile){
  p1 <- ggplot(Profile, aes(x , y)) + 
    geom_line(colour="blue") +
    ggtitle(comment(Profile))
  p1
}
   
#To compute CCF and extract the max score and the corresponding lag
CCF <- function(a,b, ratio=1){
  cv <- ccf(a,b, lag.max = round(length(a)/ratio,0), plot= FALSE)
  sco <- cv$acf
  lag <- cv$lag
  res <-  data.frame(sco,lag)
  lag_max <-  res[which.max(res$sco),]$lag
  sco_max <- max(sco)
  output <- c(lag_max,sco_max)
  NAs <-  rep(NA,abs(lag_max))
  #creating lagged profiles by adding NA values to plot them
    if (lag_max<0) {
      A_new <- append(NAs,a)
      B_new <- append(b,NAs)
      
    }
    else if (lag_max>0) {
      A_new <- append(a,NAs)
      B_new <- append(NAs,b)
    
    }
    else { #if the lag is 0
      A_new <- a
      B_new <- b }
  #changing into dataframe in order to plot
  a_df <- as.data.frame(cbind(c(1:length(A_new)),A_new))
  b_df <- as.data.frame(cbind(c(1:length(B_new)),B_new))
  #plotting
  P_align <- ggplot() + 
    geom_line(data=a_df,aes(x=V1,y=A_new), colour="red") + #premier profil (la base)
    geom_line(data=b_df,aes(x=V1,y=B_new), colour="blue")+ #profil en mouvement
    ggtitle("Profil a vs Profil b")
  
  #return(P_align)
  return(list(P_align,output))
}

#To compute CCF scores in batches creating a dataframe of all scores and lags
CCF_all <- function(Selection){
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table)
  allplots <- vector(mode="list", length=nProfile)
  res <-  data.frame(matrix(NA,nrow=nProfile,ncol=3))
  for(i in 1:nProfile) {
    a <- unlist(Selection$y[table[i,1]])
    b <- unlist(Selection$y[table[i,2]])
    nameComp <- paste(Selection$File[table[i,1]],"vs.",Selection$File[table[i,2]])
   
    ccf <- CCF(a, b)[[2]] #only takes element number two which is a dataframe of lag and corr values
    res[i,1] <- nameComp
    res[i,2:3] <- ccf
    
    #extracting the plot and changing the title indicating profiles compared
    allplots[[i]] <- CCF(a,b)[[1]]
    allplots[[i]]$labels[1] <- paste(substr(Selection$File[table[i,1]],1,11),
                                     "vs",substr(Selection$File[table[i,2]],1,11), 
                                     "   Lag = ", res[i,2],"     score = ",
                                     round((res[i,3]),3))
    allplots[[i]]$labels[2] <- ""
    allplots[[i]]$labels[3] <- "depth"
    
    }
  colnames(res) <- c("nameComp", "lag", "corr")
  return(list(allplots,res)) #returns a list of two elements, the first one has all the plots, sencond one is score dataframe
}

#To align profiles and compute other metrics using aligned profiles
Aligned_scores <- function(Selection){
  res <- CCF_all(Selection)[[2]]
  allplots <- CCF_all(Selection)[[1]]
  res$ndtw <- NA
  res$rdist <- NA
  res$CCF2 <- NA
  #res$p_value <- NA
  #res$CCorr <- NA
  
  #res$Uscore <- NA
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table) 
  
  for(i in 1:nProfile){
    a <- unlist(Selection$y[table[i,1]])
    b <- unlist(Selection$y[table[i,2]])
    Lag <- res$lag[i]  #ici ce sera test 
    if (Lag<0) {
      a2 <- head(a,-abs(Lag))
      b2 <- tail(b,-abs(Lag))
    }
    else if (Lag>0) {
      a2 <- tail(a,-Lag)
      b2 <- head(b,-Lag)
    }
    else { #if the lag is 0
      a2 <- a
      b2 <- b
    }
   # Edist <- dist()
    
    CCF2 <- max(ccf(a2,b2,plot = FALSE)$acf)
    CCor <- CCorDistance(a,b,lag.max=100)
    NDTW <- ndtw(a2,b2) #get ndtw score into DF
    Rdist <- reldist(a2,b2) #get relativedistance frome bachrach in DF
    res[i,4] <- NDTW
    res[i,5] <- Rdist
    res[i,6] <- CCF2
    #res[i,7] <- CCor
    
    am <- as.matrix(a2)
    bm <- as.matrix(b2)
    #cnr <- chumbley_non_random_adj(am,bm,window_val = 25,coarse = 0.07) #get Chumbley U stat into DF
    #res[i,7] <- cnr$p_value
  }
  DF <- Step_comp(Selection,50,0.7)
  DF2 <- Step_comp2(Selection,50)
  DF3 <- Step_comp3(Selection,50,0.7)
  res <- cbind(res,DF)
  res <- cbind(res,DF2)
  res <- cbind(res,DF3)
  return(list(allplots,res))
}
#Computes CCFs along aligned profiles with a chosen window size and computes the ratio of scores above a threshold
#it also computes the average of scores above threshold
Step_comp <- function(Selection,step,threshold){
  Cut_P <- CutPDF(Selection)
  
  Wcorr <- c()
for (i in 1:length(Cut_P$a2)){ 
  count <- 0
  #TOTCCF <- 0
  La2 <- length(Cut_P$a2[[i]])
  Tstep <- ceiling(La2/step)
  for (j in 0:((Tstep))) {
    
    deb <- step*j
    fin <- step+(step*j)
    
    
    if (fin < La2){
    a2 <- Cut_P$a2[[i]][deb:fin]
    b2 <- Cut_P$b2[[i]][deb:fin]
    CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
    
  
      if (CCF>threshold){
        count <- count+1
        #TOTCCF <- TOTCCF +CCF
      }
      else{
        count <- count
      }
    }
    
    
    else if (fin == La2){
    a2 <- Cut_P$a2[[i]][deb:fin]
    b2 <- Cut_P$b2[[i]][deb:fin]
    CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
    
    
      if (CCF>threshold){
      count <- count+1
      #TOTCCF <- TOTCCF +CCF
      }
      else{
      count <- count
      }
    break
    }
    
    else{
    deb <- fin-step
    fin <- La2
    a2 <- Cut_P$a2[[i]][deb:fin]
    b2 <- Cut_P$b2[[i]][deb:fin]
    CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
      if (CCF>threshold){
      count <- count+1
      #TOTCCF <- TOTCCF +CCF
      }
      else{
      count <- count
        }
      
    break
    }
    
    }
  ratio <- round(count/(Tstep),3)
  #Tot <- TOTCCF/count
  #name <- Cut_P$name[i]
  Wcorr[i] <- ratio
  Wcorr <- matrix(Wcorr)
  res2 <- data.frame(Wcorr)
  #res[i,2] <- Tot
  #res[i,3] <- name
  }
    
  return(res2)
}


Step_comp2 <- function(Selection,step){
  Cut_P <- CutPDF(Selection)
  Wcorr2 <- c()
  for (i in 1:length(Cut_P$a2)){ 
    count <- 0
    TOTCCF <- 0
    La2 <- length(Cut_P$a2[[i]])
    Tstep <- ceiling(La2/step)
    for (j in 0:((Tstep))) {
      
      deb <- step*j
      fin <- step+(step*j)
      
      
      if (fin < La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        
          TOTCCF <- TOTCCF +CCF
       
        
      }
      
      
      else if (fin == La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        
        
          TOTCCF <- TOTCCF +CCF
        
        
        break
      }
      
      else{
        deb <- fin-step
        fin <- La2
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
          TOTCCF <- TOTCCF +CCF
        
        
        break
      }
      
    }
    ratio <- round(TOTCCF/(Tstep),3)
    #Tot <- TOTCCF/count
    #name <- Cut_P$name[i]
    Wcorr2[i] <- ratio
    Wcorr2 <- matrix(Wcorr2)
    res2 <- data.frame(Wcorr2)
    #res[i,2] <- Tot
    #res[i,3] <- name
  }
  
  return(res2)

}


Step_comp3 <- function(Selection,step,threshold){
  Cut_P <- CutPDF(Selection)
  
  Wcorr3 <- c()
  for (i in 1:length(Cut_P$a2)){ 
    count <- 0
    #TOTCCF <- 0
    La2 <- length(Cut_P$a2[[i]])
    Tstep <- ceiling(La2/step)
    for (j in 0:((Tstep))) {
      
      deb <- step*j
      fin <- step+(step*j)
      
      
      if (fin < La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        count <- count + Transco(threshold, CCF,0,1)
        
        
      }
      
      
      else if (fin == La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        count <- count + Transco(threshold, CCF,0,1)
        
        
        break
      }
      
      else{
        deb <- fin-step
        fin <- La2
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        count <- count + Transco(threshold, CCF,0,1)
        
        break
      }
      
    }
    ratio <- round(count/(Tstep),3)
    #Tot <- TOTCCF/count
    #name <- Cut_P$name[i]
    Wcorr3[i] <- ratio
    Wcorr3 <- matrix(Wcorr3)
    res3 <- data.frame(Wcorr3)
    #res[i,2] <- Tot
    #res[i,3] <- name
  }
  
  return(res3)
}



#To retrieve a DF with cut aligned profiles, can be used for single computation of similarity measures
CutPDF <- function(Selection){
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table)
  Cut_P <-  data.frame(matrix(NA,nrow=nProfile,ncol=2)) #create Df for cut profiles
  colnames(Cut_P) <- c("a2", "b2")
  Lags <- CCF_all(Selection)[[2]]
  for(i in 1:nProfile){
    
    #name <- paste(Selection$File[table[i,1]],"vs.",Selection$File[table[i,2]])
    a <- unlist(Selection$y[table[i,1]])
    b <- unlist(Selection$y[table[i,2]])
    Lag <- Lags$lag[i]  
    if (Lag<0) {
      a2 <- head(a,-abs(Lag))
      b2 <- tail(b,-abs(Lag))
    }
    else if (Lag==0){
      a2 <- a
      b2 <- b
    }
    else {
      a2 <- tail(a,-Lag)
      b2 <- head(b,-Lag)
    }
    a2_l <- list(a2)
    b2_l <- list(b2)
    Cut_P$a2[i] <- a2_l #copy cut profile into dataframe cell 1stcol
    Cut_P$b2[i] <- b2_l #copy cut profile into dataframe cell 2ndcol
    #Cut_P$name[i] <- name
  }
  
  return(Cut_P)
}

#computes Relative Distances according to Bachrach 2010
reldist <- function(a,b){
  x <- 1:length(a)
  i <- x
  nume <- sum(((a[i])-(b[i]))^2)
  denom <- sum(((a[i])+(b[i]))^2)
  
  rdist <- 1-(nume/denom)
  return(rdist) 
}

#To compute dtw with specific parameters and extract normalizedDistance
ndtw <- function(x, y, ...) {
  dtw(x, y, ...,
      step.pattern = asymmetric,
      open.end = TRUE,
      open.begin = TRUE)$normalizedDistance
}

#To compute ndtw separately on the batch with aligned profiles
batch_ndtw <- function(Selection){
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table)
  NDTW <- data.frame(matrix(NA,nrow=nProfile,ncol=1))
  colnames(NDTW) <- c("Ndtw")
  Cut_P <- CutPDF(Selection)
  for(i in 1:nProfile){
    a2i <- unlist(Cut_P$a2[i])
    b2i <- unlist(Cut_P$b2[i])
    score <- ndtw(a2i,b2i)
    NDTW$Ndtw[i] <- score
  }
  return(NDTW)
}

#To compute reldist separately on the batch with aligned profiles
batch_reldist <- function(Selection){
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table)
  rdist <- data.frame(matrix(NA,nrow=nProfile,ncol=1))
  colnames(rdist) <- c("Rdist")
  Cut_P <- CutPDF(Selection)
  for(i in 1:nProfile){
    a2i <- unlist(Cut_P$a2[i])
    b2i <- unlist(Cut_P$b2[i])
    score <- reldist(a2i,b2i)
    rdist$Rdist[i] <- score
  }
  return(rdist)
}


batch_Chumbley <- function(Selection){
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table)
  chum <- data.frame(matrix(NA,nrow=nProfile,ncol=6))
  colnames(chum) <- c("U_stat1","P_val1","U_stat2","P_val2","U_statAdj","P_valAdj")
  Cut_P <- CutPDF(Selection)
  for(i in 1:nProfile){
    a2i <- unlist(Cut_P$a2[i])
    b2i <- unlist(Cut_P$b2[i])
    am <- matrix(a2i)
    bm <- matrix(b2i)
    
    cnr <- chumbley_non_random(am,bm,window_val = 25,coarse = 0.07)
    cnr2 <- chumbley_non_random2(am,bm,window_val = 25,coarse = 0.07)
    cnradj <- chumbley_non_random_adj(am,bm,window_val = 25,coarse = 0.07)
    
    chum$U_stat1[i] <- cnr$U
    chum$P_val1[i] <- cnr$p_value
    chum$U_stat2[i] <- cnr2$U
    chum$P_val2[i] <- cnr2$p_value
    chum$U_statAdj[i] <- cnradj$U
    chum$P_valAdj[i] <- cnradj$p_value
  }
  return(chum)
}
#To compute Aligned_scores for multiple Selection like different ROIs and put it in one dataframe
Comb_ROI <- function(Selection1, Selection2, Selection3){
  table <- as.data.frame(t(combn(length(Selection1$y),2)))
  res1 <- Aligned_scores(Selection1)[[2]]
  res2 <- Aligned_scores(Selection2)[[2]]
  res3 <- Aligned_scores(Selection3)[[2]]
  
  ResTot <- bind_rows(rowid_to_column(res1),
                      rowid_to_column(res2),
                      rowid_to_column(res3)) %>% 
    group_by(rowid) %>%
    nest(nest_data = -rowid) %>% 
    unnest_wider(nest_data) %>% 
    ungroup() %>% 
    dplyr::select(-rowid)
  
  return(ResTot)
}

#To compute the mean of scores from Aligned_scores with within and between variabilities, here for the correlation but
Mean_calc <- function(Res){
  
  Mean_DF <-  data.frame(matrix(NA,ncol=6))
  colnames(Mean_DF) <- c("intraZ1","intraZ2","intraZ3","interZ1Z2","interZ1Z3","interZ2Z3")
  M_intraZ1 <- mean(c(Res$corr[1:9],Res$corr[30:37],Res$corr[58:64],Res$corr[85:90],Res$corr[111:115],Res$corr[136:139],Res$corr[160:162],Res$corr[183:184],Res$corr[205]))
  M_intraZ2 <- mean(c(Res$corr[246:254],Res$corr[265:272],Res$corr[283:289],Res$corr[300:305],Res$corr[316:320],Res$corr[331:334],Res$corr[345:347],Res$corr[358:359],Res$corr[370]))
  M_intraZ3 <- mean(c(Res$corr[391-435]))
  M_interZ1Z2 <- mean(c(Res$corr[10:19],Res$corr[38:47],Res$corr[65:74],Res$corr[91:100],Res$corr[116:125],Res$corr[140:149],Res$corr[163:172],Res$corr[185:194],Res$corr[206:215],Res$corr[226:235]))
  M_interZ1Z3 <- mean(c(Res$corr[20:29],Res$corr[48:57],Res$corr[75:84],Res$corr[101:110],Res$corr[126:135],Res$corr[150:159],Res$corr[173:182],Res$corr[195:204],Res$corr[216:225],Res$corr[236:245]))
  M_interZ2Z3 <- mean(c(Res$corr[255:264],Res$corr[273:282],Res$corr[290:299],Res$corr[306:315],Res$corr[321:330],Res$corr[335:344],Res$corr[348:357],Res$corr[360:369],Res$corr[371:390]))
  
  Mean_DF$intraZ1[1] <- M_intraZ1
  Mean_DF$intraZ2[1] <- M_intraZ2
  Mean_DF$intraZ3[1] <- M_intraZ3
  Mean_DF$interZ1Z2[1] <- M_interZ1Z2
  Mean_DF$interZ1Z3[1] <- M_interZ1Z3
  Mean_DF$interZ2Z3[1] <- M_interZ2Z3
  
  return(Mean_DF)

}

#same as CutPDF but between two selections
CutPDF_IP <- function(Selection_A, Selection_B){
  table <- as.data.frame(as.data.frame(expand.grid(c(1:(length(Selection_A$y))),c(1:(length(Selection_B$y))))))
  nProfile <- nrow(table)
  Cut_P <-  data.frame(matrix(NA,nrow=nProfile,ncol=2)) #create Df for cut profiles
  colnames(Cut_P) <- c("a2", "b2")
  Lags <- CCF_all_IP(Selection_A,Selection_B)[[2]]
  for(i in 1:nProfile){
    a <- unlist(Selection_A$y[table[i,1]])
    b <- unlist(Selection_B$y[table[i,2]])
    Lag <- Lags$lag[i]  
    if (Lag<0) {
      a2 <- head(a,-abs(Lag))
      b2 <- tail(b,-abs(Lag))
    }
    
    else if (Lag==0){
      a2 <- a
      b2 <- b
    }
    else {
      a2 <- tail(a,-Lag)
      b2 <- head(b,-Lag)
    }
    
    a2_l <- list(a2)
    b2_l <- list(b2)
    Cut_P$a2[i] <- a2_l #copy cut profile into dataframe cell 1stcol
    Cut_P$b2[i] <- b2_l #copy cut profile into dataframe cell 2ndcol
  }
  
  return(Cut_P)
}

#Computes CCFs along aligned profiles with a chosen window size and computes the ratio of scores above a threshold
#it also computes the average of scores above threshold
#Here between 2 selections
Step_comp_IP <- function(Selection_A, Selection_B,step,threshold){
  Cut_P <- CutPDF_IP(Selection_A,Selection_B)
  
  Wcorr <- c()
  for (i in 1:length(Cut_P$a2)){ 
    count <- 0
    #TOTCCF <- 0
    La2 <- length(Cut_P$a2[[i]])
    Tstep <- ceiling(La2/step)
    for (j in 0:((Tstep))) {
      
      deb <- step*j
      fin <- step+(step*j)
      
      
      if (fin < La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        
        if (CCF>threshold){
          count <- count+1
          #TOTCCF <- TOTCCF +CCF
        }
        else{
          count <- count
        }
      }
      
      
      else if (fin == La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        
        if (CCF>threshold){
          count <- count+1
          #TOTCCF <- TOTCCF +CCF
        }
        else{
          count <- count
        }
        break
      }
      
      else{
        deb <- fin-step
        fin <- La2
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        if (CCF>threshold){
          count <- count+1
          #TOTCCF <- TOTCCF +CCF
        }
        else{
          count <- count
        }
        
        break
      }
      
    }
    ratio <- round(count/(Tstep),3)
    #Tot <- TOTCCF/count
    #name <- Cut_P$name[i]
    Wcorr[i] <- ratio
    Wcorr <- matrix(Wcorr)
    res2 <- data.frame(Wcorr)
    #res[i,2] <- Tot
    #res[i,3] <- name
  }
  
  return(res2)
}

Step_comp2_IP <- function(Selection_A,Selection_B,step){
  Cut_P <- CutPDF_IP(Selection_A,Selection_B)
  Wcorr2 <- c()
  for (i in 1:length(Cut_P$a2)){ 
    count <- 0
    TOTCCF <- 0
    La2 <- length(Cut_P$a2[[i]])
    Tstep <- ceiling(La2/step)
    for (j in 0:((Tstep))) {
      
      deb <- step*j
      fin <- step+(step*j)
      
      
      if (fin < La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        
        TOTCCF <- TOTCCF +CCF
        
        
      }
      
      
      else if (fin == La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        
        
        TOTCCF <- TOTCCF +CCF
        
        
        break
      }
      
      else{
        deb <- fin-step
        fin <- La2
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        TOTCCF <- TOTCCF +CCF
        
        
        break
      }
      
    }
    ratio <- round(TOTCCF/(Tstep),3)
    #Tot <- TOTCCF/count
    #name <- Cut_P$name[i]
    Wcorr2[i] <- ratio
    Wcorr2 <- matrix(Wcorr2)
    res2 <- data.frame(Wcorr2)
    #res[i,2] <- Tot
    #res[i,3] <- name
  }
  
  return(res2)
  
}


Step_comp3_IP <- function(Selection_A,Selection_B,step,threshold){
  Cut_P <- CutPDF_IP(Selection_A,Selection_B)
  
  Wcorr3 <- c()
  for (i in 1:length(Cut_P$a2)){ 
    count <- 0
    #TOTCCF <- 0
    La2 <- length(Cut_P$a2[[i]])
    Tstep <- ceiling(La2/step)
    for (j in 0:((Tstep))) {
      
      deb <- step*j
      fin <- step+(step*j)
      
      
      if (fin < La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        
        count <- count + Transco(threshold, CCF,0,1)
        
        
      }
      
      
      else if (fin == La2){
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        count <- count + Transco(threshold, CCF,0,1)
        
        
        break
      }
      
      else{
        deb <- fin-step
        fin <- La2
        a2 <- Cut_P$a2[[i]][deb:fin]
        b2 <- Cut_P$b2[[i]][deb:fin]
        CCF <- max(ccf(a2,b2, plot=FALSE)$acf)
        count <- count + Transco(threshold, CCF,0,1)
        
        break
      }
      
    }
    ratio <- round(count/(Tstep),3)
    #Tot <- TOTCCF/count
    #name <- Cut_P$name[i]
    Wcorr3[i] <- ratio
    Wcorr3 <- matrix(Wcorr3)
    res3 <- data.frame(Wcorr3)
    #res[i,2] <- Tot
    #res[i,3] <- name
  }
  
  return(res3)
}

#computes the CCF and extracts the lag between two different tools
CCF_all_IP <- function(Selection_A,Selection_B){
  
  table <- as.data.frame(as.data.frame(expand.grid(c(1:(length(Selection_A$y))),c(1:(length(Selection_B$y))))))
  nProfile <- nrow(table)
  allplots <- vector(mode="list", length=nProfile)
  res <-  data.frame(matrix(NA,nrow=nProfile,ncol=3))
  for(i in 1:nProfile) {
    a <- unlist(Selection_A$y[table[i,1]])
    b <- unlist(Selection_B$y[table[i,2]])
    nameComp <- paste(Selection_A$File[table[i,1]],"vs.",Selection_B$File[table[i,2]])
    ccf <- CCF(a, b)[[2]]
    res[i,1] <- nameComp
    res[i,2:3] <- ccf
    
    allplots[[i]] <- CCF(a,b)[[1]]
    allplots[[i]]$labels[1] <- paste(substr(Selection_A$File[table[i,1]],1,11),
                                     "vs",substr(Selection_B$File[table[i,2]],1,11), 
                                     "   Lag = ", res[i,2],"     score = ",
                                     res[i,3])
    allplots[[i]]$labels[2] <- ""
    allplots[[i]]$labels[3] <- "depth"
    
  }
  colnames(res) <- c("nameComp", "lag", "corr")
  return(list(allplots,res))
}

#To align profiles and compute other metrics using aligned profiles on different tools
Aligned_scores_IP <- function(Selection_A,Selection_B){
  table <- as.data.frame(as.data.frame(expand.grid(c(1:(length(Selection_A$y))),c(1:(length(Selection_B$y))))))
  res <- CCF_all_IP(Selection_A, Selection_B)[[2]]
  allplots <- CCF_all_IP(Selection_A,Selection_B)[[1]]
  res$ndtw <- NA
  res$rdist <- NA
  res$CCF2 <- NA
  #res$Uscore <- NA
  nProfile <- nrow(table) 
  
  for(i in 1:nProfile){
    a <- unlist(Selection_A$y[table[i,1]])
    b <- unlist(Selection_B$y[table[i,2]])
    Lag <- res$lag[i]  #ici ce sera test 
    if (Lag<0) {
      a2 <- head(a,-abs(Lag))
      b2 <- tail(b,-abs(Lag))
    }
    else if (Lag>0) {
      a2 <- tail(a,-Lag)
      b2 <- head(b,-Lag)
    }
    else { #if the lag is 0
      a2 <- a
      b2 <- b
    }
    

    
    CCF2 <- max(ccf(a2,b2)$acf)
    CCor <- CCorDistance(a,b,lag.max=100)
    NDTW <- ndtw(a2,b2) #get ndtw score into DF
    Rdist <- reldist(a2,b2) #get relativedistance frome bachrach in DF
    res[i,4] <- NDTW
    res[i,5] <- Rdist
    res[i,6] <- CCF2
    
    # am <- as.matrix(a2)
    #bm <- as.matrix(b2)
    # cnr <- chumbley_non_random(am,bm,window_val = 25,coarse = 0.07) #get Chumbley U stat into DF
    # res[i,6] <- cnr$U
  }
  
  DF <- Step_comp_IP(Selection_A,Selection_B,50,0.7)
  DF2 <- Step_comp2_IP(Selection_A,Selection_B,50)
  DF3 <- Step_comp3_IP(Selection_A,Selection_B,50,0.7)
  res <- cbind(res,DF)
  res <- cbind(res,DF2)
  res <- cbind(res,DF3)
  
  return(list(allplots,res))
}

#To retrieve the within and between variability score separatly
WB_var <- function(Resultat){
#Resultat <- Resultat[[2]]  
NAMES <- Resultat[["nameComp"]]
intra_Z1 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
intra_Z2 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
intra_Z3 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
inter_Z1Z2 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
inter_Z2Z3 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
inter_Z1Z3 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
for (i in 1:3) {
  for (name in NAMES){
    if ((substr(name,4,4)==i) & (substr(name,24,24)==i)){
      if (i == 1){
        intra_Z1 <- rbind(intra_Z1, filter(Resultat, nameComp == name))
        
        
      }
      if (i == 2){
        intra_Z2 <- rbind(intra_Z2, filter(Resultat, nameComp == name))
       
      }
      if (i == 3){
        intra_Z3 <- rbind(intra_Z3, filter(Resultat, nameComp == name))
        
      }
    }
  }
}

intra_Z1$VarROI <- c(rep(c("W1"), times=length(intra_Z1$corr)))
intra_Z1$Var <- c(rep(c("W"), times=length(intra_Z1$corr)))
intra_Z2$VarROI <- c(rep(c("W2"), times=length(intra_Z2$corr)))
intra_Z2$Var <- c(rep(c("W"), times=length(intra_Z2$corr)))
intra_Z3$VarROI <- c(rep(c("W3"), times=length(intra_Z3$corr)))
intra_Z3$Var <- c(rep(c("W"), times=length(intra_Z3$corr)))

for (name in NAMES){
  if (((substr(name,4,4) == 1) & (substr(name,24,24)== 2)))
    inter_Z1Z2 <- rbind(inter_Z1Z2, filter(Resultat, nameComp == name))
    
  
  if (((substr(name,4,4) == 1) & (substr(name,24,24)== 3)))
    inter_Z1Z3 <- rbind(inter_Z1Z3, filter(Resultat, nameComp == name))
    
  if (((substr(name,4,4) == 2) & (substr(name,24,24)== 3)))
    inter_Z2Z3 <- rbind(inter_Z2Z3, filter(Resultat, nameComp == name))
    
}

inter_Z1Z2$VarROI <- c(rep(c("B1"), times=length(inter_Z1Z2$corr)))
inter_Z1Z2$Var <- c(rep(c("B"), times=length(inter_Z1Z2$corr)))
inter_Z1Z3$VarROI <- c(rep(c("B2"), times=length(inter_Z1Z3$corr)))
inter_Z1Z3$Var <- c(rep(c("B"), times=length(inter_Z1Z3$corr)))
inter_Z2Z3$VarROI <- c(rep(c("B3"), times=length(inter_Z2Z3$corr)))
inter_Z2Z3$Var <- c(rep(c("B"), times=length(inter_Z2Z3$corr)))

  DF <-rbind(intra_Z1,intra_Z2, intra_Z3,inter_Z1Z2, inter_Z1Z3, inter_Z2Z3)
return(DF)
}


WB_var_IP <- function(Resultat){
  #Resultat <- Resultat[[2]]  
  NAMES <- Resultat[["nameComp"]]
  intra_K1 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
  intra_K2 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
  inter_K1K2 <- data.frame(nameComp = character(), lag = double(), corr = double(), ndtw = double, rdist = double())
  for (i in 2:3) {
    for (name in NAMES){
      if ((substr(name,2,2)==i) & (substr(name,22,22)==i)){
        if (i == 2){
          intra_K1 <- rbind(intra_K1, filter(Resultat, nameComp == name))
          
          
        }
        if (i == 3){
          intra_K2 <- rbind(intra_K2, filter(Resultat, nameComp == name))
          
        }
        
      }
    }
  }
  
  intra_K1$VarROI <- c(rep(c("W1"), times=length(intra_K1$corr)))
  intra_K1$Var <- c(rep(c("W"), times=length(intra_K1$corr)))
  intra_K2$VarROI <- c(rep(c("W2"), times=length(intra_K2$corr)))
  intra_K2$Var <- c(rep(c("W"), times=length(intra_K2$corr)))

  
  for (name in NAMES){
    if (((substr(name,2,2) == 2) & (substr(name,22,22)== 3)))
      inter_K1K2 <- rbind(inter_K1K2, filter(Resultat, nameComp == name))
    
  }
  
  inter_K1K2$VarROI <- c(rep(c("B1"), times=length(inter_K1K2$corr)))
  inter_K1K2$Var <- c(rep(c("B"), times=length(inter_K1K2$corr)))
  
  
  DF <-rbind(intra_K1,intra_K2, inter_K1K2)
  return(DF)
}


# This function plots within and between variability distributions for a single tool

Distrib_S <- function(Res1){
  
  #Res1 <- Aligned_scores(Selection)
  Zone <- WB_var(Res1[[2]])
  names(Zone)[names(Zone) == 'VarROI'] <- 'VarZone'
  
  q1 <- ggplot(Zone,aes(x=Wcorr3,color=Var))+geom_density()
  q2 <- ggplot(Zone,aes(x=rdist,color=Var))+geom_density()
  q3 <- ggplot(Zone,aes(x=CCF2,color=Var))+geom_density()
  q4 <- ggplot(Zone,aes(x=Wcorr3,color=VarZone))+geom_density()
  q5 <- ggplot(Zone,aes(x=rdist,color=VarZone))+geom_density()
  q6 <- ggplot(Zone,aes(x=CCF2,color=VarZone))+geom_density()
  
  
  figure <- multi_panel_figure(columns = 3, rows = 2, panel_label_type = "none")
  
  figure %<>%
    fill_panel(q1, column = 1, row = 1) %<>%
    fill_panel(q2, column = 2, row = 1) %<>%
    fill_panel(q3, column = 3, row = 1) %<>%
    fill_panel(q4, column = 1, row = 2) %<>%
    fill_panel(q5, column = 2, row = 2) %<>%
    fill_panel(q6, column = 3, row = 2)
  
  return(figure)
  
}

Distrib_S2 <- function(Res1){
  
  #Res1 <- Aligned_scores(Selection)
  Zone <- WB_var(Res1[[2]])
  names(Zone)[names(Zone) == 'VarROI'] <- 'VarZone'
  
  q1 <- ggplot(Zone,aes(x=corr,color=Var))+geom_density()
  q2 <- ggplot(Zone,aes(x=Wcorr,color=Var))+geom_density()
  q3 <- ggplot(Zone,aes(x=Wcorr2,color=Var))+geom_density()
  q4 <- ggplot(Zone,aes(x=corr,color=VarZone))+geom_density()
  q5 <- ggplot(Zone,aes(x=Wcorr,color=VarZone))+geom_density()
  q6 <- ggplot(Zone,aes(x=Wcorr2,color=VarZone))+geom_density()
  
  
  figure <- multi_panel_figure(columns = 3, rows = 2, panel_label_type = "none")
  
  figure %<>%
    fill_panel(q1, column = 1, row = 1) %<>%
    fill_panel(q2, column = 2, row = 1) %<>%
    fill_panel(q3, column = 3, row = 1) %<>%
    fill_panel(q4, column = 1, row = 2) %<>%
    fill_panel(q5, column = 2, row = 2) %<>%
    fill_panel(q6, column = 3, row = 2)
  
  return(figure)
  
}

#Selections must be for the 3 ROIs, this functions binds the results and plots for all ROIs at the same time
Distrib_ROI <- function(Selection1, Selection2,Selection3){
  Res1 <- Aligned_scores(Selection1)
  Res2 <- Aligned_scores(Selection2)
  Res3 <- Aligned_scores(Selection3)
  
  ROI1 <- WB_var(Res1) 
  ROI2 <- WB_var(Res2)
  ROI3 <- WB_var(Res3) 
  
  DFA <-rbind(ROI1,ROI2,ROI3)
  
  q1 <- ggplot(DFA,aes(x=corr,color=Var))+geom_density()
  q2 <- ggplot(DFA,aes(x=rdist,color=Var))+geom_density()
  q3 <- ggplot(DFA,aes(x=CCF2,color=Var))+geom_density()
  q4 <- ggplot(DFA,aes(x=corr,color=VarROI))+geom_density()
  q5 <- ggplot(DFA,aes(x=rdist,color=VarROI))+geom_density()
  q6 <- ggplot(DFA,aes(x=CCF2,color=VarROI))+geom_density()

  
  
  figure <- multi_panel_figure(columns = 3, rows = 2, panel_label_type = "none")
  
  figure %<>%
    fill_panel(q1, column = 1, row = 1) %<>%
    fill_panel(q2, column = 2, row = 1) %<>%
    fill_panel(q3, column = 3, row = 1) %<>%
    fill_panel(q4, column = 1, row = 2) %<>%
    fill_panel(q5, column = 2, row = 2) %<>%
    fill_panel(q6, column = 3, row = 2) %<>%

  
  return(figure)
}
# same as Distrib_ROI but with other metrics showed
Distrib_ROI2 <- function(Selection1, Selection2,Selection3){
  Res1 <- Aligned_scores(Selection1)
  Res2 <- Aligned_scores(Selection2)
  Res3 <- Aligned_scores(Selection3)
  
  ROI1 <- WB_var(Res1) 
  ROI2 <- WB_var(Res2)
  ROI3 <- WB_var(Res3) 
  
  DFA <-rbind(ROI1,ROI2,ROI3)
  
  q1 <- ggplot(DFA,aes(x=lag,color=Var))+geom_density()
  q2 <- ggplot(DFA,aes(x=ACF,color=Var))+geom_density()
  q3 <- ggplot(DFA,aes(x=CCorr,color=Var))+geom_density()
  q4 <- ggplot(DFA,aes(x=lag,color=VarROI))+geom_density()
  q5 <- ggplot(DFA,aes(x=ACF,color=VarROI))+geom_density()
  q6 <- ggplot(DFA,aes(x=CCorr,color=VarROI))+geom_density()
  
  
  
  figure <- multi_panel_figure(columns = 3, rows = 2, panel_label_type = "none")
  
  figure %<>%
    fill_panel(q1, column = 1, row = 1) %<>%
    fill_panel(q2, column = 2, row = 1) %<>%
    fill_panel(q3, column = 3, row = 1) %<>%
    fill_panel(q4, column = 1, row = 2) %<>%
    fill_panel(q5, column = 2, row = 2) %<>%
    fill_panel(q6, column = 3, row = 2) %<>%
    
    
    return(figure)
}

#plots a heatmap of scores
Plot_HeatMap <- function(Selection){ 
  Res <- Aligned_scores(Selection)
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  Res <- Res[[2]]$corr
  HM_df <- cbind(table,Res)
  
 P <-  ggplot(data = HM_df, aes(V2, V1, fill = Res))+  
   geom_tile(color = "white", size=0.2)+
   scale_x_discrete(limits=factor(c(1:max(table$V2))))+
   scale_y_discrete(limits=factor(c(1:max(table$V2))))+
   scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                        #midpoint = mean(Res),limit = c(min(Res),max(Res)),
                        midpoint = 0.5,limit = c(0,1),
                        space = "Lab",
                        name="Scores") +
   theme(panel.grid = element_blank())+
   labs(title= "Heatmap scores", 
        x= "",
        y= "")
  
  return(P)
}
#comoputes dataframes of scores when varying parameters like window width and threshold
List_sco <- function(Selection){
  
  table <- as.data.frame(t(combn(length(Selection$y),2)))
  nProfile <- nrow(table)
  
  Steps <- c(10,25,50,100,200)
  Thlds <- seq(0.3,0.7,0.1)
  len <- length(Steps)*length(Thlds)
  res <-  data.frame(matrix(nrow=nProfile, ncol=len))
  count <- 0
  for (i in 1: length(Steps)){
    for (j in 1: length(Thlds)){
      Col <- Step_comp3(Selection, Steps[i], Thlds[j])
      count <- count+1
      res[,count] <- Col
    }}
  
  names(res)[1:len] <- paste("Sco", 1:len, sep="")
  
  return(res)}


# Computes the lda for scores obtained with different parameters and compares the accuracy of the lda by plotting them to see which parameters seem the best
#it needs a dataframe from Aligned_scores function and one from List_sco function
Prep2 <- function(un,unbis,pval){
  
  #un <- Aligned_scores(Selection)
  #unbis <- List_sco(Selection)
  deux <-  WB_var(na.omit(un[[2]]))
  trois <- cbind(deux,unbis)
  
  X <- as.character(c(10,25,50,100,200))
  Y <- as.character(c(seq(0.3,0.7,0.1)))
  len <- length(X)*length(Y)
  data <- expand.grid(X, Y)
  mylist <- list()
  #set.seed(10)
  for (j in 1:4){
  index <-  createDataPartition(y=trois$Var, p=pval, list=FALSE) #create a list of indexes to create a train and test set
    for (i in 1:len){
   
      train = trois[index,c(2:9,11,i+11)] #creates train set based on index
      test = trois[-index,c(2:9,11,i+11)]
      
      lda.fit = train(Var ~ ., data=train, method="lda", #computes LDA
                      trControl = trainControl(method = "cv"))
      #TrainAcc <- round(lda.fit$results$Accuracy,4) #extracts LDA accurary on train set
      pred.Var <-  predict(lda.fit, test) #predicts on test set
      PredAcc <-  round(mean(pred.Var == test$Var),4)#extracts accurarcy on test set
      #data$Z[i] <- TrainAcc
      data$Z2[i] <- PredAcc
      #table(pred.Var, test$Var) ## table of what is properly classified and not
    }
  mylist[[j]] <- data
  
  }
  resm <- as.data.frame(apply(array(unlist(mylist), c(dim(mylist[[1]]), length(mylist))),2, rowMeans))
  ressd <- as.data.frame(apply(array(unlist(mylist), c(dim(mylist[[1]]), length(mylist))),2, rowSds))
  
  Resm <- cbind(data,round(dplyr::select(resm,V3),4))
  ResSD <- cbind(data,round(dplyr::select(ressd,V3),4))
  
  Plot1 <- ggplot(Resm, aes(Var1, Var2, fill= V3)) + 
    geom_tile(color="black")+
    geom_text(aes(label = V3), color = "white", size = 3)+
    labs(x="window width", y="Threshold")+
    ggtitle("Tes set mean accuracy")
  
  Plot2 <- ggplot(ResSD, aes(Var1, Var2, fill= V3)) + 
    geom_tile(color="black")+
    geom_text(aes(label = V3), color = "white", size = 3)+
    labs(x="window width", y="Threshold")+
    ggtitle("Test set SD accuracy")
  plots <- grid.arrange(Plot1,Plot2, ncol=2)
  
  
  return(plots)
  
}




Prep <- function(un,unbis,pval){
  
  #un <- Aligned_scores(Selection)
  #unbis <- List_sco(Selection)
  deux <-  WB_var(na.omit(un[[2]]))
  trois <- cbind(deux,unbis)
  
  
  
  X <- as.character(c(10,25,50,100,200))
  Y <- as.character(c(seq(0.3,0.7,0.1)))
  len <- length(X)*length(Y)
  data <- expand.grid(X, Y)
  set.seed(10)
  index <-  createDataPartition(y=trois$Var, p=pval, list=FALSE) #create a list of indexes to create a train and test set
  for (i in 1:len){
    
   
    train = trois[index,c(2:8,10,i+10)] #creates train set based on index
    test = trois[-index,c(2:8,10,i+10)]
    
    lda.fit = train(Var ~ ., data=train, method="lda", #computes LDA
                    trControl = trainControl(method = "cv"))
    TrainAcc <- round(lda.fit$results$Accuracy,4) #extracts LDA accurary on train set
    pred.Var <-  predict(lda.fit, test) #predicts on test set
    PredAcc <-  round(mean(pred.Var == test$Var),4)#extracts accurarcy on test set
    data$Z[i] <- TrainAcc
    data$Z2[i] <- PredAcc
    #table(pred.Var, test$Var) ## table of what is properly classified and not
  }
  Plot1 <- ggplot(data, aes(Var1, Var2, fill= Z)) + 
    geom_tile(color="black")+
    geom_text(aes(label = Z), color = "white", size = 3)+
    labs(x="window width", y="Threshold")+
    ggtitle("Train set accuracy")
  
  Plot2 <- ggplot(data, aes(Var1, Var2, fill= Z2)) + 
    geom_tile(color="black")+
    geom_text(aes(label = Z2), color = "white", size = 3)+
    labs(x="window width", y="Threshold")+
    ggtitle("Test set accuracy")
  plots <- grid.arrange(Plot1,Plot2, ncol=2)
  
  
  return(plots)
  
}



#computes the correlation matrix between scores
#input is a dataframe, each column being a different score
Corr_Mat <- function(Scores){
corr <- round(cor(Scores), 1)


Cplot <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white")

return(Cplot)
}



chumbley_non_random_adj <- function(data1, data2, window_opt = 500, window_val = 50, coarse = .25){
  
  unity <- function(x) {x / sqrt(sum(x^2))} ## normalize columns of a matrix to make correlation computation faster
  
  ####################################################
  ##Clean the marks and compute the smooth residuals##
  ####################################################
  
  data1 <- matrix(data1[round((0.01*nrow(data1))):round(0.99*nrow(data1)),], ncol = 1)
  data2 <- matrix(data2[round((0.01*nrow(data2))):round(0.99*nrow(data2)),], ncol = 1)
  
  ##Normalize the tool marks
  if (!is.null(coarse)) {
    y1 <- data1 - lowess(y = data1,  x = 1:nrow(data1), f= coarse)$y
    y2 <- data2 - lowess(y = data2,  x = 1:nrow(data2), f= coarse)$y
  } else {
    if (any(is.na(data1)) | any(is.na(data2))) browser()
    
    y1 = data1
    y2 = data2
  }
  
  
  ############################################
  ##Compute the observed maximum correlation##
  ############################################
  
  #####################
  ##Optimization step##
  #####################
  ##Each column in these matrices corresponds to a window in the respective tool mark
  y1_mat_opt <- matrix(NA, ncol = length(1:(length(y1) - (window_opt - 1))), nrow = window_opt)
  for(l in 1:(length(y1) - (window_opt - 1))){
    y1_mat_opt[,l] <- y1[l:(l+(window_opt - 1))]
  }
  y2_mat_opt <- matrix(NA, ncol = length(1:(length(y2) - (window_opt - 1))), nrow = window_opt)
  for(l in 1:(length(y2) - (window_opt - 1))){
    y2_mat_opt[,l] <- y2[l:(l+(window_opt - 1))]
  }
  
  ##Compute the correlation between all pairs of windows for the two marks
  ##Rows in the following matrix are mark 2, columns are mark 1
  y2_mat_opt <- apply(scale(y2_mat_opt), 2, unity)
  y1_mat_opt <- apply(scale(y1_mat_opt), 2, unity)
  corr_mat_opt <- t(y2_mat_opt) %*% y1_mat_opt ##correlation matrix
  max_corr_opt_loc <- which(corr_mat_opt == max(corr_mat_opt, na.rm=TRUE), arr.ind = TRUE) ##pair of windows maximizing the correlation
  
  
  ###################
  ##Validation step##
  ###################
  ##Each column in these matrices corresponds to a window in the respective tool mark
  y1_mat_val <- matrix(NA, ncol = length(1:(length(y1) - (window_val - 1))), nrow = window_val)
  for(l in 1:(length(y1) - (window_val - 1))){
    y1_mat_val[,l] <- y1[l:(l+(window_val - 1))]
  }
  y2_mat_val <- matrix(NA, ncol = length(1:(length(y2) - (window_val - 1))), nrow = window_val)
  for(l in 1:(length(y2) - (window_val - 1))){
    y2_mat_val[,l] <- y2[l:(l+(window_val - 1))]
  }
  
  ##Compute the correlation between all pairs of windows for the two marks
  ##Rows in the following matrix are mark 2, columns are mark 1
  y2_mat_val <- apply(scale(y2_mat_val), 2, unity)
  y1_mat_val <- apply(scale(y1_mat_val), 2, unity)
  corr_mat_val <- t(y2_mat_val) %*% y1_mat_val
  
  ##Pull out the correlations that correspond to windows with the same offset as the largest correlation found in the optimization step
  ##Pull out the correlations that correspond to windows with the same offset as the largest correlation found in the optimization step
  same_shift <- data.frame(row = NA, col = NA, U = NA)
  rows <- max_corr_opt_loc[1] + (window_opt - window_val)
  cols <- max_corr_opt_loc[2] + (window_opt - window_val)
  while(rows + window_val < nrow(corr_mat_val) & cols + window_val < ncol(corr_mat_val)){
    
    rows <- rows + window_val
    cols <- cols + window_val
    same_shift <- rbind(same_shift, c(rows, cols, corr_mat_val[rows,cols]))
    
  }
  rows <- max_corr_opt_loc[1]
  cols <- max_corr_opt_loc[2]
  while(rows - window_val > 0 & cols - window_val > 0){
    
    rows <- rows - window_val
    cols <- cols - window_val
    same_shift <- rbind(same_shift, c(rows, cols, corr_mat_val[rows, cols]))
    
  }
  same_shift <- same_shift[-1,]
  my_same_shift <- same_shift
  my_diff_shift <- data.frame(
    row = sort(my_same_shift$row),
    col = sort(my_same_shift$col, decreasing=TRUE)
  )
  # remove the middle value from a set of odd rows:
  idx <- sort(same_shift$col) == sort(same_shift$col, decreasing=TRUE)
  my_diff_shift <- subset(my_diff_shift, idx==FALSE)
  
  my_diff_shift$U = apply(my_diff_shift, MARGIN=1, FUN = 
                            function(x) corr_mat_val[x[1], x[2]])
  
  ######################################
  ##Compute the Ustatistic if possible##
  ######################################
  if(nrow(my_same_shift) == 0 | nrow(my_diff_shift) == 0) {
    
    obs_U <- NA
    n <- length(my_same_shift$U)
    m <- length(my_diff_shift$U)
    
  }
  
  if(nrow(my_same_shift) != 0 & nrow(my_diff_shift) != 0) {
    
    ranks <- rank(c(my_same_shift$U, my_diff_shift$U))
    Rx <- ranks[seq_along(my_same_shift$U)]
    Ry <- ranks[-(1:length(Rx))]
    n <- length(my_same_shift$U)
    m <- length(my_diff_shift$U)
    N <- n + m
    
    t <- sum(Rx) ##Test statistic...sum of sample one ranks
    t1 <- (t - n*((N + 1) / 2)) / sqrt( ((n*m)/(N*(N-1)))*sum(c(Rx^2, Ry^2)) - ((n*m*(N+1)^2) / (4*(N-1)))) ##Standardized test statistics
    obs_U <- t1
    
  }
  pval <- 1 - pnorm(obs_U)
  
  list(same_shift_n = n, ##Number of same shift offsets used
       diff_shift_n = m, ##Number of different shift offsets used
       U = obs_U, ##observed U-statistic
       p_value = pval, ##Corresponding p-value
       same_shift = my_same_shift$U,
       diff_shift = my_diff_shift$U,
       locations = max_corr_opt_loc)
}



chumbley_non_random2 <- function(data1, data2, window_opt = 500, window_val = 50, coarse = .25){
  
  unity <- function(x) {x / sqrt(sum(x^2))} ## normalize columns of a matrix to make correlation computation faster
  
  ####################################################
  ##Clean the marks and compute the smooth residuals##
  ####################################################
  
  data1 <- matrix(data1[round((0.01*nrow(data1))):round(0.99*nrow(data1)),], ncol = 1)
  data2 <- matrix(data2[round((0.01*nrow(data2))):round(0.99*nrow(data2)),], ncol = 1)
  
  ##Normalize the tool marks
  if (!is.null(coarse)) {
    y1 <- data1 - lowess(y = data1,  x = 1:nrow(data1), f= coarse)$y
    y2 <- data2 - lowess(y = data2,  x = 1:nrow(data2), f= coarse)$y
  } else {
    if (any(is.na(data1)) | any(is.na(data2))) browser()
    
    y1 = data1
    y2 = data2
  }
  
  
  ############################################
  ##Compute the observed maximum correlation##
  ############################################
  
  #####################
  ##Optimization step##
  #####################
  ##Each column in these matrices corresponds to a window in the respective tool mark
  y1_mat_opt <- matrix(NA, ncol = length(1:(length(y1) - (window_opt - 1))), nrow = window_opt)
  for(l in 1:(length(y1) - (window_opt - 1))){
    y1_mat_opt[,l] <- y1[l:(l+(window_opt - 1))]
  }
  y2_mat_opt <- matrix(NA, ncol = length(1:(length(y2) - (window_opt - 1))), nrow = window_opt)
  for(l in 1:(length(y2) - (window_opt - 1))){
    y2_mat_opt[,l] <- y2[l:(l+(window_opt - 1))]
  }
  
  ##Compute the correlation between all pairs of windows for the two marks
  ##Rows in the following matrix are mark 2, columns are mark 1
  y2_mat_opt <- apply(scale(y2_mat_opt), 2, unity)
  y1_mat_opt <- apply(scale(y1_mat_opt), 2, unity)
  corr_mat_opt <- t(y2_mat_opt) %*% y1_mat_opt ##correlation matrix
  max_corr_opt_loc <- which(corr_mat_opt == max(corr_mat_opt, na.rm=TRUE), arr.ind = TRUE) ##pair of windows maximizing the correlation
  
  
  ###################
  ##Validation step##
  ###################
  ##Each column in these matrices corresponds to a window in the respective tool mark
  y1_mat_val <- matrix(NA, ncol = length(1:(length(y1) - (window_val - 1))), nrow = window_val)
  for(l in 1:(length(y1) - (window_val - 1))){
    y1_mat_val[,l] <- y1[l:(l+(window_val - 1))]
  }
  y2_mat_val <- matrix(NA, ncol = length(1:(length(y2) - (window_val - 1))), nrow = window_val)
  for(l in 1:(length(y2) - (window_val - 1))){
    y2_mat_val[,l] <- y2[l:(l+(window_val - 1))]
  }
  
  ##Compute the correlation between all pairs of windows for the two marks
  ##Rows in the following matrix are mark 2, columns are mark 1
  y2_mat_val <- apply(scale(y2_mat_val), 2, unity)
  y1_mat_val <- apply(scale(y1_mat_val), 2, unity)
  corr_mat_val <- t(y2_mat_val) %*% y1_mat_val
  
  ##Pull out the correlations that correspond to windows with the same offset as the largest correlation found in the optimization step
  same_shift <- data.frame(row = NA, col = NA, U = NA)
  rows <- max_corr_opt_loc[1] + (window_opt - window_val)
  cols <- max_corr_opt_loc[2] + (window_opt - window_val)
  while(rows + window_val < nrow(corr_mat_val) & cols + window_val < ncol(corr_mat_val)){
    
    rows <- rows + window_val
    cols <- cols + window_val
    same_shift <- rbind(same_shift, c(rows, cols, corr_mat_val[rows,cols]))
    
  }
  rows <- max_corr_opt_loc[1]
  cols <- max_corr_opt_loc[2]
  while(rows - window_val > 0 & cols - window_val > 0){
    
    rows <- rows - window_val
    cols <- cols - window_val
    same_shift <- rbind(same_shift, c(rows, cols, corr_mat_val[rows, cols]))
    
  }
  same_shift <- same_shift[-1,]
  
  ##Pull out the correlations that correspond to windows with different offset as the largest correlation found in the optimization step
  ##along a single anti-diagonal
  diff_shift <- data.frame(row = NA, col = NA, U = NA)
  rows <- max_corr_opt_loc[1] + (window_opt - window_val)
  cols <- max_corr_opt_loc[2]
  while(rows + window_val < nrow(corr_mat_val) & cols - window_val > 0){
    
    rows <- rows + window_val
    cols <- cols - window_val
    diff_shift <- rbind(diff_shift, c(rows, cols, corr_mat_val[rows,cols]))
    
  }
  rows <- max_corr_opt_loc[1]
  cols <- max_corr_opt_loc[2] + (window_opt - window_val)
  while(rows - window_val > 0 & cols + window_val < ncol(corr_mat_val)){
    
    rows <- rows - window_val
    cols <- cols + window_val
    diff_shift <- rbind(diff_shift, c(rows, cols, corr_mat_val[rows, cols]))
    
  }
  diff_shift <- diff_shift[-1,]
  
  ######################################
  ##Compute the Ustatistic if possible##
  ######################################
  if(nrow(same_shift) == 0 | nrow(diff_shift) == 0) {
    
    obs_U <- NA
    n <- length(same_shift$U)
    m <- length(diff_shift$U)
    
  }
  
  if(nrow(same_shift) != 0 & nrow(diff_shift) != 0) {
    
    ranks <- rank(c(same_shift$U, diff_shift$U))
    Rx <- ranks[seq_along(same_shift$U)]
    Ry <- ranks[-(1:length(Rx))]
    n <- length(same_shift$U)
    m <- length(diff_shift$U)
    N <- n + m
    
    t <- sum(Rx) ##Test statistic...sum of sample one ranks
    t1 <- (t - n*((N + 1) / 2)) / sqrt( ((n*m)/(N*(N-1)))*sum(c(Rx^2, Ry^2)) - ((n*m*(N+1)^2) / (4*(N-1)))) ##Standardized test statistics
    obs_U <- t1
    
  }
  pval <- 1 - pnorm(obs_U)
  
  list(same_shift_n = n, ##Number of same shift offsets used
       diff_shift_n = m, ##Number of different shift offsets used
       U = obs_U, ##observed U-statistic
       p_value = pval, ##Corresponding p-value
       same_shift = same_shift$U,
       diff_shift = diff_shift$U,
       locations = max_corr_opt_loc)
}


Transco <-  function(x,y,ymin,ymax){
  
  if (y>x){
    a=y-x
    b=ymax-x
    p=round(a/b,3)
    
  }
  else if (y<x){
    a=x-y
    p=round((-a/x),3)
    
  }
  else{
    p=0
  }
  return(p)
}



#plots accuracy values of a dataframe in boxplot to compare accuracy between ROIS for example
Var_MachLning <- function(DF_Res,n,roi= "ROI", tool = "tool"){
  
  Res_F <- WB_var(na.omit(DF_Res))
  Res_T <- dplyr::select(Res_F, corr, rdist, CCF2,Wcorr, Wcorr2, Wcorr3)
  
  AccVal <- c()
  for (i in 1:n){
    
    index <- createDataPartition(y=Res_F$Var,p=0.7,list=FALSE)
    train2 <- dplyr::select(Res_F, corr,  rdist, CCF2,Wcorr, Wcorr2, Wcorr3, Var) 
    train <- train2[index,]
    test <-  train2[-index,]
    
    #set.seed(355)
    classCtrl <- trainControl(method = "repeatedcv", number=10,repeats=5) 
    
    ldaTL <- train(Var~., train, method= "glmnet", trControl=classCtrl)
    
    fittedTL <- predict(ldaTL,test)
    CM <- confusionMatrix(reference=as.factor(test$Var),data=fittedTL,mode = "everything")
    Acc <- CM$overall[[1]]
    AccVal[i] <- Acc
  }
  Mean <- mean(AccVal)
  SD <- sd(AccVal)
  
  DF <- as.data.frame(AccVal)
  DF$ROI <- roi

  
 p <-  ggplot(DF, aes(x=ROI, y=AccVal))+
    geom_boxplot()+
    coord_fixed(ylim=c(0.87,1)) +
    labs(title = paste("mean = ", round(Mean,3), " SD = ", round(SD,3), " for", tool, " ", roi))
  
  
  
 # Result <- c(Mean,SD)
  
  return(p)
  
}
#boxplot avec possibilité de changer la méthode de machine learning
Best_MachLning <- function(DF_Res,n, Method = "lda"){
  
  Res_F <- WB_var(na.omit(DF_Res))
  Res_T <- dplyr::select(Res_F, corr, rdist, CCF2,Wcorr, Wcorr2, Wcorr3)
  
  AccVal <- c()
  for (i in 1:n){
    
    index <- createDataPartition(y=Res_F$Var,p=0.7,list=FALSE)
    train2 <- dplyr::select(Res_F, corr,  rdist, CCF2,Wcorr, Wcorr2, Wcorr3, Var) 
    train <- train2[index,]
    test <-  train2[-index,]
    
    #set.seed(355)
    classCtrl <- trainControl(method = "repeatedcv", number=10,repeats=5) 
    grid_svm <- expand.grid(C=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), sigma=c(0.25,0.5,0.675,0.75,1))
    TL <- train(Var~., train, method= Method, trControl=classCtrl,tunGrid=grid_svm)
    Acc <- max(TL[["results"]][["Accuracy"]])
    
    AccVal[i] <- Acc
  }
  Mean <- mean(AccVal)
  SD <- sd(AccVal)
  
  DF <- as.data.frame(AccVal)
  DF$method <- Method
  
  p <-  ggplot(DF, aes(x=method, y=AccVal))+
    geom_boxplot()+
    labs(title = paste("mean = ", round(Mean,3), " SD = ", round(SD,3), " for " ,  Method))
}

#retrives the scores from fittedprob (predict) and identifies which comparison were correctly classified adding a 1 or a 0
Miss <- function(DF){
  DF$Miss <- 2
  
  for (i in 1:nrow(DF)){
    
    if ((DF$Var[[i]] == "B") & (DF$B[[i]] > 0.5)){
      
      DF$Miss[[i]] <- 1 #true negative
    }
    if ((DF$Var[[i]] == "B") & (DF$B[[i]] < 0.5)){
      
      DF$Miss[[i]] <- 0 #false positive
    }
    if ((DF$Var[[i]] == "W") & (DF$B[[i]] > 0.5)){
      
      DF$Miss[[i]] <- 0 #false negative
    }
    if ((DF$Var[[i]] == "W") & (DF$B[[i]] < 0.5)){
      
      DF$Miss[[i]] <- 1 #true positive
    }
  }
  return(DF)
}


#computes machine learning for a result of aligned scores and binds scores with probabilities
ML <- function(Res){
  Res_F <- WB_var(na.omit(Res[[2]]))
  Res_T <- dplyr::select(Res_F, corr, rdist, CCF2,Wcorr, Wcorr2, Wcorr3)
  set.seed(10) #index is always the same, useful when computing for different ROIs
  index <- createDataPartition(y=Res_F$Var,p=0.7,list=FALSE)
  train2 <- dplyr::select(Res_F, corr,  rdist, CCF2,Wcorr, Wcorr2, Wcorr3, Var) 
  train <- train2[index,]
  test <-  train2[-index,]
  
  MySummary  <- function(data, lev = NULL, model = NULL){
    a1 <- defaultSummary(data, lev, model)
    b1 <- twoClassSummary(data, lev, model)
    c1 <- prSummary(data, lev, model)
    out <- c(a1, b1, c1)
    out}
  
  classCtrl <- trainControl(method = "repeatedcv", number=10,repeats=5,classProbs =  TRUE,summaryFunction = MySummary,savePredictions = "final")
  set.seed(355)
  ROITL <- train(Var~., train, method= "glmnet", metric= "Spec",  trControl=classCtrl)
  
  fittedROITL <- predict(ROITL,test)
  
  CM_ROI <- confusionMatrix(reference=as.factor(test$Var),data=fittedROITL,mode = "everything") 
  
  fittedProb_ROI <- as.data.frame(predict(ROITL,test,type= "prob")) #gets the probability values for classification
  
  
  DF <- as.data.frame(cbind(test,fittedProb_ROI))
  
 return(DF) 
}

#gives which row is a good or bad classification and takes all ROIs into account
MissClass <- function(ResROI1,ResROI2,ResROI3){
  DF1 <- ML(ResROI1)
  DF2 <- ML(ResROI2)
  DF3 <- ML(ResROI3)
  
  
  DF_ROI1 <- Miss(DF1)
  DF_ROI2 <- Miss(DF2)
  DF_ROI3 <- Miss(DF3)
  
  
  Miss_tot <- as.data.frame(cbind(DF_ROI1$Var,DF_ROI1$Miss,DF_ROI1$B,DF_ROI1$W,
                                  DF_ROI2$Miss,DF_ROI2$B,DF_ROI2$W,
                                  DF_ROI3$Miss,DF_ROI3$B,DF_ROI3$W))
  
  
 
  DFF <- Comb_fit(Miss_tot)
  colnames(DFF) <- c("Class","ROI1","B_ROI1", "W_ROI1",
                     "ROI2","B_ROI2", "W_ROI2",
                     "ROI3","B_ROI3", "W_ROI3",
                     "ROIs","B_ROIs", "W_ROIs")
  
  DFF$B_ROI1 <- as.numeric(DFF$B_ROI1)
  DFF$W_ROI1 <- as.numeric(DFF$W_ROI1)
  DFF$B_ROI2 <- as.numeric(DFF$B_ROI2)
  DFF$W_ROI2 <- as.numeric(DFF$W_ROI2)
  DFF$B_ROI3 <- as.numeric(DFF$B_ROI3)
  DFF$W_ROI3 <- as.numeric(DFF$W_ROI3)
  DFF$B_ROIs <- as.numeric(DFF$B_ROIs)
  DFF$W_ROIs <- as.numeric(DFF$W_ROIs)
  
  return(DFF)
}
# combines ROIs to increase metric values as it takes good classifications in ROIs.
Comb_fit <- function(ResMissClass){
  dff <- ResMissClass
  dff$tot <- 2
  dff$B <- "b"
  dff$W <- "w"
  for (i in 1: nrow(dff)){
    if (dff$V2[[i]]==1){
      dff$tot[[i]] <- 1
      dff$B[[i]] <- dff$V3[[i]]
      dff$W[[i]] <- dff$V4[[i]]
    }
    if (dff$V2[[i]]==0 & dff$V5[[i]]==1){
      dff$tot[[i]] <- 1
      dff$B[[i]] <- dff$V6[[i]]
      dff$W[[i]] <- dff$V7[[i]]
    }
    if ((dff$V2[[i]]==0) & (dff$V5[[i]]==0)& (dff$V8[[i]]==1)){
      dff$tot[[i]] <- 0
      dff$B[[i]] <- dff$V9[[i]]
      dff$W[[i]] <- dff$V10[[i]]
    }
    if ((dff$V2[[i]]==0) & (dff$V5[[i]]==0)& (dff$V8[[i]]==0)){
      dff$tot[[i]] <- 0
      dff$B[[i]] <- dff$V3[[i]]
      dff$W[[i]] <- dff$V4[[i]]
    }
  }
  return(dff)
}

#binds CM_Val results to compare CMs and metric values when ROIs are combined 
CM_hand <- function(ResMissClass){
  DF <- ResMissClass
  
  Original <- CM_Val(DF,col="ROI1") 
  Combined <- CM_Val(DF,col="ROIs")
  
  CM <- cbind(Original[[1]],Combined[[1]])
  colnames(CM) <- c("B ROI1", "W ROI1", "B ROIs", "W ROIs")
  rownames(CM) <- c("B","W")
  
  Values <- rbind(Original[[2]],Combined[[2]])
  rownames(Values) <- c("Single ROI", "Combined")
  
  
  return(list(CM,Values))
}

#creates a CM for a single column of probabilities and gives the metric values
CM_Val <- function(DF,col= "ROI1"){
  DF$CM <- "c"
  for (i in 1:nrow(DF)){
    if (DF$Class[[i]]== "B"& DF[col][[1]][[i]]==1){
      DF$CM[[i]] <- 1 #true negative up-left
    }
    if (DF$Class[[i]]== "B"& DF[col][[1]][[i]]==0){
      DF$CM[[i]] <- 2 #false positive up-right
    }
    if (DF$Class[[i]]== "W"& DF[col][[1]][[i]]==0){
      DF$CM[[i]] <- 3 #false negative down-left
    }
    if (DF$Class[[i]]== "W"& DF[col][[1]][[i]]==1){
      DF$CM[[i]] <- 4 #true positive down-right
    }
  }
  res <-  data.frame(matrix(0,nrow=2,ncol=2))
  x <- as.data.frame(table(DF['CM'])) #compte le nombre de vrai positifs et négatifs
  y <- as.data.frame(table(DF['Class'])) #compte le nombre de B et W
  
  one <- x[x$Var1==1,]
  four <- x[x$Var1==4,]
  
  TN <- one$Freq[[1]]
  FP <- y$Freq[[1]]-one$Freq[[1]]
  FN <- y$Freq[[2]]-four$Freq[[1]]
  TP <- four$Freq[[1]]
  Total=TP+TN+FN+FP
  
  res[1,1] <- TN
  res[2,1] <- FP
  res[1,2] <- FN
  res[2,2] <- TP
  
  Accuracy <- round(((TP+TN)/Total),3)
  ErrorRate <- round(((FP+FN)/Total)*100,3)
  Sensitivity <- round((TN/(TN+FP)),3)
  Specificity <- round((TP/(TP+FN)),3)
  PosPred <- round((TN/(TN+FN)),3)
  NegPred <- round((TP/(FP+TP)),3)
  
  #Title <- c("Accuracy", "Errorrate","Sensitivity", "Specificity", "PosPred", "NegPred")
  Values <- c(Accuracy, ErrorRate,Sensitivity,Specificity,PosPred,NegPred)
  Stat <- t(as.data.frame(Values))
  colnames(Stat) <- c("Accuracy", "Error %","Sensitivity", "Specificity", "PosPred", "NegPred")
  Stat <- as.data.frame(Stat)
  return(list(res,Stat))
}

#LR computation from scores taht are obtained with ResMissclass
LR_calcul <- function(ResMissclass){
  DF <- ResMissclass
  DF$lr <- NA
  Class <- table(DF$Class)
  ClassB <- Class['B'][[1]]
  ClassW <- Class['W'][[1]]
  for (i in 1:nrow(DF)){
    
    LR <- (DF$W_ROIs[[i]]/DF$B_ROIs[[i]])/(ClassW/ClassW+ClassB)
    DF$lr[[i]] <- LR
   
  }
  DF$h <- as.factor(ifelse( DF$Class == "W", "hp", "hd" ))
  
  DF_LR <- DF[,-c(1:13)]
  
  
  return(DF_LR)
  }