## Set of packages and functions required.
## use source() to load them as required

#Packages

library(tidyverse)
library(gridExtra) #for grid.arrange()
library(toolmaRk)
library(dtw)
library(proxy)
library(dtwclust)
library(TSclust)
library(ggdendro)
library(dendextend)
library(warbleR) #pour certaines correlations
library(correlation)

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
    print(i)
    print(files[i])
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
CCF <- function(a,b, ratio=3){
  cv <- ccf(a,b, lag.max = round(length(a)/ratio,0))
  sco <- cv$acf
  lag <- cv$lag
  res <-  data.frame(sco,lag)
  lag_max <-  res[which.max(res$sco),]$lag
  sco_max <- max(sco)
  output <- c(lag_max,sco_max)
  return(output)
}

#To compute CCF scores in batches creating a dataframe of all scores and lags
CCF_all <- function(Selection, table){
  nProfile <- nrow(table)
  res <-  data.frame(matrix(NA,nrow=nProfile,ncol=3))
  for(i in 1:nProfile) {
    a <- unlist(Selection$y[table[i,1]])
    b <- unlist(Selection$y[table[i,2]])
    nameComp <- paste(Selection$File[table[i,1]],"vs.",Selection$File[table[i,2]])
    ccf <- CCF(a, b)
    res[i,1] <- nameComp
    res[i,2:3] <- ccf
  }
  colnames(res) <- c("nameComp", "lag", "corr")
  return(res)
}

#To align profiles and compute other metrics using aligned profiles
Aligned_scores <- function(Selection, table){
  res <- CCF_all(Selection,table)
  res$ndtw <- NA
  res$rdist <- NA
  res$Uscore <- NA
  nProfile <- nrow(table) 
  
  for(i in 1:nProfile){
    a <- unlist(Selection$y[table[i,1]])
    b <- unlist(Selection$y[table[i,2]])
    Lag <- res$lag[i]  #ici ce sera test 
    if (Lag<0) {
      a2 <- head(a,-abs(Lag))
      b2 <- tail(b,-abs(Lag))
    }
    else {
      a2 <- tail(a,-Lag)
      b2 <- head(b,-Lag)
    }
    NDTW <- ndtw(a2,b2) #get ndtw score into DF
    Rdist <- reldist(a2,b2) #get relativedistance frome bachrach in DF
    res[i,4] <- NDTW
    res[i,5] <- Rdist
    
    am <- as.matrix(a2)
    bm <- as.matrix(b2)
    cnr <- chumbley_non_random(am,bm,window_val = 25,coarse = 0.07) #get Chumbley U stat into DF
    res[i,6] <- cnr$U
  }
  return(res)
}

#To compute a DF with cut aligned profiles, can be used for single computation of similarity measures
CutPDF <- function(Selection, table){
  nProfile <- nrow(table)
  Cut_P <-  data.frame(matrix(NA,nrow=nProfile,ncol=2)) #create Df for cut profiles
  colnames(Cut_P) <- c("a2", "b2")
  Lags <- CCF_all(Selection,table)
  for(i in 1:nProfile){
    a <- unlist(Selection$y[table[i,1]])
    b <- unlist(Selection$y[table[i,2]])
    Lag <- Lags$lag[i]  
    if (Lag<0) {
      a2 <- head(a,-abs(Lag))
      b2 <- tail(b,-abs(Lag))
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
      open.begin = TRUE,
      distance.only = TRUE)$normalizedDistance
}

#To compute ndtw separately on the batch with aligned profiles
batch_ndtw <- function(Selection,table){
  nProfile <- nrow(table)
  NDTW <- data.frame(matrix(NA,nrow=nProfile,ncol=1))
  colnames(NDTW) <- c("Ndtw")
  Cut_P <- CutPDF(Selection,table)
  for(i in 1:nProfile){
    a2i <- unlist(Cut_P$a2[i])
    b2i <- unlist(Cut_P$b2[i])
    score <- ndtw(a2i,b2i)
    NDTW$Ndtw[i] <- score
  }
  return(NDTW)
}

#To compute reldist separately on the batch with aligned profiles
batch_reldist <- function(Selection,table){
  nProfile <- nrow(table)
  rdist <- data.frame(matrix(NA,nrow=nProfile,ncol=1))
  colnames(rdist) <- c("Rdist")
  Cut_P <- CutPDF(Selection,table)
  for(i in 1:nProfile){
    a2i <- unlist(Cut_P$a2[i])
    b2i <- unlist(Cut_P$b2[i])
    score <- reldist(a2i,b2i)
    rdist$Rdist[i] <- score
  }
  return(rdist)
}


#To compute Aligned_scores for multiple Selection like different ROIs and put it in one dataframe
Comb_ROI <- function(Selection1, Selection2, Selection3,table){
  res1 <- Aligned_scores(Selection1,table)
  res2 <- Aligned_scores(Selection2,table)
  res3 <- Aligned_scores(Selection3,table)
  
  ResTot <- bind_rows(rowid_to_column(res1),
                      rowid_to_column(res2),
                      rowid_to_column(res3)) %>% 
    nest(nest_data = -rowid) %>% 
    unnest_wider(nest_data) %>% 
    select(-rowid)
  
  return(ResTot)
}







