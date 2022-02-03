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
CCF_all <- function(Selection, table){
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
                                     res[i,3])
    allplots[[i]]$labels[2] <- ""
    allplots[[i]]$labels[3] <- "depth"
    
    }
  colnames(res) <- c("nameComp", "lag", "corr")
  return(list(allplots,res)) #returns a list of two elements, the first one has all the plots, sencond one is score dataframe
}

#To align profiles and compute other metrics using aligned profiles
Aligned_scores <- function(Selection, table){
  res <- CCF_all(Selection,table)[[2]]
  allplots <- CCF_all(Selection,table)[[1]]
  res$ndtw <- NA
  res$rdist <- NA
  #res$Uscore <- NA
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
    NDTW <- ndtw(a2,b2) #get ndtw score into DF
    Rdist <- reldist(a2,b2) #get relativedistance frome bachrach in DF
    res[i,4] <- NDTW
    res[i,5] <- Rdist
    
   # am <- as.matrix(a2)
    #bm <- as.matrix(b2)
   # cnr <- chumbley_non_random(am,bm,window_val = 25,coarse = 0.07) #get Chumbley U stat into DF
   # res[i,6] <- cnr$U
  }
  return(list(allplots,res))
}

#To retrieve a DF with cut aligned profiles, can be used for single computation of similarity measures
CutPDF <- function(Selection, table){
  nProfile <- nrow(table)
  Cut_P <-  data.frame(matrix(NA,nrow=nProfile,ncol=2)) #create Df for cut profiles
  colnames(Cut_P) <- c("a2", "b2")
  Lags <- CCF_all(Selection,table)[[2]]
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
      open.begin = TRUE)$normalizedDistance
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
  res1 <- Aligned_scores(Selection1,table)[[2]]
  res2 <- Aligned_scores(Selection2,table)[[2]]
  res3 <- Aligned_scores(Selection3,table)[[2]]
  
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

#computes the CCF and extracts the lag between two different tools
CCF_all_IP <- function(Selection_A,Selection_B, table){
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
Aligned_scores_IP <- function(Selection_A,Selection_B, table){
  res <- CCF_all_IP(Selection_A, Selection_B,table)[[2]]
  allplots <- CCF_all_IP(Selection_A,Selection_B, table)[[1]]
  res$ndtw <- NA
  res$rdist <- NA
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
    NDTW <- ndtw(a2,b2) #get ndtw score into DF
    Rdist <- reldist(a2,b2) #get relativedistance frome bachrach in DF
    res[i,4] <- NDTW
    res[i,5] <- Rdist
    
    # am <- as.matrix(a2)
    #bm <- as.matrix(b2)
    # cnr <- chumbley_non_random(am,bm,window_val = 25,coarse = 0.07) #get Chumbley U stat into DF
    # res[i,6] <- cnr$U
  }
  return(list(allplots,res))
}

#To retrieve the within and between variability score separatly
WB_var <- function(Resultat){
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

# This function plots within and between variability distributions for a single tool
Distrib <- function(Selection1, Selection2,Selection3,table){
  Res1 <- Aligned_scores(Selection1,table)[[2]]
  Res2 <- Aligned_scores(Selection2,table)[[2]]
  Res3 <- Aligned_scores(Selection3,table)[[2]]
  
  ROI1 <- WB_var(Res1) 
  ROI2 <- WB_var(Res2)
  ROI3 <- WB_var(Res3) 
  
  DFA <-rbind(ROI1,ROI2,ROI3)
  
  q1 <- ggplot(DFA,aes(x=corr,color=Var))+geom_density()
  q2 <- ggplot(DFA,aes(x=rdist,color=Var))+geom_density()
  q3 <- ggplot(DFA,aes(x=ndtw,color=Var))+geom_density()
  q4 <- ggplot(DFA,aes(x=corr,color=VarROI))+geom_density()
  q5 <- ggplot(DFA,aes(x=rdist,color=VarROI))+geom_density()
  q6 <- ggplot(DFA,aes(x=ndtw,color=VarROI))+geom_density()
  
  
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



Plot_HeatMap <- function(Res,table){ 
  Res <- Res[[2]]$corr
  HM_df <- cbind(table,Res)
  
 P <-  ggplot(data = HM_df, aes(V2, V1, fill = Res))+  
   geom_tile(color = "white", size=0.2)+
   scale_x_discrete(limits=factor(c(1:max(table$V2))))+
   scale_y_discrete(limits=factor(c(1:max(table$V2))))+
   scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                        midpoint = mean(Res),limit = c(min(Res),max(Res)), space = "Lab",
                        name="Scores") +
   theme(panel.grid = element_blank())+
   labs(title= "Heatmap scores", 
        x= "",
        y= "")
  
  return(P)
}

