## Set of packages and functions required.
## use source() to load them as required

#Packages

library(tidyverse)
library(gridExtra) #for grid.arrange()
library(toolmaRk)
library(dtw)

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

