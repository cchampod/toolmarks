

df <- data.frame(K=c(1,2),Z=c(1,2), A=c(1,2), N=c(1,2))
df$x <- list(c(1,2,3,4), c(5,6,7,8))
df$y <- list(c(1,2,3,4), c(5,6,7,8))

path <- "~/Documents/Thèses Doctorants/Thèse Jean-Alexandre Patteet/AllProfiles/"

K1Z1A1N01L1 <- read_delim(paste0(path,"K1Z1A1N01L1.txt"), 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)

df$x[1] <- list(K1Z1A1N01L1$X1)
df$y[1] <- list(K1Z1A1N01L1$X2)