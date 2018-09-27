#PondR Version 2

##Author: Henry Hershey
##Date: 2018-09-27
#add a relative weight column to a dataset

add.Wr = function(x,do.plot=F){
  Wr <- NA
  log.TL <- log10(x$tl)
  x <- cbind(x,log.TL)
  for (i in 1:nrow(x)){
    if (x$sp[i] %in% c("LMB","lmb")) {
      
      exp. <- -5.528+3.273*x$log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
      
    } else if (x$sp[i] %in% c("BG","BLG", "BG","bg","blue")) {
      exp. <- -5.374+3.316*x$log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
      
    } else if (x$sp[i] %in% c("RE","re","RES","res","resu")) {
      exp. <- -4.968+3.119*log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
    }
    
  }
  
  if(do.plot==T){
        x<-cbind(x,Wr)
        spec <- unique(x$sp)
         for (i in 1:length(spec)){
           plot(x$Wr[x$sp==spec[i]]~ x$tl[x$sp==spec[i]], ylab = "Relative Weight", xlab = "Total Length (in)", main = spec[i])
           abline(90,0, lty= 3, col= "green")[i]
           abline(80,0, lty= 3, col= "red")[i]
           abline(100,0, lty = 3, col = "blue")[i]
         } 
        x[,c("sp","tl","wt","Wr")]
  } else cbind(x,Wr)[,c("sp","tl","wt","Wr")]
 
}

#calculate proportional size distributions for each species and put it in a dataframe

PSD = function(x, type=c("quality","preferred","memorable","trophy"),do.plot=F){
  if (type=="quality"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RE=NA, row.names = "PSD")
        PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= 300)/sum(x$tl[x$sp %in% c("LMB","lmb")] >= 200)
        PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 150)/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 80)
        PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 180)/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 100)
        PSDtable
  }else if (type=="preferred"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RE=NA, row.names = "PSD")
    PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= 380)/sum(x$tl[x$sp %in% c("LMB","lmb")] >= 200)
    PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 200)/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 80)
    PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 230)/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 100)
    PSDtable
    
  } else if (type=="memorable"){
      PSDtable <- data.frame(LMB=NA, BG=NA, RE=NA, row.names = "PSD")
      PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= 510)/sum(x$tl[x$sp %in% c("LMB","lmb")] >= 200)
      PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 250)/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 80)
      PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 280)/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 100)
      PSDtable
      
  }else if (type=="trophy"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RE=NA, row.names = "PSD")
    PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= 630)/sum(x$tl[x$sp %in% c("LMB","lmb")] >= 200)
    PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 300)/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= 80)
    PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 330)/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 100)
    PSDtable
    
  }

  if(do.plot==T){
    
    plot(PSDtable[1,1]~PSDtable[1,2], xlim=c(0,100),ylim=c(0,100), 
         xlab = "Bluegill PSD", ylab = "Largemouth Bass PSD", main = "Pond Balance", pch=17, cex=1.5, col="red")
    abline(40,0, lty = 3)
    abline(70,0, lty = 3)
    abline(v=40, lty =3)
    abline(v=20, lty=3)
    text(60,20, "Bass Crowded")
    text(8,90, "Bluegill \nCrowded")
    text(10,20, "Unfertile")
    text(30,55, "Balanced")
  }
  return(PSDtable)
}

