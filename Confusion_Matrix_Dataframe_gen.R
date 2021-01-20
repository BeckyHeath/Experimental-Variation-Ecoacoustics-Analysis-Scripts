#########################################################
# Script to Generate a DataFrame from Confusion Matricies
# (for graphing)
#
# Becky Heath Autumn 2020 
# r.heath18@imperial.ac.uk
#


### load packages and set working Directory ####
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### load confusion matrix data ####
files <- list.files(path ="Confusion_Matricies/")

#Perform Data Extraction
out.file<-data.frame()
for (x in files){
  path <- paste("Confusion_Matricies/",x, sep="")
  cm <- read.csv(path, header = FALSE)
  meta <- str_split(x, "_")
  Ind <- meta[[1]][[2]]
  if (Ind == "Analytical"){                  ########## Analytical Indices 
    frame.size <- meta[[1]][[4]]
    
    if (frame.size == 2){                    # Fix 2.5 min problem
      frame.size <- paste(meta[[1]][4:5], collapse= ".")
      Comp <- meta[[1]][[6]]
      chunks <- meta[[1]][[7]]
      Time <- meta[[1]][[8]]
      if (chunks == 4){                     # Sort the Quarter Day Thing 
        TM <- meta[[1]][8:10]
        Time <- paste(TM, collapse =' ')
        }
        
      } else {                     #So if there's no issue with the 2.5 
        Comp <- meta[[1]][[5]]
        chunks <- meta[[1]][[6]]
        Time <- meta[[1]][[7]]
        if (chunks == 4){           # Again the quarter day thing
          TM <- meta[[1]][7:9]
          Time <- paste(TM, collapse =' ')
          }
      }
      
      
    } else {                    ######### AudioSet Here
    frame.size <- meta[[1]][[3]]
    if (frame.size == 2){                 # Sorting the 2.5 thing
      frame.size <- paste(meta[[1]][3:4], collapse= ".")
      Comp <- meta[[1]][[5]]
      Time <- meta[[1]][[7]]
      chunks <- meta[[1]][[6]]
      if (chunks == 4){             # Sorting the Quarter day thing 
        TM <- meta[[1]][7:9] 
        Time <- paste(TM, collapse =' ')
      }
      
      
      } else {
        Comp <- meta[[1]][[4]]
        print(meta)
        print(Comp)
        Time <- meta[[1]][[6]]
        chunks <- meta[[1]][[5]]
        if (chunks == 4){
          TM <- meta[[1]][6:8] 
          Time <- paste(TM, collapse =' ')
    }
    }
  }
  ## Write Out.Line (9 per File)
  #01 Site 1 Guessed 1
  Obs <- cm[1,1]
  Pred <- Obs
  N <- cm[1,2]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #02 Site 1 Guessed 2
  Obs <- cm[1,1]
  Pred <- cm[2,1]
  N <- cm[1,3]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #03 Site 1 Guessed 3
  Obs <- cm[1,1]
  Pred <- cm[3,1]
  N <- cm[1,4]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #04 Site 2 Guessed 1
  Obs <- cm[2,1]
  Pred <- cm[1,1]
  N <- cm[2,2]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #05 Site 2 Guessed 2
  Obs <- cm[2,1]
  Pred <- Obs
  N <- cm[2,3]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #06 Site 2 Guessed 3
  Obs <- cm[2,1]
  Pred <- cm[3,1]
  N <- cm[2,4]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #07 Site 3 Guessed 1
  Obs <- cm[3,1]
  Pred <- cm[1,1]
  N <- cm[3,2]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #08 Site 3 Guessed 2
  Obs <- cm[3,1]
  Pred <- cm[2,1]
  N <- cm[3,3]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  #09 Site 3 Guessed 3
  Obs <- cm[3,1]
  Pred <- Obs
  N <- cm[3,4]
  out.line <- data.frame(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)
  out.file <-rbind(out.file,out.line)
  rm(Obs,Ind,Comp,Pred,Time,N,chunks,frame.size)                           ### CLEAR VARIABLES
  }
}

write.csv(out.file, "Complete_Confusion_Matrix_data.csv", row.names = FALSE)
