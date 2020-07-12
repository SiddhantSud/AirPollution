my_getData<-function(directory){
  Dlocation<- paste0(getwd(),"/",directory)
  flist<-list.files(path=Dlocation,pattern=NULL,all.files=FALSE,full.names=FALSE)
  data<-data.frame()
  for(i in flist){
    dat<-read.csv(paste(Dlocation,"/",i,sep=""))
    data<-rbind(data,dat)
  }
  data
}

my_getData <- function(directory, id = 1:332) {
  path <- paste(getwd(),"/", directory,sep="")
  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      dat <- read.csv(paste0(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else if (i < 100) {
      dat <- read.csv(paste0(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else {
      dat <- read.csv(paste0(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
  }
  data
}

my_complete<-function(direc,id=1:332){

  data<-data.frame()
  for (i in id){
    temp<-getData(directory = direc,id=i)
    nfull<-sum(complete.cases(temp))
    data9<-data.frame(i,nfull)
    data<-rbind(data,data9)
  }
  data
}
my_corr<-function(dir,threshold=0){
  corr_vect <- NULL
  data<-data.frame()
  for (i in 1:332){
    temp<-getData(directory = dir,id=i)
    data <- temp[complete.cases(temp),]
    if (nrow(data) > threshold) {
      corr_vect <- c(corr_vect, cor(data[,"sulfate"], data[, "nitrate"]))
    }
  }
  return(corr_vect)
}