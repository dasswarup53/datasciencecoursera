pollutantmean<-function(directory,pollutant,id){
  monitor_count<-0
  mean<-0
  for(x in id){
    file<-paste(formatC(x,width = 3,flag="0"),"csv",sep=".")
    data<-read.csv(file)
    datana.omit(data)
    if(pollutant=="sulfate")
    {monitor_count<-monitor_count+1
      mean<-mean+colMeans(data[,2,drop=FALSE],na.rm=TRUE)
    }
    if(pollutant=="nitrate")
    {monitor_count<-monitor_count+1
      mean<-mean+colMeans(data[,3,drop=FALSE],na.rm=TRUE)
    }
  }
  print("mean is as follows :")
  print(mean/monitor_count)
}
complete <- function(directory, id = 1:332) {
  count_complete <- function(fname) sum(complete.cases(read.csv(fname)))
  fnames <- list.files(directory, full.names=TRUE)[id]
  data.frame(id = id, complete = unlist(lapply(fnames, count_complete)))
}
corr <- function(directory, threshold = 0) {
  tcorr <- function(fname) {
    data <- read.csv(file.path(directory, fname))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      return (cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  tcorrs <- sapply(list.files(directory), tcorr) 
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)])
  return (tcorrs)
}
