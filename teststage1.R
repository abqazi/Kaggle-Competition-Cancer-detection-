df=data.frame()

sz=32

list_names = list.files(paste("C:/Users/Hannan Qazi/Documents/stage1",collapse = NULL,sep=''))

for (k in 1:length(list_names)){
  name = list_names[k]
  
  setwd = paste("C:/Users/Hannan Qazi/Documents/stage1/",name,sep = '')#Put directory of image folder of each set
  fileList=list.files(paste("C:/Users/Hannan Qazi/Documents/stage1/",name,sep = ''), recursive = TRUE) #Gives me the files names
  
  z=array(dim=c(length(fileList),sz,sz)) #decalre image array...
  
  #header=array(dim=c(length(fileList),42))
  
  #par(mai=c(.05,.05,.05,.05)) #reset margins to make all pictures
  
  #par(mfrow=c(10,14))
  
  #Collapse the data and
  for (i in 1:length(fileList)){
    a=paste(setwd, fileList[i],sep="/")
    mydata=readDICOMFile(a)
    y=resize(t(mydata$img), w=sz,h=sz)
    #image(y,col=gray(0:255/256), axes=FALSE, xlab="", ylab="")
    z[i,,]=imageData(y)
    #h=as.vector(mydata$hdr$value)
    #header[i,]=h
    #names(header[i,])=mydata$hdr$name
    rm(mydata)
    rm(x)
    rm(y)
    #rm(h)
  }
  
  p1=as.data.frame(z)
  #p2=as.data.frame(header)
  #pic1=cbind(p1,p2)
  mypca=prcomp(p1, scale=FALSE, retx=TRUE)
  summary(mypca)
  screeplot(mypca)
  predictors=as.vector(mypca$x[,1:35])
  actual=0
  output = mypca$x[,1:35]
  combi = cbind(name, output)
  df=rbind(df,combi)
}

#Run this only for train set
#add "cancer" column to the "df" dataframe
df["cancer"] <- NA
colnames(df)[1] <- "id"
#import "stage1_labels" with solutions
stagelabels = read.csv("C:/Users/Hannan Qazi/Documents/stage1_labels.csv")
#Create join
m = merge(df,stagelabels, by='id')
colnames(m)[length(m)] <- "cancer"
m = m[,c(1:(length(m)-2),length(m))]
#export csv of previous output file


#write.csv(mypca$x[,1:40],"C:/Users/mfdo11/Desktop/Predictive Analytics/pcas.csv")#Choose directory where to output .csv file with principal components
write.csv(df,"C:/Users/Hannan Qazi/Desktop/trainstage1.csv", row.names = FALSE)#Choose directory where to output .csv file with principal components

