#Midterm Exam : Test (stage 2)

df=data.frame()
# Atempted to try sample images with different sizes of 64, 512 and 32
#sz=64
#sz=512
sz=32

#reading all names for patients
patient.names = list.files(paste("C:/Users/Hannan Qazi/Documents/stage2",sep='')) 

for (k in 1:length(patient.names)){
  name = patient.names[k]
  
  #setting directory for images within each patient
  setwd = paste("C:/Users/Hannan Qazi/Documents/stage2/",name,sep = '')
  
  #reading all images for each patient by setting recursive condition true
  List=list.files(paste("C:/Users/Hannan Qazi/Documents/stage2/",name,sep = ''), recursive = TRUE) 
  
  # image array creation 
  z=array(dim=c(length(fileList),sz,sz)) 
  
  # nested loop to obtain image data by reading them using readDICOMFile 
  for (i in 1:length(List)){
    a=paste(setwd, fileList[i],sep="")
    mydata=readDICOMFile(a)
    y=resize(t(mydata$img), w=sz,h=sz)  # resizing or changing pixel size of image 

    z[i,,]=imageData(y)
    
    rm(mydata)  #removing data for each image so as not to overload memory as it goes over all images
    rm(x)
    rm(y)
    #
  }
  
  
  p1=as.data.frame(z)
  
  
  mypca=prcomp(p1, retx=TRUE)  #scaling wouldnt work, default is false 
  #summary(mypca)
  #screeplot(mypca)
  
  #we chose 35 PCA's since they accounted for 97% of variation
  
  predictors=as.vector(mypca$x[,1:35]) 
  actual=0
  
  
  result = mypca$x[,1:35]
  
  # combinig columns of name for each image with their PCA's
  idnpca = cbind(name, result)
  
  #combining all image rows into a data frame
  df=rbind(df,idnpca)
}





write.csv(df,"C:/Users/Hannan Qazi/Desktop/tstage2.csv", row.names = FALSE)
