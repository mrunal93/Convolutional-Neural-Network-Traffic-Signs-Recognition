#read files
df<-read.csv(file.choose())
#SET WORKING directory
#to get current working directory
getwd()

#to change wd(working directory)
#click section set working directory then choose wd

#data cleaning : structural cleaning

str(df)

#remove id 
df$Id<-NULL # IT REMOVE 'ID'
#summary of data
summary(df) # for factor it onlt give count and level gives number of level and name
levels(df$Zoning_Class)

levels(df$Neighborhood) 
summary(df$Overall_Material)
df$Overall_Material<-as.factor(df$Overall_Material)
df$House_Condition<-as.factor(df$House_Condition)
df$Underground_Full_Bathroom<-as.factor(df$Underground_Full_Bathroom)
df$Underground_Half_Bathroom<-as.factor(df$Underground_Half_Bathroom)
df$Half_Bathroom_Above_Grade<-as.factor(df$Half_Bathroom_Above_Grade)
df$Bedroom_Above_Grade<-as.factor(df$Bedroom_Above_Grade)
df$Rooms_Above_Grade<-as.factor(df$Rooms_Above_Grade)
df$Fireplaces<-as.factor(df$Fireplaces)
df$Garage_Size<-as.factor(df$Garage_Size) #even we can exicute all this with for loop
df$Full_Bathroom_Above_Grade<-as.factor(df$Full_Bathroom_Above_Grade)
df$Kitchen_Above_Grade<-as.factor(df$Kitchen_Above_Grade)
str(df)
#data cleaning : missing value imputaion
nas<- apply(df,2,function(M) sum(is.na(M)))
nas
sort(nas, decreasing = T)
is.na(df$Miscellaneous_Feature)

#now to clean the missing data
install.packages("missForest")
library(missForest)
set.seed(123) #same repetitation of random number
dfimp=missForest(df,maxiter = 2,ntree = 20)
dfimp
dfimp<-dfimp$ximp

# $imp==> imputed values from missForest
#view imputed missing variable
summary(df$Miscellaneous_Feature)
summary(dfimp$Miscellaneous_Feature)
df=dfimp
df$Construction_Year=as.factor(df$Construction_Year)
df$Remodel_Year=as.factor(df$Remodel_Year)
df$Garage_Built_Year=as.factor(df$Garage_Built_Year)
df$Year_Sold=as.factor(df$Year_Sold)
df$Garage_Built_Year=as.factor(df$Garage_Built_Year)
library(dplyr)
dfnum<-data.frame(select_if(df,is.numeric))
df_fact<-data.frame(select_if(df,is.factor))
getwd()
library(RColorBrewer)
dev.new()
display.brewer.all()  # for displaying all the palettes ()

#color palettes selection
shade=brewer.pal(n=8,name='Accent')# n is no. of colors
class(shade)# to check datatype we use datatype
shade=replicate(50,shade)# to replicate more 50 colorshade and store in shade
shade



##paste function
  
######
for (m in 1:7) 
  
{
      #names(dfnum), # its display varible name
      print(paste('distribution of',
      names(dfnum[m]),
      sep=' '))
      }

### calculate freq. for bar plot
table(df_fact[,4])
dev.new()
barplot(table(df_fact[,4])
)


#to get frequency of  catogary in varible/feature
z99=data.frame(table(df_fact[,9]))
z99
f=z99$Var1
f
z99$p=round((z99$Freq/sum(z99$Freq))*100,1);z99
paste(z99$p,"%",sep="")




#how to save plot as images
jpeg('a2.jpg')
#dev.new()
hist(df$Sale_Price)
dev.off()

ae<-length(df_fact);ae
#while loop
j=1
s=1
e=8
while (j<=ae) {
  h=paste("bar",e,sep = '_')
  ## save plot in jpeg format
  
  jpeg(paste(h,'jpg',sep = '.'),
       res = 600,width = 12,
       height = 6, units = 'in')
  par(mfrow=c(2,4))
  for (i in s:e) {
    # freq.table
    zi<-data.frame(table(df_fact[,i]))
    #y-axis limit (20% more than max.freq)
    yi<-c(0,1.2*max(zi$Freq))
    #list of variables
    li=zi$Var1
    #calculating % of total freq
    zi$p<-round((zi$Freq/sum(zi$Freq))*100,1)
    #paste '%'sign for % numbers
    l2i<-paste(zi$p,'%',sep='')
    bi<-barplot(zi$Freq,ylim = yi,col= shade,
                main = paste(names(df_fact[i]),
                             'wise proportions(nos.&%)',sep = ''),
                names.arg = levels(df_fact[,i]))#x-axis labels
    #sep="\n"==> paste one below the other(next line)
    pti<-paste(zi$freq,l2i,sep = '\n')
    #addition text to bars
    text(bi,labels = pti,col = 'blue',pos = 3,cex = 1)
    
  }# end of for loop pos==>position,cex==>character front
  
  dev.off()#after saving jpeg clear buffer
  j<-j+8
  s<-e+1
  if(e+8>length(df_fact)){
    e=length(df_fact)
  }else{
    e=e+8
  }
  
  
  
}


###### pie c

j=1
s=1
e=8
while (j<=length(df_fact)+1) {
  h=paste("pie",e,sep = '_')
  ## save plot in jpeg format
  
  jpeg(paste(h,'jpg',sep = '.'),
       res = 600,width = 12,
       height = 6, units = 'in')
  par(mfrow=c(2,4))
  for (i in s:e) {
    # freq.table
    zi<-data.frame(table(df_fact[,i]))
    #y-axis limit (20% more than max.freq)
    yi<-c(0,1.2*max(zi$Freq))
    #list of variables
    li=zi$Var1
    #calculating % of total freq
    zi$p<-round((zi$Freq/sum(zi$Freq))*100,0)
    #paste '%'sign for % numbers
    l2i<-paste(li,zi$p,sep=':')
    l3i<-paste(l2i,'%',sep = '')
    pie(zi$Freq,lebels=l3i,col= shade,
                main = paste(names(df_fact[i]),
                             'wise proportions(nos.&%)',sep = ''),
                ) 
   # pti<-paste(zi$freq,l3i,sep = '\n')
    #addition text to bars
    #text(bi,zi$freq,labels = pti,col = 'blue',pos = 3,cex = 1)
    
  
      }
  
  dev.off()#after saving jpeg clear buffer
  j<-j+8
  s<-e+1
  if(e+8>length(df_fact)){
    e=length(df_fact)
  }else{
    e=e+8
  }
  
  
  
}
 #####pivot table   (we use this to compair any two varible and analys only for factor)
library(gmodels)
levels(df_fact$Road_Type)     #### leve should be less then 6 or 6 type
CrossTable(df_fact$Zoning_Class,
           df_fact$Road_Type,
           digits = 1,
           format = 'SPSS',
           prop.chisq = F,prop.c = F,prop.t = F)### if we want to cancled colum % and colum table

CrossTable(df_fact$Heating_Type,
           df_fact$House_Type,
           digits = 1,
           format = 'SPSS',
           prop.chisq = F)
#hot-coding of category variables

install.packages("mltools")
library(mltools)
library(data.table)
df5=one_hot(as.data.table(df))

#######
library(missForest)
set.seed(123) #same repetitation of random number
dfimp7=missForest(dfnew,maxiter = 2,ntree = 20)
dfimp8<-dfimp7$ximp

dfnew<-read.csv(file.choose())
dfnew$Id<-NULL
library(mltools)
df7<-one_hot(as.data.table(dfimp8))
#splitr the data into train(trn)& test(tst)
set.seed(123)
rno=sample(nrow(df7),nrow(df7)*.7)### sample the vector ramdomly
trn<-df7[rno,]
tst<-df7[-rno,]### select the remaning value
## buildindg lm on train()trndata
L1<- lm(trn$Sale_Price~.,data=trn)

summary(L1)
### pridetiob and accuracy
L1pred<-predict(L1,newdata = tst)
head(L1pred)
head(tst$Sale_Price)
RMSEL1<-sqrt(mean(L1pred-tst$Sale_Price)^2);RMSEL1
#diagnostic plot for assumptions
dev.new()
par(mfrow=c(2,2))
plot(L1)
## for model2 :without outliers
##remove outlier
df7out<-df7[-c(722,311),]
set.seed(123)
rno7=sample(nrow(df7out),nrow(df7out)*.7)### sample the vector ramdomly
trn<-df7out[rno,]
tst<-df7out[-rno,]### select the remaning value



