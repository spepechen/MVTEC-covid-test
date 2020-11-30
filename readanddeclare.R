library(tidyverse)

#Set you own path rememebet to clone group repo
setwd("C:/LocalData/altimir/MASTERlocal/MyMaterial3module")

#Read data fron the data files.Attention to headers, NA, formats etc
dd <- read.csv("C:/LocalData/altimir/MASTERlocal/MyMaterial3module/owid-covid-data.csv",header=TRUE, na.strings="NA")
ddExtra <- read_excel("InfoPaisosExtra.xlsx", na = "NA")

#Make some checking, the correct number of rows and columns? 
dim(dd)
n<-dim(dd)[1]
n
K<-dim(ddExtra)[2]
K
#Check the type of objects under each heading
sapply(dd, class)
sapply(ddExtra, class)
#check column contents by reading headings
names(dd)
names(ddExtra)
#view as a sheet
View(dd)

#open access by name to columns.By attaching a data frame (or list) to the search path
#it is possible to refer to the variables in the data frame by their names alone, 
#rather than as components of the data frame (e.g.,  height rather than women$height)
attach(dd)




# DECLARE CATEGORICAL
# if numerical variables are taken as FACTORS, include proper "dec" parameter
#dd <- read.table("credsco.csv",header=T, sep=";", dec=".");
 

#declare factors in the ddEXtra
ddExtra$Continent <- as.factor(ddExtra$Continent)
ddExtra$Country_Clasification <- as.factor(ddExtra$Country_Clasification)
ddExtra$Corruption_preception <- as.factor(ddExtra$Corruption_preception)
ddExtra$Land_Conditions <- as.factor(ddExtra$Land_Conditions)
ddExtra$Government_Type  <- as.factor(ddExtra$Government_Type )
ddExtra$Development_Status  <- as.factor(ddExtra$Development_Status )
#check levels
levels(Continent)
levels(Country_Clasification)
levels(Corruption_preception)
levels(Land_Conditions)
levels(Government_Type)
levels(Development_Status)


#DECLARE ORDINAL    TBD
#corruption perception
#Development_Status




#DECLARE DATES
Data<-as.Date(dd[,4],format="%Y-%m-%d")
class(Data)
summary(Data)



#RENAME   TBD 
#acronymise the titles so they fit in eg a graph like this
barplot(table(ddExtra$Government_Type), main=paste("Barplot of", names(ddExtra$Government_Type)))
#substitute names with another list of names
names(dd)<-newNames(.....)


#substitute the values of the modalities, recode
#get the list of values for a valuable. Create a copy of the variable to work with 
#need to declare it as factor(category) and check the levels
GovType<-factor(ddExtra$Government_Type)
levels(GovType)

#decide the new values
GovTypeNew<-GovType
newvalues<-c("AMon","ComS","CMon","Dic","InTrans","IParR","IPreR","ISPreRep","PRep","PLD","PR","SPR")
GovTypeNew<-newvalues[match(GovTypeNew, levels(GovType))]
#running the barplot with this new thing to check how it looks
barplot(table(GovTypeNew), cex.names=0.7,main=paste("Barplot of", names(ddExtra$Government_Type)))


#JOIN DATA
#need to have a common column, rename Location in dd as COUNTRY

location(dd) <- "COUNTRY"
covidCountry <- merge(dd,ddExtra, by="Country")

#MISSING VALUES 

#Substitute missing data via interpolation to be operative, could be also a better calculation
#for qualitative missing replace with "missing"
#Analyze the amount of missing values, how much, what columns, what rows
#Decide what to drop and what to keep
#Decide the type of gapfilling 


#AGGREGATE DATA




#aggregate example. Think the granularity and the statistics

CountryPop<-aggregate(InfoPaisosExtra$Population_total, by=list(GovTypeNew), FUN=mean)

#aggregate the whole data frame: then take care how to take in to account the qualitative
#forexample the mode or simply  delete. Migth need a function that recognizes the type of 
#data and then  do different things on the data depending on the type








#CLUSTER

#Clustering, example on the country dataset, first declare the factors
library(cluster)
#Dissimilarity matrix. Compute all the pairwise dissimilarities (distances) 
#between observations in the data set. The original variables may be of mixed types
#daisy(x, metric = c("euclidean", "manhattan", "gower"), stand = FALSE, type = list())














#propagate preprocessing to dataframe
class(Dictamen)
class(dd[,1])
dd[,1]<-Dictamen
class(dd[,1])

dd[,3]<-Vivienda
dd[,6]<-Estado.civil
dd[,7]<-Registros
dd[,8]<-Tipo.trabajo

class(dd[,1])

#Export pre-processed data to persistent files, independent from statistical package
write.table(dd, file = "credscoCategoriques.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
