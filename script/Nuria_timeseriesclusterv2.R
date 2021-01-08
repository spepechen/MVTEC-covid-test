library(tidyverse)
library(cluster)
library("ggplot2")
library("ggdendro")
library("imputeTS")
library(factoextra)
library(NbClust)
library(matrixTests)
library(nbpMatching)
library(networkD3)
library(lubridate)
Sys.setlocale("LC_ALL", "English") #to assure labels

#LOAD DATA AND PREPROCESS FOR CLUSTERING

#Load data is the output from xavier_eda_nuria.R in weekly aggregates
load(file="output/dfweekly.RData")
df<-data.frame(dfweekly)

#this has only numerical data. Must join the categorical
#the amount of countries does not totally match,we make a inner join 
#so that we loose the ones that are not in both datasets
#use the data tha Spe revised
datacateg <- read.csv("output/ddExtra2.csv",header=T)%>% rename(location = COUNTRY)
joineddf <- inner_join(df, datacateg, by="iso_code")%>% rename(location = location.x) 

#and declare factors as in spe_data_prep.R
joineddf <- joineddf %>% 
  mutate(Continent = factor(Continent),
         Country_Clasification = factor(Country_Clasification), 
         Government_Type = factor(Government_Type), 
         Corruption_preception = factor(Corruption_preception),
         Development_Status = factor(Development_Status, ordered=TRUE, c( "Developed economies","Transition economies","Developing economies")), 
         Land_Conditions =  factor(Land_Conditions))

#Select the data to go into analysis
selecteddf <- joineddf %>% select(location, week , total_deaths_per_million, total_cases_per_million, new_deaths_smoothed_per_million)
  
#Fill the missing values for selected data
selectedFilled<-fill.missing(selecteddf, seed = 101, simplify = TRUE, idcol = "location")

#plot a selected variable
ggplot(selectedFilled, aes(week, total_deaths_per_million, color=location))+ 
  geom_line()+xlab("")+ theme_minimal()+
  theme(legend.position = 'none')


#clustering on time series: needs a column for each week entry
selecteddfpivot <- selectedFilled %>%  
  pivot_wider(id_cols=c('location','total_deaths_per_million'),
              names_from='week',
              names_sep = ".",
              values_from=c('total_deaths_per_million'),
              values_fill=0) %>%
remove_rownames()%>%
  column_to_rownames("location")





#PERFORM CLUSTERING ANALYSIS

#since data here is all numeric, I can calculate 
#distance in a more direct way that when data is mixed
#for mixed type data I would use: 
#dissimMatrix<-daisy(deaths, metric = "gower", stand=TRUE)
#distMatrix<-dissimMatrix^2

distMatrix <- dist(selecteddfpivot, method="euclidean") 
h1 <- hclust(distMatrix,method="ward.D2")  

labels<-h1$label[h1$order]


#--------------------WAYS OF PLOTTING THE DENDROGRAM-------------------
# 1. base plot
plot(h1, hang=-1, cex= 0.2)
plot(h1,  cex= 0.2)

#1.1 base plot with boxes
plot(h1, hang=-1, cex= 0.2)
rect.hclust(h1 , k = 11 ,border = 2:6)
#abline(h =0.4, col = 'yellow')

# 2. base plot as dendogram
hcd <- as.dendrogram(h1)
par(cex=0.3, mar=rep(2,4))
plot(hcd, type = "rectangle", ylab = "Height")

#3. plot with ggplot2 and ggdendro
ggdendrogram(hcd, rotate = TRUE)

ddata <- dendro_data(hcd, type = "rectangle")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text( data=ddata$labels, aes(x = x, y = y, label = label), size = 0.5,hjust = 0, position = position_nudge(y = +100))+
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))

p + 
  theme_dendro()



#4 producing a javascript via networkD3
dendroNetwork(h1, height = 600)
dendroNetwork(h1) %>% saveNetwork(file ="Net1.html")



#------------------------------------------------------------------------------


#Decide the optimal cluster number from indexes
#dindex, huber
res.nbclust <- NbClust( data=selecteddfpivot,distance = "euclidean",
                       min.nc = 2, max.nc =25, 
                       method = "ward.D2", index ="dindex")
print(res.nbclust$Best.nc)

#Cut according to decided cluster number
clustered_data <- cutree(h1, k=11)

#Line plot according to cluster
ggplot(dfweekly_clusters, aes(week, total_deaths_per_million, group=location, color=Continent))+
  geom_line()+xlab("")+ 
  theme_minimal()+
  theme(legend.position = 'top')+
  facet_wrap(~cluster)





#OUTPUT THE RESULTS

#a file with locations and cluster numbers
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) 
colnames(clustered_data_tidy) <- c("location","cluster")
clustered_data_tidy$location <- as.character(clustered_data_tidy$location)

write.csv(x=clustered_data_tidy, file="output/clustered_data_tidy.csv") 

 
#Add cluster column to the pivoted data

selecteddfpivot_cluster <-  selecteddfpivot %>%rownames_to_column("location")%>%
  inner_join(clustered_data_tidy, by = "location")
cluster <- as.factor(selecteddfpivot_cluster$cluster)
levels(cluster)
selecteddfpivot_cluster[,ncol(selecteddfpivot_cluster)]<-as.factor(cluster)


#Add cluster column to the original dataframe with all variables

dfweekly_clusters <- joineddf %>%
  inner_join(clustered_data_tidy, by = "location")
cluster <- as.factor(dfweekly_clusters$cluster)
levels(cluster)
dfweekly_clusters[,ncol(dfweekly_clusters)]<-as.factor(cluster)
save(dfweekly_clusters, file="output/dfweekly_withclusters.RData")  
write.csv(x=dfweekly_clusters, file="output/dfweekly_withclusters.csv") 


#convert output from hclust into a nested JSON file

library(rjson)
HCtoJSON<-function(h1){
  labels<-h1$labels
  merge<-data.frame(h1$merge)
  for (i in (1:nrow(merge))) {
    if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
    else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
  }
  eval(parse(text=paste0("JSON<-toJSON(node",nrow(merge), ")")))
  return(JSON)
}

dataJSON<-HCtoJSON(h1)
write(dataJSON, "data.json")





#VARIOUS OUTPUTS and CHEKINGS for building  the D3 visual

clustered_data3 <- cutree(h1, k=3)
clustered_data_tidy3 <- as.data.frame(as.table(clustered_data3)) 
colnames(clustered_data_tidy3) <- c("location","cluster3")

clustered_data5 <- cutree(h1, k=5)
clustered_data_tidy5 <- as.data.frame(as.table(clustered_data5)) 
colnames(clustered_data_tidy5) <- c("location","cluster5")

clustered_data11 <- cutree(h1, k=11)
clustered_data_tidy11 <- as.data.frame(as.table(clustered_data11)) 
colnames(clustered_data_tidy11) <- c("location","cluster11")

clustered_data24 <- cutree(h1, k=24)
clustered_data_tidy24 <- as.data.frame(as.table(clustered_data24)) 
colnames(clustered_data_tidy24) <- c("location","cluster24")

many_clustered_data_tidy<-(clustered_data_tidy3) %>%
  left_join(clustered_data_tidy5, by = "location") %>%
  left_join(clustered_data_tidy11, by = "location") %>%
  left_join(clustered_data_tidy24, by = "location")
many_clustered_data_tidy$location <- as.character(many_clustered_data_tidy$location)

many_dfweekly_clusters <- joineddf %>%  inner_join(many_clustered_data_tidy, by = "location")

selectedFilled_clusters <- selectedFilled %>%  inner_join(many_clustered_data_tidy, by = "location")

save(many_dfweekly_clusters, file="output/many_dfweekly_withclusters.RData")  
write.csv(x=many_dfweekly_clusters, file="output/many_dfweekly_withclusters.csv") 
write.csv(x=selectedFilled_clusters, file="output/selectedFilled_clusters.csv") 

max(selectedFilled$total_deaths_per_million, na.rm = TRUE)

labelsdf<-as.data.frame(labels)
colnames(labelsdf) <- c("location")
labelsdf_clusters <- labelsdf %>%
   inner_join(many_clustered_data_tidy, by = "location")%>%  
 inner_join(ddnames, by = "location")
write.csv(x=labelsdf_clusters, file="output/labelsdf_clusters.csv") 




