library(tidyverse)
library(cluster)
library("ggplot2")
library("ggdendro")
library("imputeTS")
library(factoextra)


load(file="output/joined_data_by_isocode.RData")

#We choose the data needed for this clustering from the "joined" database
#interpolate the na is possible but it does not make sense

filtered_joined <- joined %>% filter(location != "International" & location != "World" )
df<-data.frame(filtered_joined)
selecteddf <- df %>% select(iso_code, date , total_cases_per_million, total_deaths_per_million, stringency_index)
    #selecteddffull<-na_interpolation(selecteddf, option = "linear")  
drop_na() %>% 
  glimpse()


#plot the selected data
ggplot(selecteddf, aes(date, new_cases_per_million, group=iso_code, color=iso_code))+ geom_line()+xlab("")+ theme(legend.position = 'none')


#prepare data for cluster analysis
selecteddf_spread <- selecteddf %>%
  spread(iso_code, total_cases_per_million)  

deaths<- t(selecteddf_spread [-1])

#perform the cluster analysis
#since data here is all numeric, I can calculate 
#distance in a more direct way that when data is mixed
#for mixed type data I would use: 
#dissimMatrix<-daisy(deaths, metric = "gower", stand=TRUE)
#distMatrix<-dissimMatrix^2

distMatrix <- dist(deaths, method="euclidean") 
h1 <- hclust(distMatrix,method="ward.D2")  

#Decide the cluster number and join to the database

clustered_data <- cutree(h1, k=11)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("iso_code","cluster")
clustered_data_tidy$iso_code <- as.character(clustered_data_tidy$iso_code)

joined_clusters <- selecteddf %>%
  inner_join(clustered_data_tidy, by = "iso_code") %>%
  glimpse()



#Line plot according to cluster

ggplot(joined_clusters, aes(date, total_cases_per_million, group=iso_code, color=cluster))+
  geom_line()+xlab("")+ 
  theme(legend.position = 'none')+
  facet_wrap(~cluster)




#################  WAYS OF PLOTTING THE DENDROGRAM   #################
# 1. base plot
plot(h1, hang=-1, cex= 0.2)

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

