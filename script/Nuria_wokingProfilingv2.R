library(Hmisc)

## TO BE RUN AFTER THE CLUSTERING FILE!!!! 

#script continuation of Nuria_timeseriesclusterv2.R
#this scripts uses the df previoulsy generated 
#"selecteddfpivot_cluster"  and "dfweekly_clusters"
#Performs the profiling on these two data sets




#Write the functions(as in Karina's script)

ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}
ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2]);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}

##PROFILING ON THE PIVOTED SELECTED DATASET

#droping SanMarino, Peru and Brussels if needed
#selecteddfpivot_cluster<-selecteddfpivot_cluster%>%   
# filter(location != "Brussels" & location != "Peru" & location != "San Marino" )


#dades contain the dataset
dades<-as.data.frame(selecteddfpivot_cluster)
K<-(dim(dades)[2])#number of columns:48 weeks + location + cluster
par(ask=TRUE)
#P must contain the class variable
P<-selecteddfpivot_cluster$cluster
nc<-length(levels(factor(P))) # number of groups
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"cluster"
n<-dim(dades)[1]#number of rows: countries

#pdf("profiling.pdf")# to collect the graphs in a pdf

for(k in 2:K){
  if (is.numeric(dades[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    
    
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=FALSE)
    
    barplot(tapply(dades[[k]], P, mean),ylim=c(0,1400), xpd=F, main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    # o<-oneway.test(dades[,k]~P)
    #print(paste("p-valueANOVA:", o$p.value))
   kw<-kruskal.test(dades[,k]~P)
  print(paste("p-value Kruskal-Wallis:", kw$p.value))
   pvalk[,k]<-ValorTestXnum(dades[,k], P)
   print("p-values ValorsTest: ")
    print(pvalk[,k])      
   print(names(dades)[k])
  } 
  
}#endfor

#dev.off()# to collect the graphs in a pdf


  
   


    
   


##PROFILING ON  A SELECTION OF THE ORIGINAL DATASET

#droping SanMarino, Peru and Brussels if needed
#dfweekly_clusters<-dfweekly_clusters%>%   
 # filter(location != "Brussels" & location != "Peru" & location != "San Marino" )

##preprocessing to fill in the na first

#Select data columns to separate num from qual (instead of looping)
#cols <- c(1:3,6:7, 10:11, 14:15,18,21,31, 33:34)
cols <- c(1:3,6:7, 10:11, 14:15, 33:34)
dfweekly_clusters_selectedcolsnumeric <- dfweekly_clusters[,cols]
colsquali<-c(1:3,42:52)
dfweekly_clusters_selectedcolsquali<- dfweekly_clusters[,colsquali]



#Fill the missing values for selected  numerical  data
dfweekly_filled<-fill.missing(dfweekly_clusters_selectedcolsnumeric, seed = 101, simplify = TRUE, idcol = "location")


dfweekly_filledclean<-dfweekly_filled[,1:11]
#Join back filled numerical without extra flag columns and qualy
dfweekly_ready <- left_join(dfweekly_filledclean,dfweekly_clusters_selectedcolsquali) 




#dades contain the dataset
dades<-as.data.frame(dfweekly_ready)
dd<-as.data.frame(dfweekly_ready)
K<-(dim(dades)[2])#number of columns
par(ask=TRUE)
#P must contain the class variable
P<-dfweekly_ready$cluster
nc<-length(levels(factor(P))) # number of groups
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"cluster"
n<-dim(dades)[1]#number of rows: countries by week



#pdf("profiling3.pdf")# to collect the graphs in a pdf

for(k in 1:K){
  if (is.numeric(dades[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    # o<-oneway.test(dades[,k]~P)
    #print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dades[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    if(class(dd[,k])=="Date"){
      #print(summary(dd[,k]))
      #print(sd(dd[,k]))
      #decide breaks: weeks, months, quarters...
      hist(dd[,k],breaks="weeks")
    }else{
      #qualitatives
      #print(paste("Variable", names(dades)[k]))
        table<-table(P,dades[,k])
      #   print("Cross-table")
      #   print(table)
      rowperc<-prop.table(table,1)
      
      colperc<-prop.table(table,2)
      #  print("Distribucions condicionades a files")
      # print(rowperc)
      
      #ojo porque si la variable es true o false la identifica amb el tipus Logical i
      #aquest no te levels, por tanto, coertion preventiva
      
      dades[,k]<-as.factor(dades[,k])
      
      
      marg <- table(as.factor(P))/n
      print(append("Categories=",levels(as.factor(dades[,k]))))
      
      #from next plots, select one of them according to your practical case
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
      
      #condicionades a classes
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
      
      #amb variable en eix d'abcisses
      marg <-table(dades[,k])/n
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      #x<-plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), xaxt="n")
      #text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      #condicionades a columna 
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      table<-table(dades[,k],P)
      #print("Cross Table:")
     # print(table)
     # print("Distribucions condicionades a columnes:")
     # print(colperc)
      
      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      #diagrames de barres adosades
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )
      
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
      
      print("valorsTest:")
    #  print( ValorTestXquali(P,dades[,k]))
      #calcular els pvalues de les quali
    }
  }
}#endfor
#dev.off()# to collect the graphs in a pdf

#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}

#afegir la informacio de les modalitats de les qualitatives a la llista de pvalues i fer ordenacio global

#saving the dataframe in an external file
#write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

