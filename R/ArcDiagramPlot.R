ArcDiagramPlot <- function(journal.graph){

# get edgelist
edgelist = get.edgelist(journal.graph)

# clean up the really long names


# get vertex labels
#arcdiagram orders by edgelist not by igraph vertex labels- so doesn't include those species that don't have edges.
nam=character()
for(i in 1:dim(edgelist)[1]){
  nam=c(nam, edgelist[i,])  
}
vlabels = levels(factor(nam))

com <- rep(NA, length(vlabels))
for(i in 1:length(vlabels)){
  k <- subset(data.frame, as.character(Source2) == vlabels[i])
  com[i] <- ifelse(length(k$Journal.Community[which(is.na(k$Journal.Community) == F)]) == 0, NA,
                   k$Journal.Community[which(is.na(k$Journal.Community) == F)][1])    
}
com <- as.numeric(as.character(com))
com <- ifelse(com == 1, 4, com)
com <- ifelse(com == 2, 5, com)
com <- ifelse(com == 4, 2, com)
com <- ifelse(com == 5, 1, com)
com <- ifelse(is.na(com), 4, com)

vgroups = com
#rename groups so that largest communities first
x=sort(unique(vgroups))
lx=numeric()
for(i in 1:length(x)){
  lx [i]=length(which(vgroups==x[i]))
}
y=x[order(lx,decreasing=TRUE)]
vgroups2=numeric()
for(i in 1:length(vgroups)){
  vgroups2[i] <- ifelse(is.na(vgroups[i]) == T, NA, which(y==vgroups[i]))
  print(i)
}

# get vertex fill color
col1=brewer.pal(12,"Paired")
cols=rep(col1[c(1,3,5,7,9,11)],3)
cols <- c(rgb(255/255, 215/255, 153/255, alpha = .3), 
          rgb(255/255, 153/255, 153/255, alpha = .3), 
          rgb(153/255, 204/255, 255/255, alpha = .3), 
          rgb(153/255, 204/255, 153/255, alpha = .3))
vfill = rep("gray",length(vgroups2))
for(i in 1:length(y)){
  vfill[which(vgroups2==i)]=cols[i]
}
# get vertex border color
cols=c(rgb(255/255, 215/255, 0), "red", "blue", "green")
vborders = rep("gray",length(vgroups2))
for(i in 1:length(y)){
  vborders[which(vgroups2==i)]=cols[i]
}


# get vertex degree
jo.degrees=degree(journal.graph)

# get edges value
values = get.edge.attribute(journal.graph, "weight")

#sort by community, then degree
ord=order(vgroups2,max(jo.degrees)-jo.degrees)

#with species labels 
par(mar=c(20,0,2,0))
arcplot(edgelist, 
        sorted = T,
        ordering=ord, 
        #        labels=vlabels, 
        cex.labels=0.6,
        show.nodes=TRUE, 
        col.nodes=vborders, 
        bg.nodes=vfill,
        cex.nodes = jo.degrees/max(jo.degrees)+1, 
        pch.nodes=21,
        col.labels="grey30",
        lwd.nodes = 1.5, 
        line= -0.1, 
        lwd.arcs = .5 * values,
        col.arcs = rgb(160/255, 160/255, 160/255, alpha = .2))
#--- END angie code ---#
}
