############################
#Mini Project: 1
eventdata <- `2010.01.01.2019.01.23.Indonesia.Laos.Thailand`



# Aggregate Data
# Add Column of 1's
data <- cbind(eventdata, "indicator"=1)
View(data)
library(dplyr)

# Aggregate Data
agdata <- aggregate(cbind(fatalities=data$fatalities, events=data$indicator), 
                    by=list(actor1=(data$actor1), actor2=(data$actor2)), FUN=sum)
View(agdata)


# Write a loop to create sociomatrix



# Find unique actors
actors <- unique(c(agdata$actor1, agdata$actor2))


# Remove duplicate dyads

agdata$id <- apply(cbind(agdata$actor1, agdata$actor2), 1, function(x) paste(sort(x), collapse="+"))

agg_data2 <- aggregate(cbind(fatalities=agdata$fatalities, events=agdata$events), by=list(id = agdata$id), FUN=sum)

key <- agdata[,c(1:2,5)]

agg_data3 <- merge(key, agg_data2, by.x=c('id'), by.y=c('id'), all=FALSE)


# Split String bringing back Actor 1 and Actor 2
agg_data2$actor1 <- do.call(rbind, strsplit(agg_data2$id,split='+', fixed=TRUE))[,1]
agg_data2$actor2 <- do.call(rbind, strsplit(agg_data2$id,split='+', fixed=TRUE))[,2]

# Create Matrix
actors <- unique(c(agg_data2$actor1, agg_data2$actor2))

n= length(actors)
mat <- matrix(0, nrow=n, ncol=n, dimnames=list(actors, actors))
diag(mat) = NA

# Fill Matrix
#create loop
for(i in 1:nrow(agg_data2)){
  
  #identify actos involved in alliances
  
  rowactor = as.character(agg_data2$actor1[i])
  colactor = as.character(agg_data2$actor2[i])
  
  #fill in adjmat
  mat[rowactor, colactor] = agg_data2$events[i]
  mat[colactor, rowactor] = agg_data2$events[i]
}

# Calculate Actor Centrality
degree_score <- apply(mat, 1, sum, na.rm=TRUE)

max(apply(mat, 1, sum, na.rm=TRUE))

max(apply(mat, 2, sum, na.rm=TRUE))

# Most Central Actor: Southern Muslim Separatists


# Graph Network
library(igraph)
g = graph_from_adjacency_matrix(mat, 
                                mode='undirected', 
                                weighted=TRUE,
                                diag=FALSE
)

# Plot Graph
plot(g,
     vertex.label=V(g)$label, 
     vertex.size=V(g)$size,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .5, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20" # change edge color to grey  
)

# only label if # ties greater than 10
V(g)$label <- ifelse( degree_score>=5, V(g)$name, NA )

# now plot
par(mar=c(0,0,0,0))
plot(g,
     vertex.label=V(g)$label, 
     vertex.size=V(g)$size,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .5, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20", # change edge color to grey 
     edge.width = 1.4,
     vertex.size = 1,
     vertex.size2 = 1,
     asp = 1.6,
     margin = -0.1
)

