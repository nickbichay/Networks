

setwd("/Users/nickbichay/Desktop/ /aPLS 900/miniproj1/")


data <- read.csv("/Users/nickbichay/Desktop/ /aPLS 900/miniproj1/2010-01-01-2019-01-23-Indonesia-Laos-Thailand.csv", stringsAsFactors=FALSE)


# add 1's to aggreate

data <- cbind(data, "indicator"= 1)


# aggregate temporal data to one cross-section measure

agg_data <- aggregate(cbind(fatalities=data$fatalities, events=data$indicator), by=list(actor1=(data$actor1), actor2=(data$actor2)), FUN=sum)


# remove duplicate dyads

agg_data$id <- apply(cbind(agg_data$actor1, agg_data$actor2), 1, function(x) paste(sort(x), collapse='+'))


agg_data2 <- aggregate(cbind(fatalities=agg_data$fatalities, events=agg_data$events), by=list(id = agg_data$id), FUN=sum)

agg_data2$actor1 <- do.call(rbind, strsplit(agg_data2$id, split='+', fixed=TRUE))[,1]
agg_data2$actor2 <- do.call(rbind, strsplit(agg_data2$id, split='+', fixed=TRUE))[,2]


# adjacency matrix

actors <- unique(c(agg_data2$actor1, agg_data2$actor2))

n = length(actors)
mat <- matrix(0, nrow=n, ncol=n, dimnames=list(actors,actors))
diag(mat) = NA


for(i in 1:nrow(agg_data2)){
    rowActor = as.character(agg_data2$actor1[i])
    colActor = as.character(agg_data2$actor2[i])
    mat[rowActor, colActor]  <- agg_data2$events[i]
    mat[colActor, rowActor]  <- agg_data2$events[i]
    }


# degree scores

degree_scores <- apply(mat, 1, sum, na.rm=TRUE)
which.max(degree_scores)


# graph

library(igraph)

graph <- graph_from_adjacency_matrix(mat, mode='undirected', weighted=TRUE, diag=FALSE)


V(graph)$label <- ifelse(degree_scores>=10, V(graph)$name, NA)



par(mar=c(0,0,0,0))
plot(graph,
     vertex.label=V(graph)$label,
     vertex.label.cex=0.5,
     vertex.color = "grey", 
     vertex.label.color = "black",
     edge.width=1.3,
     vertex.size = 10,
     edge.curved=.25,
    )

