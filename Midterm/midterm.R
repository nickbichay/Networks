
setwd('/Users/nickbichay/Desktop/ /aPLS 900/Midterm/')


library(igraph)
library(countrycode)


# import data

data <- read.csv('Dyadic_COW_4.0.csv', header=TRUE, stringsAsFactors=FALSE)

# -9 = missing values
data <- data[data$flow1 >= 0 & data$flow2 >= 0,]


##### Slice data by decade

mats <- list()
decades <- c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000)


for(i in decades) {

# slice data by decade
data_i <- data[data$year %in% seq(i, (i+9)), c(1,2,3,6,7)]

print(i)
mean(data_i$flow2)

data_i <- aggregate(cbind(flow1=data_i$flow1, flow2=data_i$flow2), by=list(ccode1=(data_i$ccode1), ccode2=(data_i$ccode2)), FUN=mean)


# remove duplicate dyads

data_i$id <- apply(cbind(data_i$ccode1, data_i$ccode2), 1, function(x) paste(sort(x), collapse='+'))


data_i <- aggregate(cbind(flow1 = data_i$flow1, flow2 = data_i$flow2), by=list(id = data_i$id), FUN=sum)

data_i$ccode1 <- do.call(rbind, strsplit(data_i$id, split='+', fixed=TRUE))[,1]
data_i$ccode2 <- do.call(rbind, strsplit(data_i$id, split='+', fixed=TRUE))[,2]


# keep only top 120 by exports
totals <- aggregate(cbind(exports=data_i$flow2), by=list(ccode1=(data_i$ccode1)), FUN=sum)
totals <- totals[order(-totals$exports),]
top120 <- totals$ccode1[1:120]

data_i <- data_i[(data_i$ccode1 %in% top120),]


actors <- unique(c(data_i$ccode1, data_i$ccode2))

n = length(actors)
mat_i <- matrix(0, nrow=n, ncol=n, dimnames=list(actors,actors))
diag(mat_i) = NA


for(j in 1:nrow(data_i)){
    rowActor = as.character(data_i$ccode1[j])
    colActor = as.character(data_i$ccode2[j])
    mat_i[rowActor, colActor]  <- data_i$flow2[j]
    mat_i[colActor, rowActor]  <- data_i$flow1[j]
    }

mats[[i]] <- mat_i

}

# remove null lists (in the between years)
mats[sapply(mats, is.null)] <- NULL


##### graph objects

#### 1920

g_1920 = graph_from_adjacency_matrix(mats[[1]], mode='directed', weighted=TRUE, diag=FALSE)


e_1920 <- eigen_centrality(g_1920)$'vector'
V(g_1920)$size <- e_1920*35

#e_1920_del <- E(g_1920)[E(g_1920)$weight<0.05]
#g_1920 <- delete.edges(g_1920, e_1920_del )

#g_1920_del <- V(g_1920)[strength(g_1920)<=53]
#g_1920 <- delete.vertices(g_1920, g_1920_del)

width_1920 <- log(E(g_1920)$weight)/10



V(g_1920)$color <- countrycode(attributes(V(g_1920))$names, origin='cown', destination='continent')

V(g_1920)$color=gsub("Americas","green",V(g_1920)$color)
V(g_1920)$color=gsub("Asia","red",V(g_1920)$color)
V(g_1920)$color=gsub("Europe","blue",V(g_1920)$color)
V(g_1920)$color=gsub("Oceania","yellow",V(g_1920)$color)
V(g_1920)$color=gsub("Africa","purple",V(g_1920)$color)

# two NAs which are in Europe
V(g_1920)$color[is.na(V(g_1920)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1920,
    vertex.label="",
    layout=layout.random, 
    vertex.size=V(g_1920)$size,
    #vertex.lab=V(g_1920)$label,
    vertex.color=V(g_1920)$color,
    vertex.frame.color=V(g_1920)$color,
    edge.width=width_1920,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1920)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1930

g_1930 = graph_from_adjacency_matrix(mats[[2]], mode='directed', weighted=TRUE, diag=FALSE)


e_1930 <- eigen_centrality(g_1930)$'vector'
V(g_1930)$size <- e_1930*30

#e_1930_del <- E(g_1930)[E(g_1930)$weight<0.05]
#g_1930 <- delete.edges(g_1930, e_1930_del )

#g_1930_del <- V(g_1930)[strength(g_1930)<=53]
#g_1930 <- delete.vertices(g_1930, g_1930_del)

width_1930 <- E(g_1930)$weight/300



V(g_1930)$color <- countrycode(attributes(V(g_1930))$names, origin='cown', destination='continent')

V(g_1930)$color=gsub("Americas","green",V(g_1930)$color)
V(g_1930)$color=gsub("Asia","red",V(g_1930)$color)
V(g_1930)$color=gsub("Europe","blue",V(g_1930)$color)
V(g_1930)$color=gsub("Oceania","yellow",V(g_1930)$color)
V(g_1930)$color=gsub("Africa","purple",V(g_1930)$color)

# two NAs which are in Europe
V(g_1930)$color[is.na(V(g_1930)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1930,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1930)$size,
    #vertex.lab=V(g_1930)$label,
    vertex.color=V(g_1930)$color,
    vertex.frame.color=V(g_1930)$color,
    edge.width=width_1930,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1930)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1940s

g_1940 = graph_from_adjacency_matrix(mats[[3]], mode='directed', weighted=TRUE, diag=FALSE)


e_1940 <- eigen_centrality(g_1940)$'vector'
V(g_1940)$size <- e_1940*30

#e_1940_del <- E(g_1940)[E(g_1940)$weight<0.05]
#g_1940 <- delete.edges(g_1940, e_1940_del )

#g_1940_del <- V(g_1940)[strength(g_1940)<=53]
#g_1940 <- delete.vertices(g_1940, g_1940_del)

width_1940 <- E(g_1940)$weight/300



V(g_1940)$color <- countrycode(attributes(V(g_1940))$names, origin='cown', destination='continent')

V(g_1940)$color=gsub("Americas","green",V(g_1940)$color)
V(g_1940)$color=gsub("Asia","red",V(g_1940)$color)
V(g_1940)$color=gsub("Europe","blue",V(g_1940)$color)
V(g_1940)$color=gsub("Oceania","yellow",V(g_1940)$color)
V(g_1940)$color=gsub("Africa","purple",V(g_1940)$color)

# two NAs which are in Europe
V(g_1940)$color[is.na(V(g_1940)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1940,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1940)$size,
    #vertex.lab=V(g_1940)$label,
    vertex.color=V(g_1940)$color,
    vertex.frame.color=V(g_1940)$color,
    edge.width=width_1940,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1940)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1950 

g_1950 = graph_from_adjacency_matrix(mats[[4]], mode='directed', weighted=TRUE, diag=FALSE)


e_1950 <- eigen_centrality(g_1950)$'vector'
V(g_1950)$size <- e_1950*30



#e_1950_del <- E(g_1950)[E(g_1950)$weight<0.05]
#g_1950 <- delete.edges(g_1950, e_1950_del )

#g_1950_del <- V(g_1950)[strength(g_1950)<=53]
#g_1950 <- delete.vertices(g_1950, g_1950_del)

width_1950 <- E(g_1950)$weight/300



V(g_1950)$color <- countrycode(attributes(V(g_1950))$names, origin='cown', destination='continent')

V(g_1950)$color=gsub("Americas","green",V(g_1950)$color)
V(g_1950)$color=gsub("Asia","red",V(g_1950)$color)
V(g_1950)$color=gsub("Europe","blue",V(g_1950)$color)
V(g_1950)$color=gsub("Oceania","yellow",V(g_1950)$color)
V(g_1950)$color=gsub("Africa","purple",V(g_1950)$color)

# two NAs which are in Europe
V(g_1950)$color[is.na(V(g_1950)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1950,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1950)$size,
    #vertex.lab=V(g_1950)$label,
    vertex.color=V(g_1950)$color,
    vertex.frame.color=V(g_1950)$color,
    edge.width=width_1950,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1950)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1960

g_1960 = graph_from_adjacency_matrix(mats[[5]], mode='directed', weighted=TRUE, diag=FALSE)


e_1960 <- eigen_centrality(g_1960)$'vector'
V(g_1960)$size <- e_1960*30

#e_1960_del <- E(g_1960)[E(g_1960)$weight<0.05]
#g_1960 <- delete.edges(g_1960, e_1960_del )

#g_1960_del <- V(g_1960)[strength(g_1960)<=53]
#g_1960 <- delete.vertices(g_1960, g_1960_del)

width_1960 <- E(g_1960)$weight/300



V(g_1960)$color <- countrycode(attributes(V(g_1960))$names, origin='cown', destination='continent')

V(g_1960)$color=gsub("Americas","green",V(g_1960)$color)
V(g_1960)$color=gsub("Asia","red",V(g_1960)$color)
V(g_1960)$color=gsub("Europe","blue",V(g_1960)$color)
V(g_1960)$color=gsub("Oceania","yellow",V(g_1960)$color)
V(g_1960)$color=gsub("Africa","purple",V(g_1960)$color)

# two NAs which are in Europe
V(g_1960)$color[is.na(V(g_1960)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1960,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1960)$size,
    #vertex.lab=V(g_1960)$label,
    vertex.color=V(g_1960)$color,
    vertex.frame.color=V(g_1960)$color,
    edge.width=width_1960,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1960)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1970

g_1970 = graph_from_adjacency_matrix(mats[[6]], mode='directed', weighted=TRUE, diag=FALSE)


e_1970 <- eigen_centrality(g_1970)$'vector'
V(g_1970)$size <- e_1970*30

#e_1970_del <- E(g_1970)[E(g_1970)$weight<0.05]
#g_1970 <- delete.edges(g_1970, e_1970_del )

#g_1970_del <- V(g_1970)[strength(g_1970)<=53]
#g_1970 <- delete.vertices(g_1970, g_1970_del)

width_1970 <- log(E(g_1970)$weight/200)



V(g_1970)$color <- countrycode(attributes(V(g_1970))$names, origin='cown', destination='continent')

V(g_1970)$color=gsub("Americas","green",V(g_1970)$color)
V(g_1970)$color=gsub("Asia","red",V(g_1970)$color)
V(g_1970)$color=gsub("Europe","blue",V(g_1970)$color)
V(g_1970)$color=gsub("Oceania","yellow",V(g_1970)$color)
V(g_1970)$color=gsub("Africa","purple",V(g_1970)$color)

# two NAs which are in Europe
V(g_1970)$color[is.na(V(g_1970)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1970,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1970)$size,
    #vertex.lab=V(g_1970)$label,
    vertex.color=V(g_1970)$color,
    vertex.frame.color=V(g_1970)$color,
    edge.width=width_1970,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1970)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1980

g_1980 = graph_from_adjacency_matrix(mats[[7]], mode='directed', weighted=TRUE, diag=FALSE)

e_1980 <- eigen_centrality(g_1980)$'vector'
V(g_1980)$size <- e_1980*30

#e_1980_del <- E(g_1980)[E(g_1980)$weight<0.05]
#g_1980 <- delete.edges(g_1980, e_1980_del )

#g_1980_del <- V(g_1980)[strength(g_1980)<=53]
#g_1980 <- delete.vertices(g_1980, g_1980_del)

width_1980 <- E(g_1980)$weight/3000



V(g_1980)$color <- countrycode(attributes(V(g_1980))$names, origin='cown', destination='continent')

V(g_1980)$color=gsub("Americas","green",V(g_1980)$color)
V(g_1980)$color=gsub("Asia","red",V(g_1980)$color)
V(g_1980)$color=gsub("Europe","blue",V(g_1980)$color)
V(g_1980)$color=gsub("Oceania","yellow",V(g_1980)$color)
V(g_1980)$color=gsub("Africa","purple",V(g_1980)$color)

# two NAs which are in Europe
V(g_1980)$color[is.na(V(g_1980)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1980,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1980)$size,
    #vertex.lab=V(g_1980)$label,
    vertex.color=V(g_1980)$color,
    vertex.frame.color=V(g_1980)$color,
    edge.width=width_1980,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1980)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 1990

g_1990 = graph_from_adjacency_matrix(mats[[8]], mode='directed', weighted=TRUE, diag=FALSE)

e_1990 <- eigen_centrality(g_1990)$'vector'
V(g_1990)$size <- e_1990*30

#e_1990_del <- E(g_1990)[E(g_1990)$weight<0.05]
#g_1990 <- delete.edges(g_1990, e_1990_del )

#g_1990_del <- V(g_1990)[strength(g_1990)<=53]
#g_1990 <- delete.vertices(g_1990, g_1990_del)

width_1990 <- log(E(g_1990)$weight)/10



V(g_1990)$color <- countrycode(attributes(V(g_1990))$names, origin='cown', destination='continent')

V(g_1990)$color=gsub("Americas","green",V(g_1990)$color)
V(g_1990)$color=gsub("Asia","red",V(g_1990)$color)
V(g_1990)$color=gsub("Europe","blue",V(g_1990)$color)
V(g_1990)$color=gsub("Oceania","yellow",V(g_1990)$color)
V(g_1990)$color=gsub("Africa","purple",V(g_1990)$color)

# two NAs which are in Europe
V(g_1990)$color[is.na(V(g_1990)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_1990,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_1990)$size,
    #vertex.lab=V(g_1990)$label,
    vertex.color=V(g_1990)$color,
    vertex.frame.color=V(g_1990)$color,
    edge.width=width_1990,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (1990)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')


#### 2000

g_2000 = graph_from_adjacency_matrix(mats[[9]], mode='directed', weighted=TRUE, diag=FALSE)

e_2000 <- eigen_centrality(g_2000)$'vector'
V(g_2000)$size <- e_2000*30

#e_2000_del <- E(g_2000)[E(g_2000)$weight<0.05]
#g_2000 <- delete.edges(g_2000, e_2000_del )

#g_2000_del <- V(g_2000)[strength(g_2000)<=53]
#g_2000 <- delete.vertices(g_2000, g_2000_del)

width_2000 <- E(g_2000)$weight/300



V(g_2000)$color <- countrycode(attributes(V(g_2000))$names, origin='cown', destination='continent')

V(g_2000)$color=gsub("Americas","green",V(g_2000)$color)
V(g_2000)$color=gsub("Asia","red",V(g_2000)$color)
V(g_2000)$color=gsub("Europe","blue",V(g_2000)$color)
V(g_2000)$color=gsub("Oceania","yellow",V(g_2000)$color)
V(g_2000)$color=gsub("Africa","purple",V(g_2000)$color)

# two NAs which are in Europe
V(g_2000)$color[is.na(V(g_2000)$color)] <- "blue"


par(mar=c(0,0,1,0))
plot(g_2000,
    vertex.label="",
    layout=layout.grid, 
    vertex.size=V(g_2000)$size,
    #vertex.lab=V(g_2000)$label,
    vertex.color=V(g_2000)$color,
    vertex.frame.color=V(g_2000)$color,
    edge.width=width_2000,
    #edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Trade Exports (2000)"
)
legend('bottomleft', legend= c('Americas', 'Asia', 'Europe', 'Oceania', 'Africa'), col = c('green', 'red', 'blue', 'yellow', 'purple'), pch=20, bty = 'n')














