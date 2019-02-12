
setwd("/Users/nickbichay/Desktop/ /aPLS 900/miniproj2")


# aggregate data

data  <- read.csv("inflows.csv", header=TRUE, stringsAsFactors=FALSE)

colnames(data)[1] <- "origin"


agg_data <- aggregate(cbind(inflows=data$inflows), by=list(origin=(data$origin), destination=(data$destination)), FUN=sum)


# adjacency matrix

countries <- unique(c(agg_data$origin, agg_data$destination))

n = length(countries)

mat <- matrix(0, nrow=n, ncol=n, dimnames=list(countries, countries))


for(i in 1:nrow(agg_data)){
    rowActor = as.character(agg_data$origin[i])
    colActor = as.character(agg_data$destination[i])
    mat[rowActor, colActor]  <- agg_data$inflows[i]
    }



###### centrality measures

library(igraph)
g  <-  graph_from_adjacency_matrix(mat, mode='directed', weighted=TRUE, diag=FALSE)



# degree

degree  <-  apply(g[], 1, sum)   
V(g)$size <- strength(g)/140000

g_del <- V(g)[strength(g)<500000]
g <- delete.vertices(g, g_del)

e_del <- E(g)[E(g)$weight<5000]
g <- delete.edges(g, e_del )

width <- E(g)$weight/550000



par(mar=c(0,0,0,0))
plot(g, 
    vertex.size=V(g)$size,
    layout=layout_with_kk,
    vertex.lab=V(g)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2
)

sort(degree)[22:26]



# closeness

g_clos = graph_from_adjacency_matrix(mat, mode='directed', diag=FALSE, weighted=TRUE)


avg_distance = closeness(g_clos)


V(g_clos)$size <- (avg_distance*5000000)
g_del_clos <- V(g_clos)[strength(g_clos)<500000]
g_clos <- delete.vertices(g_clos, g_del_clos)

e_del_clos <- E(g_clos)[E(g_clos)$weight<5000]
g_clos <- delete.edges(g_clos, e_del_clos)

width_clos <- E(g_clos)$weight/550000

par(mar=c(0,0,0,0))
plot(g_clos, 
    vertex.size=V(g_clos)$size,
    layout=layout_with_kk,
    vertex.lab=V(g_clos)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width_clos,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2
)

sort(avg_distance)[22:26]



# betweenness

g_bet = graph_from_adjacency_matrix(mat, mode='directed', diag=FALSE, weighted=TRUE)

b <- betweenness(g_bet)

V(g_bet)$size <- b

g_del_bet <- V(g_bet)[strength(g_bet)<500000]
g_bet <- delete.vertices(g_bet, g_del_bet)

g_del_bet <- E(g_bet)[E(g_bet)$weight<5000]
g_bet <- delete.edges(g_bet, g_del_bet)

width_bet <- E(g_bet)$weight/550000

par(mar=c(0,0,0,0))
plot(g_bet, 
    vertex.size=V(g_bet)$size,
    layout=layout_with_kk,
    vertex.lab=V(g_bet)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width_bet,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2
)

sort(b)[22:26]


# eigen

g_eig <- graph_from_adjacency_matrix(mat, mode='directed', diag=FALSE, weighted=TRUE)

e <- eigen_centrality(g_eig)$'vector'

V(g_eig)$size <- e*30

g_del_eig <- V(g_eig)[strength(g_eig)<500000]
g_eig <- delete.vertices(g_eig, g_del_eig)

g_del_eig <- E(g_eig)[E(g_eig)$weight<5000]
g_eig <- delete.edges(g_eig, g_del_eig)

width_eig <- E(g_eig)$weight/550000

par(mar=c(0,0,0,0))
plot(g_eig, 
    vertex.size=V(g_eig)$size,
    layout=layout_with_kk,
    vertex.lab=V(g_eig)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width_eig,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2
)

sort(e)[22:26]



###### Graph all 4

par(mar=c(0,0,2,0))
par(mfrow=c(2, 2))
plot(g, 
    vertex.size=V(g)$size,
    layout=layout_with_kk,
    vertex.lab=V(g)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Degree"
)

plot(g_clos, 
    vertex.size=(V(g_clos)$size*.6),
    layout=layout_with_kk,
    vertex.lab=V(g_clos)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width_clos,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Closeness"
)

plot(g_bet, 
    vertex.size=V(g_bet)$size,
    layout=layout_with_kk,
    vertex.lab=V(g_bet)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width_bet,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Betweenness"
)

plot(g_eig, 
    vertex.size=V(g_eig)$size,
    layout=layout_with_kk,
    vertex.lab=V(g_eig)$label,
    vertex.color="light blue",
    vertex.frame.color="light blue",
    vertex.label.color="black",
    vertex.label.cex=1,
    edge.width=width_eig,
    edge.curved=.25,
    edge.color="grey60",
    asp=1.3,
    edge.arrow.size=0.2,
    main="Eigenvector"
)

