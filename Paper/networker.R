

networker <- function(data, start, end) {


grapher <- function(thing){

title <- paste0("Foreign Lobbying: ", start, ' to ', end)

par(mar=c(0,0,1,0))
plot_g <- plot(thing,
    layout=layout_as_bipartite,
    vertex.label="",
    edge.width= E(g)$size,
    vertex.size=V(g)$size,
    vertex.color=V(g)$color,
    edge.arrow.size=0,
    main= title,
    )
}



# subset dataset
data <- data_raw[,c('country', 'startyear', 'endyear', 'Registrant')]
data <- data[data$startyear >= start & data$startyear <= end,]

# aggregate
data$duration <- (data$endyear - data$startyear) + 1

data <- aggregate(cbind(hire_years=data$duration), by=list(country=data$country, firm=data$Registrant), FUN=sum)





##### ADD VARIABLES

## COW
data$COW <- suppressWarnings(countrycode(data$country, "country.name", "cown"))
# suppress non-matched countries and then drop them
data <- data[!is.na(data$COW),]


## FARISS


# average fariss over time period
fariss <- na.omit(fariss[fariss$year >= start & fariss$year <= end,])

fariss <- aggregate(cbind(human_rights=fariss$latentmean), by=list(COW=fariss$COW), FUN=mean)





## PREPARE DATA

data_merged <- merge(fariss, data, by ="COW")


data_for_mat <- data.frame(COW=data_merged$COW, firm=data_merged$firm, hire_years=data_merged$hire_years, fariss=data_merged$human_rights)



##### ADJACENCY MATRIX AND NETWORK PLOT


g <- graph_from_data_frame(data_for_mat, directed=TRUE)

bipartite.mapping(g)


V(g)$type <- bipartite_mapping(g)$type

cows <- V(g)[V(g)$type==FALSE]$name
cows <- as.numeric(cows)


for(i in cows){
    suppressWarnings( # warnings are due to non-country nodes not receiving pts
    V(g)[V(g)$name == i]$fariss <- data_for_mat$fariss[data_for_mat$COW == i])    
}




e_del <- E(g)[E(g)$hire_years <= 1]
g <- delete.edges(g, e_del)

g_del <- V(g)[strength(g) <= 2 & V(g)$type==TRUE]
g <- delete.vertices(g, g_del)

g_del <- V(g)[strength(g) <= 0 & V(g)$type==FALSE]
g <- delete.vertices(g, g_del)


probs = quantile(V(g)$fariss, probs = seq(0, 1, by= 0.25), na.rm=TRUE)


grad <- brewer.pal(n=4, name="RdYlGn")
V(g)$color[between(V(g)$fariss, probs[1], probs[2])] <- grad[1]
V(g)$color[between(V(g)$fariss, probs[2], probs[3])] <- grad[2]
V(g)$color[between(V(g)$fariss, probs[3], probs[4])] <- grad[3]
V(g)$color[between(V(g)$fariss, probs[4], probs[5])] <- grad[4]

E(g)$size <- rescale(E(g)$hire_years, to= c(0.2, 2))
V(g)$size <- rescale(strength(g), to= c(5, 20))




##### STATS



### Netork Stats

## top five out degree centrality (firms)
firm_degree5 <- sort(apply(g[], 2, sum), decreasing=TRUE)[1:5]

## top five in degree centrality (countries): unweighted and weighted
country_degree5 <- sort(apply(g[], 1, sum), decreasing=TRUE)[1:5]
    
# revert to country name
names(country_degree5) <- countrycode(names(country_degree5), 
    origin='cown', destination='country.name')


## eigenvector centrality (firms)
firm_eig5 <- sort(eigen_centrality(g)$'vector'[V(g)$type==TRUE], decreasing=TRUE)[1:5]

## eigenvector centrality (firms)
country_eig5 <- sort(eigen_centrality(g)$'vector'[V(g) $type==FALSE], decreasing=TRUE)[1:5]

# revert to country name
names(country_eig5) <- countrycode(names(country_eig5), 
    origin='cown', destination='country.name')



### Human Rights Score by firm

# calculate firm scores
firm_list = unique(data_merged$firm)
firm_scores <- matrix(NA, nrow=length(firm_list), ncol=2, dimnames=list(NULL,c("firm", "score")))
firm_scores <- as.data.frame(firm_scores)

for(i in 1:length(firm_list)){
    slice <- data_merged[data_merged$firm == firm_list[i],]
    slice <- slice[slice$human_rights <= 1,]

    firm_scores$firm[i] <- firm_list[i]
    firm_scores$score[i] <- sum(slice$hire_years)
}

# calculate proportion score
tot_bad_yrs <- sum(firm_scores$score)
firm_scores$score_prop <- firm_scores$score/tot_bad_yrs

# top five worst firms
worst_offenders <- firm_scores[order(-firm_scores$score),]
top5 <- worst_offenders[1:5,]

sum(top5$score_prop)    # prop of 'bad' business done by top 5 firms

 

##### LIST IT


firm_stats <- cbind(
    "Firm Degree"= paste0(names(firm_degree5), " (", round(firm_degree5, 2), ")"), 
    "Firm Eigenvector" =paste0(names(firm_eig5), " (", round(firm_eig5, 2), ")")
    )

country_stats <- cbind(
    "Country Degree"= paste0(names(country_degree5), " (", round(country_degree5, 2), ")"), 
    "Country Eigenvector" =paste0(names(country_eig5), " (", round(country_eig5, 2), ")")
    )

top5 <- top5

#grapher(thing=g)

output <- list(firm_stats, country_stats, top5)

return(output)

}
