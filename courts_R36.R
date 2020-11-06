library(stringr)
library(rvest)
library(dplyr)
library(tidyr)
library(ggnetwork)
library(ggthemes)
library(network)
library(sna)

#the post_war network is on github as is a list of justices and their codes
#this data is from the WashU SCOTUS project
#I can pipe you in more data as well

#this would be a clear way to make this work
post_war%>%
  filter(term>2010)

#or you can use the dissensus version that excludes 9-0 opinions

#create a basic network
C<-network(post_war, directed = TRUE, matrix.type="edgelist")

#add edge type
#special is concurrences and special concurrences - other stuff omitted
set.edge.attribute(C, attrname="TYPE", post_war$TYPE)

#adds the year of the decision
set.edge.attribute(C, attrname="TERM", post_war$term)

#quick layout
L<-ggnetwork(C, layout = "eigen")
#gets rid of all the dots that don't actiually link 
#point of interest, 70 is former president taft
J<-filter(L, vertex.names>70)

#make a quick plot of that
ggplot(J, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(aes(color = TYPE))+
  #set the other aesthetics as static values
  geom_nodetext(aes(label = vertex.names))+
  theme_blank()


#a number of the modularity formulas require adjacency matricies, so we quick convert to that
smasher<-as.matrix.network.adjacency(C)
#calculate modularities - detect groups and clumps and stuff
#a fav that comes up null

#a table of centralities - you know the first three
close<-closeness(smasher)
between<-betweenness(smasher)

#bonacic power centrality - recursive formula - power from power of alters
bonacic<-bonpow(C)

#prestige measures - both rescaled
prestige_degree<-prestige(smasher, cmode="indegree", rescale=TRUE)
prestige_eigen<-prestige(smasher, cmode="eigenvector", rescale=TRUE)

#centrality table
justice_centralities<-data.frame(close, between, prestige_eigen,
                                 prestige_degree, bonacic)

#modularities
#louvain- this is a poor implementation of Louvain, but it works with the libraries
louvain_net<-modMax::greedy(smasher)
#kcores - older method of modularity, falls back to degree centrality 
kcores_net<-kcores(C)
#a method that implements CNM that refines based on commmonly placed nodes
msgvm_net<-modMax::msgvm(smasher, initial=c("general"), parL=50)
#coarsening levels
mome_net<-modMax::mome(smasher)

#cleans up that dataframe
modularities<-data.frame(louvain_net[3], kcores_net, msgvm_net[3], mome_net[3])
colnames(modularities)[1]<-"Louvain"
colnames(modularities)[4]<-"msgvm"
colnames(modularities)[5]<-"mome"
View(modularities)

C %v% "louvain" <- louvain_net[3]
C %v% "kcores" <- kcores_net
L<-ggnetwork(C, layout = "fruchtermanreingold")
J<-filter(L, vertex.names>75)

ggplot(J, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(aes(linetype = TYPE))+
  #set the other aesthetics as static values
  geom_nodetext(aes(colour = as.factor(kcores), label = vertex.names))+
  theme_blank()

#further fun
cutpoints(C)
mutuality(C)
court_brokerage<-brokerage(C, modularities$Louvain)
