#library list
library(sna)
library(network)
library(ggplot2)
library(dplyr)
library(ggnetwork)

#The Road...
#produce a network

R<-network(road, directed = FALSE, matrix.type = "edgelist")

#node attributes
set.vertex.attribute(R, "between", betweenness(R))
set.vertex.attribute(R, "close", closeness(R))
set.vertex.attribute(R, "count", degree(R))

#about that edge
set.edge.attribute(R, "where", road$WHERE)

#target layout - arranged by degree, labels offset 5 from dots, outliers way out side
L<-ggnetwork(R, layout = "target", "between", circ.lab.offset=5, periph.outside=TRUE)

#eigenvector based layourt using the strong assumptions, you can switch to weak if you want
K<-ggnetwork(R, layout = "eigen", "symstrong")

#area is size, cool controls Simulated Annealing - bigger numbers bigger crystals 
P<-ggnetwork(R, layout = "fruchtermanreingold", area = 100, cool=2)

#a physical simulation of a spring process - kilograms per newton meter 
#mass is in KG - heavy like bowling balls
#equi is when springs and mass balance
#repe
#kfr is friction - I have it turned down....
#repulsive forces are turned on...
P<-ggnetwork(R, layout = "spring", mass=5, equil=.1, repeqdis=1, kfr = .001, repulse=TRUE)

#highway plots
ggplot(P, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(colour=where)) +
  geom_nodes(aes(size = between, alpha = 1/count)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  theme(legend.position = "bottom")

#overall centralization
centralization(R, betweenness)

#lets make a table
highway_data<-data.frame(
  road=get.vertex.attribute(R, "vertex.names"),
  degree=degree(R, cmode="indegree"),
  between=betweenness(R),
  close=closeness(R),
  prestige=prestige(R, cmode = "eigenvector", rescale=TRUE),
  hybrid=infocent(R)
)

#head into groups...



#what if you want to cut it...
cutpoints(R)

#the greys world....





#feeeet ball

#import conf_clean - this now joins smoothly for all team data about FCS and FBS ball
#foot_meta is in memory - this is scores, dates, times, etc...
#clean_football is a pure edge list
footy<-network(clean_football, directed=TRUE, matrix.type="edgelist")

#now we want to add some information about teams
#these are node properties

#these libraries are really sensitive to ordering - so join now
intermediate<-get.vertex.attribute(footy, "vertex.names")
advanced<-data.frame(vertex.names=intermediate)
conference_clean<-inner_join(advanced, conf_data)


footy%v%"conference"<-conference_clean$CurrentConference
footy %v% "nickname" <- conference_clean$Nickname
footy %v% "size" <- conference_clean$Enrollment
footy %v% "nickname" <- conference_clean$Level
footy %v% "age" <- conference_clean$FirstPlayed
footy %v% "state" <- conference_clean$State.1.

#it's edge time
set.edge.attribute(footy, attrname = "score", (as.numeric(foot_meta$Pts)-as.numeric(foot_meta$Pts.1)))
set.edge.attribute(footy, attrname = "date", foot_meta$Date)
set.edge.attribute(footy, attrname = "time", foot_meta$Time)
set.edge.attribute(footy, attrname = "day", foot_meta$Day)


#this one is just supposed to be pretty
ggplot(footy, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = date)) +
  geom_nodes(aes(colour = conference, size=score)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  theme(legend.position = "bottom")







