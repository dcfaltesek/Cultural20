#Core Skills for Unit One
#import a dataset
#tvratings
distinct(tvratings)
#import relevant packages for your work
library(dplyr)
library(ggplot2)

#Write a filter
#just flag day
tvratings%>%
  filter(Year==1988)%>%
  filter(Rating>20)

#there are multiple right answers here...


#Use select to make a table managible
tvratings%>%
  select(Program, Rating)

#Write a summary function 
tvratings%>%
  group_by(Network, Year)%>%
  summarize(Performance=mean(Rating, na.rm= TRUE))

#calculate something across a window
raw_ratings<-tvratings%>%
  mutate("total"=Rating*1.21)

View(raw_ratings)

#plot that
ggplot(raw_ratings, aes(Year, total, colour=Network))+geom_point()
  
  #check to see if two things are related...
cor.test(raw_ratings$total, raw_ratings$Rank)
#Turns out they are, also that is significant

#Produce an appropriate discrete plot
z<-ggplot(tvratings, aes(Type, Rating, colour=Network))+geom_boxplot()

#easy flips and such
z+coord_polar()
z+coord_flip()

#produce an appropriate continuous plot
ggplot(tvratings, aes(Rating))+geom_freqpoly()+facet_grid(~Type)

#demonstrate control of advanced plot aesthetics including color, labels
tvratings%>%
  filter(Rank>5)%>%
  ggplot(aes(Year, Rating, group=Type)) +
  geom_line(aes(linetype="dotted", color=Network, size=2))+
  geom_point(aes(color=Network, size=3))+
  scale_colour_brewer(palette = "Greens")+
  facet_grid(~Network)+
  ggtitle("We Only Come Out At Night")+xlab("when")

#and you need to export stuff
write.csv(tvratings, "tvratings.csv", row.names = FALSE)
