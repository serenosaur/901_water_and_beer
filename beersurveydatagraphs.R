library(tidyr)
library(ggplot2)
library(ggrepel)

#read in file
beerdata <- read.csv(file.choose(), header=TRUE)
#rename the first column
names(beerdata)[names(beerdata) == 'ï..'] <- 'Breweries'

#remove the Year column
beerdatascores <- beerdata[,c(1,3,4,5,6,7,8,9,10,11,12,13)]

#create a tidier version of the data plot (see tidyr documentation for more details)
bdstidy <- gather(beerdatascores[,1:11], measure, score, 2:11)
#reorder the measure levels. This adjusts the order of the measures in the plot legend.
bdstidy$measure <- factor(bdstidy$measure, levels=c("Brewery.Name","Beer.names","Label.or.logo.imagery",
    "Water","Energy","Waste","Great.Lakes","Formal.Program","Employee.volunteerism.or.charity","Local.business.engagement"))

#create the bar graph
ggplot(data = bdstidy, aes(x = Breweries, y = score, fill = measure)) + 
  geom_col() + coord_flip()
  
#omit Miller and calculate average scores for each category as well as the total average score
bdsnomiller <- beerdatascores[2:20,]
bdsnomiller$local.avg <- rowMeans(bdsnomiller[,c(2,3,4)])
bdsnomiller$environmental.avg <- rowMeans(bdsnomiller[,c(5,6,7,11)])
bdsnomiller$community.avg <- rowMeans(bdsnomiller[,c(8,9,10)])
bdsnomiller$average.score <- rowMeans(bdsnomiller[,c(2,3,4,5,6,7,8,9,10,11)])
#add the Year column back in 
bdsnomiller$year.founded <- beerdata[2:20,2]

#graph the time vs avg score plot
ggplot(data=bdsnomiller, aes(x=year.founded, y=Total, color=environmental.avg )) + geom_point(size=3) + 
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=4)) + geom_label_repel(aes(label=Breweries)) +theme(legend.position="bottom") #+ scale_color_gradient(low="black", high="green") 
