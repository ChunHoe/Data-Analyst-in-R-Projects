library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

#Load file
forestfires <- read_csv("~/CHL/DataQuest R/forestfires.csv")
View(forestfires)

#Filter per month and per day
month<-forestfires%>%group_by(month)%>%summarize(total_fire_per_month=n())
view(month)
day<-forestfires%>%group_by(day)%>%summarize(total_fires_per_day=n())
View(day)
ggplot(data=month)+aes(x=month,y=total_fire_per_month)+geom_bar(stat = "identity")
ggplot(data=day)+aes(x=day,y=total_fires_per_day)+geom_bar(stat='identity')

#Fix month and day sequence
forestfires<-forestfires%>%mutate(month=factor(month,levels=c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))
forestfires<-forestfires%>%mutate(day=factor(day,levels=c("mon","tue","wed","thu","fri","sat","sun")))
month2<-forestfires%>%group_by(month)%>%summarise(fire_per_month=n())
day2<-forestfires%>%group_by(day)%>%summarize(fire_per_day=n())
ggplot(data=month2)+aes(x=month,y=fire_per_month)+geom_bar(stat="identity")+labs(title="Fire by Month")
ggplot(data=day2)+aes(x=day,y=fire_per_day)+geom_bar(stat="identity")+labs(title="Fire by Day")

#Boxplot
column_names<-colnames(forestfires)
boxplot_variables<-column_names[5:13]
print(boxplot_variables)

create_boxplot=function(x,y){ggplot(data=forestfires)+aes_string(x=x,y=y)+geom_boxplot()+labs(title=y)}
boxplot_per_day<-map2("day",boxplot_variables,create_boxplot)
print(boxplot_per_day)
boxplot_per_month<-map2("month",boxplot_variables,create_boxplot)
print(boxplot_per_month)

#Scatterplot
scatter_variables<-boxplot_variables[1:8]
create_scatter=function(x,y){ggplot(data=forestfires)+aes_string(x=x,y=y)+geom_point(alpha=0.3)+labs(title=x)}
scatter_per_area<-map2(scatter_variables,"area",create_scatter)
print(scatter_per_area)

#Histogram Area
histogram_area<-ggplot(data=forestfires)+aes(x=area)+geom_histogram(bins=30)
print(histogram_area)



