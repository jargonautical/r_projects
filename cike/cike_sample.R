# read in the dataset
mydata <- read.csv("~/R projects/cike/pop_sk.csv", header = TRUE)
# view the dataset
mydata
# look at the column headings
names(mydata)
# look at the dimensions (number of rows and columns)
dim(mydata)
# the type of dataset
class(mydata)
#look at the first few rows
head(mydata, 5)
# summary statistics
summary(mydata)
# a simple bar chart
barplot(height=mydata$X1996, names=mydata$Age)
# remove a row
# data_frame = data_frame[-1,]
mydata = mydata[-1,]
# ----------------------------------------------------------------#
# needs ggplot library (assuming you have it installed)
library(ggplot2)

# requires the data to be structured a little differently
mydata <- read.csv("~/R projects/cike/pop_sk2.csv", header = TRUE)

# Stacked
ggplot(mydata, aes(fill=Age, y=Value, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="none")

# stacked as %
ggplot(mydata, aes(fill=Age, y=Value, x=Year)) + 
  +     geom_bar(position="fill", stat="identity") +
  +     theme(legend.position="none")

# whole dataset - 24 years
mydata <- read.csv("~/R projects/cike/pop_sk3.csv", header = TRUE) 

# box plot
ggplot(boxdata, aes(x=as.factor(Year), y=Value)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Year")

# small multiples
ggplot(data=boxdata, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Year) +
  ggtitle("Small Multiples in R") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90)) 

# as.numeric(factor(iris$Species))

# "p" - points
# "l" - lines
# "b" - both points and lines
# "c" - empty points joined by lines
# "o" - overplotted points and lines
# "s" and "S" - stair steps
# "h" - histogram-like vertical lines
# "n" - does not produce any points or lines

plot((mydata$age_num, mydata$Value)
     main="my chart"
     type="h")

plot(x, sin(x),
     main="The Sine Function",
     ylab="sin(x)",
     type="h")

with(mydata[mydata$Year == "2008",], 
     plot(age_num, Value),
     main="my chart",
     type="h")

plot((mydata$age_num, mydata$Value),
     filter(mydata$Year == "1996"),
     main="my chart",
     type="l")