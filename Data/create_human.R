#Reading the data

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")
library(dplyr)
library(tidyverse)
colnames(gii)
#define a new column edu2F_edu2M 
gii <- mutate(gii, edu2F_edu2M = Population.with.Secondary.Education..Female. /Population.with.Secondary.Education..Male. )

#define a new column edu2F_edu2M 

gii <- mutate(gii, labF_labM = Labour.Force.Participation.Rate..Female./ Labour.Force.Participation.Rate..Male.)


#Merge dataset

human <- inner_join(hd, gii, by = "Country")

# look at the (column) names of human
names(human)

# look at the structure of human

str(human)
# print out summaries of the variables
summary(human)

write.table(human, file = "human.csv", sep=",")


#String manipulation

library(tidyr)
library(stringr)


# tidyr package and human are available

# access the stringr package
library(stringr)

# look at the structure of the GNI column in 'human'

str(human$GNI)
# remove the commas from GNI and print out a numeric version of it
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric



#Dealing with not available (NA) values

# human with modified GNI and dplyr are available

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))



# human without NA is available

# look at the last 10 observations
tail(human, 10)

# last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human_ <- human[1:last, ]

# add countries as rownames
rownames(human) <- human$Country


# modified human, dplyr and the corrplot functions are available

# remove the Country variable
human_ <- select(human, -Country)

# Access GGally
library(GGally)

# visualize the 'human_' variables
ggpairs(human_)

# compute the correlation matrix and visualize it with corrplot
cor(human_)%>%corrplot()



