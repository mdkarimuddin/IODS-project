#Reading the data

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)


hd <- hd %>% 
  rename(
    HDI = Human.Development.Index..HDI.,
    GNIperCap = Gross.National.Income..GNI..per.Capita,
    GNIrank_HDIrank = GNI.per.Capita.Rank.Minus.HDI.Rank,
    EYE = Expected.Years.of.Education,
    LEB = Life.Expectancy.at.Birth,
    MYE = Mean.Years.of.Education
  )


gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")


gii <- gii %>% 
  rename(GII = Gender.Inequality.Index..GII.,
         MMR = Maternal.Mortality.Ratio,
         ABR = Adolescent.Birth.Rate,
         parRep = Percent.Representation.in.Parliament,
         edu2F = Population.with.Secondary.Education..Female.,
         edu2M = Population.with.Secondary.Education..Male.,
         labF = Labour.Force.Participation.Rate..Female.,
         labM = Labour.Force.Participation.Rate..Male.
  )


library(dplyr)
library(tidyverse)
colnames(gii)
#define a new column edu2F_edu2M 
gii <- mutate(gii, edu2F_edu2M = edu2F/edu2M)
#define a new column edu2F_edu2M 

gii <- mutate(gii, labF_labM =labF/labM)

#Merge dataset

human <- inner_join(hd, gii, by = "Country")

# look at the (column) names of human
names(human)

# look at the structure of human

str(human)
# print out summaries of the variables
summary(human)

write.table(human, file = "human.csv", sep=",")

# read the human data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)



# look at the (column) names of human
names(human)

# look at the structure of human

str(human)
# print out summaries of the variables
summary(human)


#String manipulation

library(stringr)
library(tidyverse)
# look at the structure of the GNI column in 'human'

str(human$GNI)
# remove the commas from GNI and print out a numeric version of it


human$GNI = str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric

str(human$GNI)
str(human)

#Dealing with not available (NA) values

# human with modified GNI and dplyr are available

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")


library(dplyr)
library(corrplot)


#Columns to keep

human <- human[keep]


#Drop the NA values
human_ <-human%>%drop_na()
# filter out the the last rows of data
last<-nrow(human_)-7
#Choose everything untill last 7 rows
human_ <- human_[1:last, ]

rownames(human_) <- human_$Country

# remove the Country variable
human_ <- select(human_, -Country)

human<-human_

write.table(human, file = "human.csv", sep=",")



