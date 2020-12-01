# Md Karim Uddin, 02.11.2020

# Read the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
write.table(BPRS, file = "BPRS.csv", sep=",")
# Look at the (column) names of BPRS
names(BPRS)

# Look at the structure of BPRS
str(BPRS)

# Print out summaries of the variables
summary(BPRS)

# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)


# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

# Take a glimpse at the BPRSL data
glimpse(BPRSL)

write.table(BPRSL, file = "BPRSL.csv", sep=",")

# dplyr is available

# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')
write.table(RATS, file = "RATS.csv", sep=",")
# Factor variables ID and Group
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Glimpse the data
glimpse(RATS)

# dplyr, tidyr and RATS are available

# Convert data to long form
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 

# Glimpse the data
glimpse(RATSL)


# dplyr, tidyr and RATSL are available

# Check the dimensions of the data

dim(RATSL)
write.table(RATSL, file = "RATSL.csv", sep=",")

