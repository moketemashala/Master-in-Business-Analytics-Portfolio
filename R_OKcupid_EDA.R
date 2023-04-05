#' Author: Mokete Mashala
#' Date: March-13-2023
#' Purpose: Assignment A1_OKCupid Case
#' Professor: Ted Kwartler

# Setwd
setwd("C:/PERSONAL/02. Hult MBAN-DUAL Degree/12. Visializing Data with R/Hult_Visualizing-Analyzing-Data-with-R/personalFilesA1")
options(scipen=999)


# Libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(forcats)
library(stringr)
library(lubridate)
library(tidyr)
library(wordcloud)
library(tm)
library(corrplot)
library(ggmap)
library(maps)
library(ggthemes)
library(leaflet)
library(mapproj)
library(reshape2)
library(cluster)
library(rattle)
library(NbClust)


# Options - this is to turn off scientific notation
options(scipen = 999)

#References:
# https://rstudio-pubs-static.s3.amazonaws.com/209370_b62220c849b946088b463fdbec935848.html
# https://www.pluralsight.com/guides/visualization-text-data-using-word-cloud-r
# https://www.rdocumentation.org/packages/ggplot2/versions/3.4.1/topics/ggtheme

# Read csv files
profiles <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/profiles.csv')
latlon <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/LatLon.csv')
addr <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/addr.csv')
sharedcensus <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/sharedCensus2010Vars.csv')


############################################################################################
############################# HIGH LEVEL VIEW OF DATA  #####################################
############################################################################################

#dimensions of the data frame
dim(profiles)          # profiles has 59946 observations and 22 variables
dim(latlon)            # latlon has 199 observations and 3 variables
dim(addr)              # dim(addr) has 199 observations and 5 variables
dim(sharedcensus)      # sharedcensus has 199 observations and 464 variables

# view the structure of the data frame, number or rows and columns, data types etc.
str(profiles)
str(latlon)
str(addr)
str(sharedcensus)

#glimpse of the data to check data types
options(dplyr.width = Inf)  #this is to make sure all data will be displayed
glimpse(profiles)
glimpse(latlon)
glimpse(addr)
glimpse(sharedcensus)

# view head
head(profiles)
head(latlon)
head(addr)
head(sharedcensus)

#view tail
tail(profiles)
tail(latlon)
tail(addr)
tail(sharedcensus)

#summary of data
summary(profiles)
summary(profiles$age)
summary(profiles$income)
summary(latlon)
summary(addr)
summary(sharedcensus)


#observe the columns of data for each file
profiles %>% names
latlon %>% names
addr %>% names
sharedcensus %>% names

#check for null values
colSums(is.na(profiles))

#Observations with NAs are:
# body_type        diet      drinks       drugs      education   ethnicity      height 
# 5296             24395     2985         14080      6628        5680           3 

#income         job      offspring        pets          religion 
# 48442        8198        35561          19921          20226 

#        sign      smokes      speaks           essay0 
#      11056        5512          50            5485 

colSums(is.na(latlon))
colSums(is.na(addr))
# postalCode       city     county      state 
#      14         18         16         14 

colSums(is.na(sharedcensus))


############################################################################################
######################   CLEAN DATA AND CHANGE DATA TYPES  #################################
############################################################################################

#############################
###### PROFILES _ AGE #######
#############################

#Do relabeling
profiles$sex <- factor(profiles$sex, labels = c("Female", "Male"))


# analyse the age column
# find unique values
unique(profiles$age)

#frequency table
table(profiles$age)

#histogram
ggplot(data = profiles, aes(x = age)) +
  geom_histogram(bins = 30, fill = "purple", color = 'white') +
  scale_x_continuous(breaks = seq(18, 70, by = 5))+
  labs(x = 'Age', y= 'Number of Users', title = 'Distribution of Age')

#boxplot
boxplot(profiles$age, 
        main="Age Distribution", 
        xlab="Age", 
        ylab="Frequency",
        col="lightblue")


#check for the percentage of age that is over 80
num_over_80 <- nrow(filter(profiles, age > 80))

#calculate percentage of age over 80
percent_over_80 <- num_over_80 / nrow(profiles) * 100

# print percentage
cat(sprintf("Percentage of age over 80: %.4f%%\n", percent_over_80))

# The percentage of age over 80 is 0.0033%
# Filter out for ages over 80
profiles <-  profiles %>%
  filter(age <= 80)

#boxplot _ check again
boxplot(profiles$age, 
        main="Age Distribution (Excluding over 80", 
        xlab="Age", 
        ylab="Frequency",
        col="lightblue")


#histogram check again after removing outliers
ggplot(data = profiles, aes(x = age)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(x = "Age", y= "Number of Users", title = "Distribution of Age") +
  facet_wrap(~ sex, nrow = 2)

# *********************************************************

#boxplot
ggplot(data = filter(profiles, age <= 80), aes(x = sex, y = age, fill = sex)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  labs(x = "Gender", y = "Age", title = "Distribution of Age by Gender", fill = "Gender")


####################################
###### PROFILES _ BODYTYPE #########
####################################

# find unique values
unique(profiles$body_type)  # there are 13 unique body types

#frequency table
table(profiles$body_type)

# NA values
sum(is.na(profiles$body_type))
#there are 5295 observations with NA

#find the percentage of NA bodytypes
na_count_body_type <- sum(is.na(profiles$body_type))
total_count <- nrow(profiles)
percent_na_body_type <- na_count_body_type / total_count * 100

# print percentage, there are 8.83% of NA body_types
cat(sprintf("Percentage of NA values for body type: %.2f%%\n", percent_na_body_type))

#frequency table. The average is the most common body_type. Therefore replace NA with most common
table(profiles$body_type)
sort(table(profiles$body_type), decreasing = TRUE)

#replace NA with average body_type
profiles$body_type <- ifelse(is.na(profiles$body_type), "average", profiles$body_type)

# NA values. The data is clean. There are no more NA values for body_type
sum(is.na(profiles$body_type))

# ******************************************************
freq <- table(profiles$body_type)
barplot(freq, main="Body Type Distribution", xlab="Body Type", ylab="Frequency") 


# ******************************************************

#barplot check again after removing outliers
ggplot(data = profiles, aes(x = body_type)) +
  geom_bar() +
  labs(x = "Body_Type", y= "Number of Users", title = 'Distribution of Body_Type') +
  facet_wrap(~ sex, nrow = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ******************************************************

plotDF <- data.frame(table(profiles$sex,  profiles$body_type))

ggplot(data = plotDF, aes(fill=reorder(Var2, Freq), y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Body Type") +
  labs(title = "Body Type Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  guides(fill = guide_legend(reverse = FALSE, order = 2))


####################################
###### PROFILES _ DIET #############
####################################

# find unique values
unique(profiles$diet)  # there are 18 unique diet preferences

#frequency table
table(profiles$diet)

# NA values
sum(is.na(profiles$diet))
#there are 24394 observations with NA

#find the percentage of NA diet
na_count_diet <- sum(is.na(profiles$diet))
total_count <- nrow(profiles)
percent_na_diet <- na_count_diet / total_count * 100

# print percentage, there are 40.69% of NA diet
cat(sprintf("Percentage of NA values for diet: %.2f%%\n", percent_na_diet))

#frequency table. The anything is the most for diet. 
table(profiles$diet)
sort(table(profiles$diet), decreasing = TRUE)


profiles$diet <- recode(profiles$diet, 
                             "mostly halal" = "halal",
                             "strictly halal" = "halal",
                             "mostly kosher" = "kosher",
                             "strictly kosher" = "kosher",
                             "strictly other" = "other",
                             "mostly vegan" = "vegan",
                             "mostly anything" = "anything",
                             "mostly other" = "other",
                             "strictly vegan" = "vegan",
                             "strictly anything" = "anything",
                             "strictly vegetarian" = "vegetarian",
                             "mostly vegetarian" = "vegetarian")

#frequency table. The anything is the most for diet. 
table(profiles$diet)
sort(table(profiles$diet), decreasing = TRUE)

#Since there number of NA is 50%, impute all NA with as 'anything'
profiles$diet <- ifelse(is.na(profiles$diet), "anything", profiles$diet)

#frequency table
table(profiles$diet)

# NA values. The data is clean. There are no more NA values for diet
sum(is.na(profiles$diet))

freq <- table(profiles$diet)
barplot(freq, main = "Diet Distribution", xlab = "Diet", ylab = "Frequency",
        names.arg = levels(factor(profiles$diet)),
        las = 2)

#barplot check again after adding the 'unknown' category
ggplot(data = profiles, aes(x = diet, fill = sex)) +
  geom_bar() +
  labs(x = 'Diet', y= 'Number of Users', title = 'Distribution of Diet') +
  facet_wrap(~ sex, nrow = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values = c("pink", "lightblue"))

# ******************************************************

plotDF <- data.frame(table(profiles$sex,  profiles$diet))

ggplot(data = plotDF, aes(fill=reorder(Var2, Freq), y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Diet") +
  labs(title = "Diet Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(reverse = TRUE, order = 2))


####################################
###### PROFILES _ DRINKS #########
####################################

# find unique values
unique(profiles$drinks)  # there are 6 unique drinks

#frequency table
table(profiles$drinks)

# NA values
sum(is.na(profiles$drinks))
#there are 2983 observations with NA

#find the percentage of NA drinks
na_count_drinks <- sum(is.na(profiles$drinks))
total_count <- nrow(profiles)
percent_na_drinks <- na_count_drinks / total_count * 100

# print percentage, there are 4.98% of NA drinks
cat(sprintf("Percentage of NA values for drinks: %.2f%%\n", percent_na_drinks))

#frequency table. Socially is the most common category for drinks. Therefore replace NA with most common
table(profiles$drinks)
sort(table(profiles$drinks), decreasing = TRUE)

#replace NA with average drinks
profiles$drinks <- ifelse(is.na(profiles$drinks), "socially", profiles$drinks)

# NA values. The data is clean. There are no more NA values for drinks
sum(is.na(profiles$drinks))

freq <- table(profiles$drinks)
barplot(freq, main="Drinks Distribution", xlab="Drinking Habits", ylab="Frequency") 

#barplot check again after replacing values
ggplot(data = profiles, aes(x = drinks, fill = sex)) +
  geom_bar() +
  labs(x = 'Drinking Habits', y= 'Number of Users', title = 'Distribution of Drinks') +
  facet_wrap(~ sex, nrow = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("pink", "lightblue"))


# ******************************************************
#plot Drinking Habits distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$drinks))

ggplot(data = plotDF, aes(fill=reorder(Var2, Freq), y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Drinks") +
  labs(title = "Drinking Habits Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(reverse = TRUE, order = 2))
  

####################################
###### PROFILES _ DRUGS#############
####################################

# find unique values
unique(profiles$drugs)  # there are 3 unique body types

#frequency table
table(profiles$drugs)

# NA values
sum(is.na(profiles$drugs))
#there are 14079 observations with NA

#find the percentage of NA drugs
na_count_drugs <- sum(is.na(profiles$drugs))
total_count <- nrow(profiles)
percent_na_drugs <- na_count_drugs / total_count * 100

# print percentage, there are 23.49% of NA drugs
cat(sprintf("Percentage of NA values for drugs: %.2f%%\n", percent_na_drugs))

#frequency table. Never is the most for drugs. 
table(profiles$drugs)
sort(table(profiles$drugs), decreasing = TRUE)

#Since there number of NA is 23.49%, rather group all NA with as 'never'
profiles$drugs <- ifelse(is.na(profiles$drugs), "never", profiles$drugs)

#frequency table
table(profiles$drugs)

# NA values. The data is clean. There are no more NA values for drugs
sum(is.na(profiles$drugs))

freq <- table(profiles$drugs)
barplot(freq, main = "Drugs Distribution", xlab = "Uses Drugs", ylab = "Frequency",
        names.arg = levels(factor(profiles$drugs)))

#barplot check again after adding the 'unknown' category
ggplot(data = profiles, aes(x = drugs, fill = sex)) +
  geom_bar() +
  labs(x = "Uses Drugs", y= "Number of Users", title = "Distribution of use of Drugs") +
  facet_wrap(~ sex, nrow = 2) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values = c("pink", "lightblue"))


# ******************************************************
#plot drugs distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$drugs))

ggplot(data = plotDF, aes(fill=reorder(Var2, Freq), y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Drugs") +
  labs(title = "Use of drugs Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(reverse = TRUE, order = 2))


####################################
###### PROFILES _ EDUCATION ########
####################################

# find unique values
unique(profiles$education)  # there are 33 unique education observations

#frequency table
table(profiles$education)

# NA values
sum(is.na(profiles$education))
#there are 6627 observations with NA

#find the percentage of NA education
na_count_education <- sum(is.na(profiles$education))
total_count <- nrow(profiles)
percent_na_education <- na_count_education / total_count * 100

# print percentage, there are 11.06% of NA education
cat(sprintf("Percentage of NA values for education: %.2f%%\n", percent_na_education))

#frequency table. The anything is the most for education. 
table(profiles$education)
sort(table(profiles$education), decreasing = TRUE)

#Since there number of NA is 50%, rather group all NA with as 'graduated from college/university'
profiles$education <- ifelse(is.na(profiles$education), "graduated from college/university", profiles$education)

#frequency table
table(profiles$education)

# NA values. The data is clean. There are no more NA values for education
sum(is.na(profiles$education))

#barplot
freq <- table(profiles$education)
barplot(freq, main = "Education Distribution", ylab = "Frequency",
        names.arg = levels(factor(profiles$education)),
        las = 2)


# The descriptions are too long. therefore suggest to clean them and shorten them
profiles$education <- recode(profiles$education, 
                             "working on college/university" = "in progress college",
                             "working on space camp" = "in progress space camp",
                             "graduated from space camp" = "space camp",
                             "graduated from masters program" = "masters",
                             "graduated from college/university" = "bachelor",
                             "working on two-year college" = "in progress college",
                             "NA" = "unknown",
                             "graduated from high school" = "high school",
                             "working on ph.d program" = "in progress ph.d",
                             "graduated from ph.d program" = "ph.d",
                             "dropped out of space camp" = "dropout space camp",
                             "college/university" = "college",
                             "dropped out of college/university" = "dropout college",
                             "graduated from two-year college" = "college",
                             "working on med school" = "in progress graduate",
                             "graduated from law school" = "law degree",
                             "space camp" = "space camp",
                             "graduated from med school" = "college",
                             "dropped out of masters program" = "drop out graduate",
                             "dropped out of high school" = "dropout high school",
                             "working on masters program" = "in progress graduate",
                             "working on high school" = "in progress high school",
                             "dropped out of law school" = "dropout college",
                             "dropped out of med school" = "dropout college",
                             "dropped out of ph.d program" = "dropout ph.d",
                             "dropped out of two-year college" = "dropout",
                             "working on law school" = "in progress college")


# find unique values
unique(profiles$education)  # there are 33 unique education observations

#frequency table
table(profiles$education)

#barplot
freq <- table(profiles$education)
barplot(freq, main = "Education Distribution",
        names.arg = levels(factor(profiles$education)),
        las = 2)


# filter out for frequency higher than 1000
freq <- table(profiles$education)
freq_filtered <- subset(freq, freq <= 1000)
barplot(freq_filtered, main = "Education Distribution", 
        names.arg = names(freq_filtered),
        las = 2)

#barplot check again after adding updating the categories
ggplot(data = profiles, aes(x = education, fill = education)) +
  geom_bar() +
  labs(x = 'Education', y= 'Number of Users', title = 'Distribution of Education') +
  facet_wrap(~ sex, nrow = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#barplot check again after adding updating the categories and filtering for count more than 1000
ggplot(data = subset(profiles, table(profiles$education)[profiles$education] > 1000), aes(x = education, fill = education)) +
  geom_bar() +
  labs(x = 'Education', y= 'Number of Users', title = 'Distribution of Education') +
  facet_wrap(~ sex, nrow = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#plot education distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$education))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Education") +
  labs(title = "Education Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw()

#########################################
###### PROFILES _ ETHNICITY #############
#########################################

# find unique values
unique(profiles$ethnicity) 

#frequency table
table(profiles$ethnicity)

# NA values
sum(is.na(profiles$ethnicity))
#there are 5678 observations with NA

#find the percentage of NA ethnicity
na_count_ethnicity <- sum(is.na(profiles$ethnicity))
total_count <- nrow(profiles)
percent_na_ethnicity <- na_count_ethnicity / total_count * 100

# print percentage, there are 9.47% of NA ethnicity
cat(sprintf("Percentage of NA values for ethnicity: %.2f%%\n", percent_na_ethnicity))

#frequency table. White is the most frequent  ethnicity. 
table(profiles$ethnicity)
sort(table(profiles$ethnicity), decreasing = TRUE)

#Since there number of NA is 9.47%, rather group all NA with as 'white'
profiles$ethnicity <- ifelse(is.na(profiles$ethnicity), "white", profiles$ethnicity)


# NA values. The data is clean. There are no more NA values for drugs
sum(is.na(profiles$ethnicity))


#frequency table
table(profiles$ethnicity)

# One way to analyse this is to strip the individual descriptors and then evaluate 
# for the most frequent descriptor.

#### step 1: split each observation by "," to get a list of categories
enthicity_list <- strsplit(profiles$ethnicity, ",")
#enthicity_list <- strsplit(profiles$ethnicity, " / ")
enthicity_list

#### Step 2: flatten the list of categories
enthicity_flat <- unlist(enthicity_list)
enthicity_flat

#### step 3: count frequency of each category
enthicity_freq <- table(enthicity_flat)

### Step 4.1: barplot
barplot(enthicity_freq, main = "Frequency of Categories", xlab = "Category", ylab = "Frequency")
# white ethnicity is prevalent

# ******************************************************

# Another way to deal with this is to strip the first letter from each observation

#Update the ethnicity column with the first description
profiles$ethnicity <- str_extract(profiles$ethnicity, "\\w+")
profiles$ethnicity

#frequency table. White is the most frequent  ethnicity. 
table(profiles$ethnicity)
sort(table(profiles$ethnicity), decreasing = TRUE)

# create a bar plot visualization
ggplot(data = profiles, aes(x = ethnicity, fill = ethnicity)) +
  geom_bar() +
  facet_wrap(~ profiles$sex) +
  labs(title = "Ethnicity Distribution by Gender",
       x = "Ethnicity ",
       y = "Number of Users",
       fill = "Ethnicity") +
  theme_bw()

# *****************************************************************

# stacked bar plot
ggplot(data = profiles, aes(x = ethnicity, fill = sex)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Ethnicity Distribution by Gender",
       x = "Ethnicity",
       y = "Number of Users",
       fill = "Gender") +
  theme_bw()

# *****************************************************************

# filled bar plot
ggplot(data = profiles, aes(x = ethnicity, fill = sex)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Ethnicity Distribution by Gender",
       x = "Ethnicity",
       y = "Number of Users",
       fill = "Gender") +
  theme_bw()


# *****************************************************************

#ggplot for ethnicity
ggplot(data = profiles, aes(x = ethnicity, fill = sex)) +
  geom_bar(position = position_fill(reverse = TRUE), color = "black") +
  labs(title = "Ethnicity Distribution by Gender",
       x = "Ethnicity",
       y = "Proportion",
       fill = "Gender") +
  theme_bw()

# *****************************************************************

##### plot dataframe, change to dataframe
plotDF <- data.frame(table(profiles$sex,  profiles$ethnicity))

# Stacked
ggplot(data = plotDF, aes(fill=reorder(Var2, Freq), y=Freq, x=Var1)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot", x = "Gender", y = "Number of Users", fill = "Ethnicity") +
  scale_x_discrete(labels = c("Male", "Female")) +
   theme_bw()


# *****************************************************************

freq <- table(profiles$ethnicity)
freq_sort <- sort(freq, decreasing = TRUE)
bar_colors <- c("#CC79A7", "#F0E442", "#56B4E9", "#E69F00", "#F0C589", "#009E73", "#D55E00", "#56E4E9", "#0072B2")
barplot(freq_sort, main = "Ethnicity Distribution",
        names.arg = names(freq_sort), col=bar_colors[match(names(freq_sort),levels(factor(profiles$ethnicity)))]) 

#barplot 
ggplot(data = profiles, aes(x = reorder(ethnicity, -table(ethnicity)[ethnicity]), fill = sex)) +
  geom_bar() +
  labs(x = "Ethnicity", y= "Number of Users", title = "Distribution of Ethnicity by Gender", fill = "Gender") +
  facet_wrap(~ sex, nrow = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values = c("pink", "lightblue"), labels = c("Female", "Male"))


########################################
###### PROFILES _ HEIGHT ###############
#######################################

# find unique values
unique(profiles$height)  

#frequency table
table(profiles$height)

# NA values
sum(is.na(profiles$height))
#there are 3 observations with NA

#find the percentage of NA height
na_count_height <- sum(is.na(profiles$height))
total_count <- nrow(profiles)
percent_na_height <- na_count_height / total_count * 100

# print percentage, there are 0.0050% of NA height
cat(sprintf("Percentage of NA values for height: %.4f%%\n", percent_na_height))

#frequency table.  
table(profiles$height)
sort(table(profiles$height), decreasing = TRUE)

#Replace with the median height
profiles$height[is.na(profiles$height)] <- median(profiles$height, na.rm=TRUE)

#frequency table
table(profiles$height)

# NA values. The data is clean. There are no more NA values for height
sum(is.na(profiles$height))


#histogram
ggplot(data = profiles, aes(x = height)) +
  geom_histogram(bins = 100, fill = 'steelblue', color = 'white') +
  labs(x = 'Age', y= 'Number of Users', title = 'Distribution of Age')

#boxplot
boxplot(profiles$height, 
        main="Age Distribution", 
        xlab="Age", 
        ylab="Frequency",
        col="lightblue")

# **********************************************************

#boxplot indicated some outliers for height
#filter for height that is over 80 and less than 50
height_filtered <- nrow(filter(profiles, height > 80 | height < 50))

#check for the percentage of height that is over 80 and less than 50
percent_height_filtered <- height_filtered / nrow(profiles) * 100

# print percentage
cat(sprintf("Percentage of height over 80 and less than 50: %.4f%%\n", percent_height_filtered))

# The percentage of height over 80 and less than 50 is 0.045%
# Filter out for height over 80 and less than 50
profiles <-  profiles %>%
  filter(height <= 80) %>%
  filter(height >= 50)

#boxplot _ check again
boxplot(profiles$height, 
        main="Height Distribution (Between 50 and 80)", 
        xlab="Height", 
        ylab="Frequency",
        col="lightblue")

# **********************************************************

#histogram check again after removing outliers
ggplot(data = profiles, aes(x = height)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(x = "Age", y= "Number of Users", title = "Distribution of Age") +
  facet_wrap(~ sex, nrow = 2)

#histogram
ggplot(data = filter(profiles, height <= 80), aes(x = height)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_continuous(limits = c(0, 80), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Height", y= "Number of Users", title = "Distribution of Height") +
  facet_wrap(~ sex, nrow = 2)

# **********************************************************
#boxplot
ggplot(data = filter(profiles, height <= 80), aes(x = sex, y = height, fill = sex)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(40, 80), expand = c(0, 0)) +
  labs(x = "Sex", y = "Height", title = "Distribution of Height by Sex")

#in general males are taller than females.

# **********************************************************

# Height distribution
freq <- table(profiles$height)
barplot(freq, main = "Height Distribution", xlab = "Height", ylab = "Frequency",
        names.arg = levels(factor(profiles$height)),
        las = 2)

# **********************************************************
# overall histogram of the height distribution
ggplot(profiles, aes(x = height)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Height Distribution",
       x = "Height (inches)",
       y = "Frequency")


########################################
###### PROFILES _ INCOME ###############
#######################################

# find unique values
unique(profiles$income)  

#frequency table
table(profiles$income)

# NA values
sum(is.na(profiles$income))
#there are 48373 observations with NA

#find the percentage of NA income
na_count_income <- sum(is.na(profiles$income))
total_count <- nrow(profiles)
percent_na_income <- na_count_income / total_count * 100

# print percentage, there are 80.84% of NA income
cat(sprintf("Percentage of NA values for income: %.2f%%\n", percent_na_income))
# 80 percent is a lot of data. The focus on income will be very minimal
#income will be dealt with separately

#frequency table.  
table(profiles$income)
sort(table(profiles$income), decreasing = TRUE)


# ********************************************************************

#point graph
ggplot(profiles, aes(x = age, y = income)) +
  geom_point()


#histogram
ggplot(data = profiles, aes(x = income)) +
  geom_histogram(bins = 100, fill = 'steelblue', color = 'white') +
  labs(x = 'Income', y= 'Number of Users', title = 'Distribution of Income')

#boxplot
boxplot(profiles$income, 
        main="Income Distribution", 
        xlab="Income", 
        ylab="Frequency",
        col="lightblue")


#plot education distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$income))

ggplot(data = plotDF, aes(fill = Var2, x = Freq, y = Var1)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_discrete(name = "Education") +
  labs(title = "Income Distribution by Gender",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom") +
  scale_y_discrete(labels = c("Male", "Female")) +
  guides(fill = guide_legend(reverse = TRUE, order = 2))


# ******************************************************************
# REGRESSION...ON REPORTED INCOME

# Boxplot
boxplot(profiles$income, 
        main="Income Distribution", 
        xlab="Income", 
        ylab="Frequency",
        col="lightblue")

# filter out for only observations where income was reported
profiles_filtered <- profiles[complete.cases(profiles$income), ]
  

# Build a scatter plot to show relationship 
p <- ggplot(profiles_filtered, aes(age, income)) + geom_point(alpha=0.02) + theme_gdocs()
p

# Since we see a relationship let's make a linear model to predict prices
fit <- lm(profiles_filtered$income ~ profiles_filtered$age + 0, profiles_filtered)
fit

# Add out model predictions
p <- p + geom_abline(intercept =  0, 
                     slope = coefficients(fit), 
                     color='red') +
  theme_gdocs()
p

# ******************************************************************
# HYPOTHESIS TESTIN OF INCOME BETWEEN MALE AND FEMALE...

# Perform a hypothesis testing that males on average earn more than females
ttest <- t.test(income ~ sex, data = profiles_filtered)

#Results
ttest
# results 
# alternative hypothesis: true difference in means between group Female and group 
# Male is not equal to 0
# mean income of female is 85K, mean income of male is 110K
#reject Null Hypothesis that mean salary of females = mean salary of females

#plot the results with error bars
ggplot(profiles_filtered, aes(x = sex, y = income)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightgrey") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) + 
  labs(title = "Comparison of the Mean Income by Gender",
       x = "Gender", y = "Mean Income") +
  theme(panel.background = element_rect(fill = "white"))


# ******************************************************************
# REPLACE WITH MEDIAN AND CHECK

#Replace with the median income and store into a new data frame
profiles_income <- profiles %>%
  mutate(income = ifelse(is.na(income), median(income, na.rm = TRUE), income))

# view updated profile Dataframe

head(updated_profiles)

#frequency table
table(profiles_income$income)

# NA values. The data is clean. There are no more NA values for height
sum(is.na(profiles_income$height))


#point graph
ggplot(profiles_income, aes(x = age, y = income)) +
  geom_point()


#histogram
ggplot(data = profiles_income, aes(x = income)) +
  geom_histogram(bins = 100, fill = 'steelblue', color = 'white') +
  labs(x = 'Age', y= 'Number of Users', title = 'Distribution of Age')

#boxplot
boxplot(profiles_income$income, 
        main="Income Distribution", 
        xlab="Income", 
        ylab="Frequency",
        col="lightblue")


#plot education distribution by gender
plotDF <- data.frame(table(profiles_income$sex,  profiles$income))

ggplot(data = plotDF, aes(fill = Var2, x = Freq, y = Var1)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_discrete(name = "Education") +
  labs(title = "Income Distribution by Gender",
       x = "Proportion",
       y = "",
       fill = "") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom") +
  scale_y_discrete(labels = c("Male", "Female"))



####################################
###### PROFILES _ JOBS #############
####################################

# find unique values
unique(profiles$job)  # there are 2 unique Job titles

#frequency table
table(profiles$job)

# NA values
sum(is.na(profiles$job))
#there are 8173 observations with NA

#find the percentage of NA Job
na_count_job <- sum(is.na(profiles$job))
total_count <- nrow(profiles)
percent_na_job <- na_count_job / total_count * 100

# print percentage, there are 13.66% of NA Job
cat(sprintf("Percentage of NA values for Job: %.2f%%\n", percent_na_job))

#frequency table. Never is the most for drugs. 
table(profiles$job)
sort(table(profiles$job), decreasing = TRUE)

#Since there number of NA is 13.66%, rather group all NA with as "other"
profiles$job <- ifelse(is.na(profiles$job), "other", profiles$job)

#Update the job column with the first description
profiles$job <- str_extract(profiles$job, "\\w+")
profiles$job

#frequency table
table(profiles$job)

# NA values. The data is clean. There are no more NA values for drugs
sum(is.na(profiles$job))

#frequency barplot
freq <- table(profiles$job)
barplot(freq, main = "Job Distribution", xlab = "job", ylab = "Frequency",
        names.arg = levels(factor(profiles$job)))

#barplot check again after adding NA's values to "other" category
ggplot(data = profiles, aes(x = reorder(job, -table(job)[job]), fill = sex)) +
  geom_bar() +
  labs(x = "job", y= "Number of Users", title = "Distribution of use of Job") +
  facet_wrap(~ sex, nrow = 2) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values = c("pink", "lightblue"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#barplot filter out other to get a better distribution
ggplot(data = subset(profiles, job != "other"), aes(x = job, fill = sex)) +
  geom_bar() +
  labs(x = "Job", y= "Number of Users", title = "Distribution of Job by Gender") +
  facet_wrap(~ sex, nrow = 2) + 
  scale_fill_manual(values = c("pink", "lightblue"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ******************************************************
#plot Job title distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$job))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Job Title") +
  labs(title = "Job Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE, order = 2))

# ******************************************************
#Plot but exclude "other" 

plotDF_wo_other <- profiles %>%
  filter(job != "other") %>%
  group_by(sex, job) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count)) %>%
  ggplot(aes(x = sex, y = prop, fill = job)) +
  geom_col(position = "fill") +
  labs(title = "Job Distribution by Gender (Excluding 'Other')",
       x = "Gender",
       y = "Proportion",
       fill = "Job Title") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE, order = 2))

plotDF_wo_other


#######################################
######## PROFILES _ LAST ONLINE #######
#######################################


#last_online has a character datatype, change it to datetime
profiles$last_online <- as.POSIXct(profiles$last_online, format = "%Y-%m-%d %H:%M:%S")

#check if it has changed
glimpse(profiles)

#create a month and a year columns

profiles <- profiles %>%
  mutate(last_online_year = year(last_online),
         last_online_month = month.abb[month(last_online)])

#check if it has changed
glimpse(profiles)

#check how many distinct years are there --> there are two distinct years
distinct_year <- profiles %>%
  distinct(last_online_year)

# *************************************************************************

#plot Users last online distribution by gender
plotDF <- data.frame(table(profiles$last_online_year,  profiles$sex))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = Freq, y = Freq + 300), position = position_stack(vjust = 0.3), size = 3) +
  labs(x = "Year", y = "Number of Users", title = "Users last online by year") +
  scale_fill_manual(values = c("#FFB6C2", "#87CEFA"), name = "Gender", labels = c("Female", "Male"))

# *************************************************************************

# Do a time series to check the change in the number of users over time

# aggregate users by year and month
users_time_series <- profiles %>%
  group_by(last_online_month, last_online_year) %>% 
  summarize(count = n()) %>% 
  ungroup()

# *************************************************************************

# plot a time series
ggplot(users_time_series, aes(x = ymd(paste0(last_online_year, "-", last_online_month, "-01")), y = count)) +
  geom_line() +
  labs(x = "Month and Year", y = "Number of Users", title = "Number of Users over Time")

# aggregate the users by year, month, and sex
users_time_series <- profiles %>%
  group_by(last_online_month, last_online_year, sex) %>% 
  summarize(count = n()) %>% 
  ungroup()

# *************************************************************************

# plot a time series by gender
ggplot(users_time_series, aes(x = ymd(paste0(last_online_year, "-", last_online_month, "-01")), y = count, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Month and Year", y = "Number of Users", title = "Number of Users over Time by Gender") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_color_manual(values = c("#FFB6C2", "#87CEFA"), name = "Gender", labels = c("Female", "Male"))


#########################################
###### PROFILES _ LOCATION #############
#########################################

# find unique values
unique(profiles$location) 

#frequency table
table(profiles$location)

# NA values
sum(is.na(profiles$location))
#there no null values

#frequency table. San Francisco, California is the most frequent  occuring location. 
table(profiles$location)
sort(table(profiles$location), decreasing = TRUE)

#split into state and city to see where most people come from
profiles <- profiles %>%
  separate(location, into = c("city", "state"), sep = ",", remove = FALSE)

#check if it has changed
glimpse(profiles)

#find distinct city and state
distinct(profiles, city)   # 198 distinct cities
distinct(profiles, state)  # 41 distinct states

# only interested in plotting the city and state with highest count and then checking by
#gender as well

#top cities
top_cities <- profiles %>%
  filter(!is.na(city)) %>%
  group_by(city, sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  top_n(5, count) %>%
  arrange(desc(count))

#plot the top 5 cities that have the most users
ggplot(top_cities, aes(x = reorder(city, -count), y = count, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "City", y = "Number of Users", title = "Top 5 Cities with the Most Users")+
  scale_fill_manual(values = c("#FFB6C2", "#87CEFA"), name = "Gender", labels = c("Female", "Male"))

#top states
top_states <- profiles %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  top_n(3, count) %>%
  arrange(desc(count))

#plot the top 3 cities that have the most users
ggplot(top_states, aes(x = reorder(state, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkkhaki") +
  geom_text(aes(label = count), vjust = -0.3) +
  labs(x = "State", y = "Number of Users", title = "Top 3 States with the Most Users")


#########################################
###### PROFILES _ OFFSPRING #############
#########################################

# find unique values
unique(profiles$offspring) 
#there are 16 categories

#to perform further analysis, it is decided to split the categories into much
#broader by splitting into "No kids", "Has kids" and "Wants kids"

profiles$offspring_new <- ifelse(grepl("doesn't have kids", profiles$offspring), "No kids",
                                    ifelse(grepl("has kids", profiles$offspring) | grepl("has a kid", profiles$offspring),"Has kids",
                                           ifelse(grepl("might want kids", profiles$offspring) | grepl("wants kids", profiles$offspring), "Wants kids", "Unknown")))
 
unique(profiles$offspring_new) 
#there are 16 categories

#frequency table
table(profiles$offspring_new)

# NA values
sum(is.na(profiles$offspring_new))
#there no null values

#frequency table. Unknown, followed by "No kids" is the most frequent  occurring event 
#for offspring. 
table(profiles$offspring_new)
sort(table(profiles$offspring_new), decreasing = TRUE)

#plot education distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$offspring_new))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Offspring") +
  labs(title = "Offspring Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")


#########################################
###### PROFILES _ ORIENTATION ###########
#########################################

# find unique values
unique(profiles$orientation) 
#there are 3 categories

#frequency table
table(profiles$orientation)

# NA values
sum(is.na(profiles$orientation))
#there no null values

#frequency table. Unknown, followed by "No kids" is the most frequent  occurring event 
#for offspring. 
table(profiles$orientation)
sort(table(profiles$orientation), decreasing = TRUE)

#plot orientation distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$orientation))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Sexual Orientatio") +
  labs(title = "Sexual Orientation Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")

#########################################
###### PROFILES _ PETS####### ###########
#########################################

# find unique values
unique(profiles$pets) 
#there are 16 categories

#frequency table
table(profiles$pets)

# NA values
sum(is.na(profiles$pets))
#there 19887 null values

#frequency table. 
#for offspring. 
table(profiles$pets)
sort(table(profiles$pets), decreasing = TRUE)

#find the percentage of NA pets
na_count_pets <- sum(is.na(profiles$pets))
total_count <- nrow(profiles)
percent_na_pets <- na_count_job / total_count * 100

# print percentage, there are 13.66% of NA pets
cat(sprintf("Percentage of NA values for pets: %.2f%%\n", percent_na_pets))

#frequency table. Never is the most for pets. 
table(profiles$pets)
sort(table(profiles$pets), decreasing = TRUE)

#replace NA with "likes dogs and likes cats"
profiles$pets <- ifelse(is.na(profiles$pets), "likes dogs and likes cats", profiles$pets)

# NA values. The data is clean. There are no more NA values for pets
sum(is.na(profiles$pets))

#plot pet preference distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$pets))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Pets") +
  labs(title = "Distribution of Pets preference by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw()

#plot pet preference distribution by ethnicity
plotDF <- data.frame(table(profiles$ethnicity,  profiles$pets))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Pets") +
  labs(title = "Distribution of Pets preference by Ethnicity",
       x = "Ethnicity",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw()

# update the preferences to make it broader

# The descriptions are too long. therefore suggest to clean them and shorten them
profiles$pets <- recode(profiles$pets, 
                        "likes dogs and likes cats" = "likes both",
                        "has cats" = "has cats",
                        "likes cats" = "likes cats",
                        "not applicable" = "not applicable",
                        "has dogs and likes cats" = "likes cats",
                        "likes dogs and has cats" = "likes dogs & has cats",
                        "likes dogs and dislikes cats" = "likes dogs",
                        "has dogs" = "has dogs",
                        "has dogs and dislikes cats" = "has dogs",
                        "likes dogs" = "likes dogs",
                        "has dogs and has cats" = "has dogs & cats",
                        "dislikes dogs and has cats" = "has cats",
                        "dislikes dogs and dislikes cats" = "dislikes both",
                        "dislikes cats" = "dislikes cats",
                        "dislikes dogs and likes cats" = "likes cats",
                        "dislikes dogs" = "dislikes dogs")


#plot pet preference distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$pets))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Pets") +
  labs(title = "Distribution of Pets preference by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(reverse = TRUE, order = 2))


#########################################
###### PROFILES _ RELIGION ##############
#########################################

# find unique values
unique(profiles$religion) 
#there are 16 categories

#frequency table
table(profiles$religion)

# NA values
sum(is.na(profiles$religion))
#there 20187 null values

#frequency table. Agnosticism is the most frequent occurring religion 
table(profiles$religion)
sort(table(profiles$religion), decreasing = TRUE)

#replace NA with "no_response"
profiles$religion <- ifelse(is.na(profiles$religion), "no_response", profiles$religion)

# NA values. The data is clean. There are no more NA values for pets
sum(is.na(profiles$religion))

#frequency table
table(profiles$religion)


# ******************************************************

#Update the religion column with the first description
profiles$religion <- str_extract(profiles$religion, "\\w+")
profiles$religion

#frequency table. White is the most frequent  religion. 
table(profiles$religion)
sort(table(profiles$religion), decreasing = TRUE)

# *****************************************************************

#plot religion distribution by sex
plotDF <- data.frame(table(profiles$sex,  profiles$religion))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Pets") +
  labs(title = "Distribution of Religion Gender",
       x = "Religion",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE, order = 2))

# *****************************************************************

# stacked bar plot
ggplot(data = profiles, aes(x = religion, fill = sex)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Ethnicity Distribution by Gender",
       x = "Ethnicity",
       y = "Number of Users",
       fill = "Gender") +
  theme_bw()

# *****************************************************************

#stacked bar plot filtering out for "no response" and other
ggplot(data = profiles %>% filter(religion != "no_response" & religion != "other"), 
       aes(x = reorder(religion, -table(religion)[religion]), 
           fill = sex)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Religion Distribution by Gender (Excluding no response and other)",
       x = "Religion",
       y = "Number of Users",
       fill = "Gender") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "white"))+
    scale_fill_manual(values = c("pink", 'lightblue'))


####################################
###### PROFILES _ SEX #############
####################################

# find unique values
unique(profiles$sex)  # there are 2 unique genders

#frequency table
table(profiles$sex)

# NA values
sum(is.na(profiles$sex))
#there no null values

#frequency table. There are more males than females 
table(profiles$sex)
sort(table(profiles$sex), decreasing = TRUE)

#frequency barplot
freq <- table(profiles$sex)
barplot(freq, main = "Gender Distribution", xlab = "sex", ylab = "Frequency",
        names.arg = levels(factor(profiles$sex)))

#barplot check again after adding NA's values to "other" category
ggplot(data = profiles, aes(x = reorder(sex, -table(sex)[sex]), fill = sex)) +
  geom_bar() +
  labs(x = "Gender", y= "Number of Users", title = "Distribution of Gender") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values = c("pink", "lightblue"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#create a pie chart

## create a table for the gender counts
gender_counts = table(profiles$sex)

##convert the table into a data frame
gender_df <- as.data.frame(gender_counts)
colnames(gender_df) <- c("Gender", "Count")

## add a column in the data frame to include the percentage by gender
gender_df$Percent <- gender_df$Count / sum(gender_df$Count) * 100

##plot the pie chart
ggplot(data = gender_df, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1.5, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percent), "%")), 
            position = position_stack(vjust = 0.5),
            size = 8) +
  theme_void() +
  scale_fill_manual(values = c("pink", "lightblue")) +
  labs(fill = "Gender") +
  ggtitle("Distribution of Gender")


#########################################
###### PROFILES _ SIGN##### #############
#########################################

# find unique values
unique(profiles$sign) 

#frequency table
table(profiles$sign)

# NA values
sum(is.na(profiles$sign))
#there are 11029 observations with NA

#find the percentage of NA sign
na_count_sign <- sum(is.na(profiles$sign))
total_count <- nrow(profiles)
percent_na_sign <- na_count_sign / total_count * 100

# print percentage, there are 18.43% of NA sign
cat(sprintf("Percentage of NA values for sign: %.2f%%\n", percent_na_sign))

#frequency table. Leo is the most frequent  ethnicity. 
table(profiles$sign)
sort(table(profiles$sign), decreasing = TRUE)

#Since there number of NA is 18.43%, rather group all NA with as 'gemini and it's fun to think about'
profiles$sign <- ifelse(is.na(profiles$sign), "gemini and it's fun to think about", profiles$sign)


# NA values. The data is clean. There are no more NA values for sign
sum(is.na(profiles$sign))


#frequency table
table(profiles$sign)

# ******************************************************

# A way to deal with this is to strip the first letter from each observation

#Update the sign column with the first description
profiles$sign <- str_extract(profiles$sign, "\\w+")
profiles$sign

#frequency table. Leo is the most frequent  sign. 
table_sign <- table(profiles$sign)
sign_sorted <- sort(table_sign, decreasing = TRUE)
sign_sorted 

# *****************************************************************
#do a lollipop

# create a table of counts of zodiac sign, excluding "other 
sign_counts <- table(profiles$sign[profiles$sign != "other"])

## convert table to a data frame
sign_df <- as.data.frame(sign_counts)
colnames(sign_df) <- c("sign", "count")

ggplot(data = sign_df, aes(x = count, y = reorder(sign, count))) +
  geom_segment(aes(xend = 0, yend = sign), color = "steelblue", size = 1.5) +
  geom_point(color = "steelblue", size = 6, shape = 20, fill = "white") +
  labs(x = "Number of users", y = "Zodiac Sign", title = "Distribution of Zodiac Signs (Excluding Other)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10))


# *****************************************************************


##### plot dataframe, change to dataframe
plotDF <- data.frame(table(profiles$sex,  profiles$sign))

# Stacked
ggplot(data = plotDF, aes(fill = Var2, y = Freq, x = Var1)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Zodiac sign by gender", x = "Gender", y = "Number of Users", fill = "Zodiac Sign") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw()


# ******************************************************
#plot Zodiac sign distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$sign))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Zodiac Sign") +
  labs(title = "Zodiac Sign by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")


# ******************************************************
#Plot but exclude "other" 


plotDF_wo_other <- profiles %>%
  filter(sign != "other") %>%
  group_by(sex, sign) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  #mutate to include a % column
  mutate(prop = count / sum(count)) %>%
  ggplot(aes(x = sex, y = prop, fill = sign)) +
  geom_col(position = "fill") +
  labs(title = "Zodiac Sign Distribution by Gender (Excluding 'Other')",
       x = "Gender",
       y = "Proportion",
       fill = "Zodiac Sign") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE, order = 2))

plotDF_wo_other


#########################################
###### PROFILES _ SMOKES ##################
#########################################

# find unique values. There are 6 unique values
unique(profiles$smokes) 

#frequency table
table(profiles$smokes)

# NA values
sum(is.na(profiles$smokes))
#there are 5493 observations with NA

#find the percentage of NA smokes
na_count_smokes <- sum(is.na(profiles$smokes))
total_count <- nrow(profiles)
percent_na_smokes <- na_count_smokes / total_count * 100

# print percentage, there are 9.18% of NA smokes
cat(sprintf("Percentage of NA values for smokes: %.2f%%\n", percent_na_smokes))

#frequency table. "No" is the most frequent  ethnicity. 
table(profiles$smokes)
sort(table(profiles$smokes), decreasing = TRUE)

#Since there number of NA is 9.18%, rather group all NA with as "no"
profiles$smokes <- ifelse(is.na(profiles$smokes), "no", profiles$smokes)


# NA values. The data is clean. There are no more NA values for smokes
sum(is.na(profiles$smokes))


#frequency table
table(profiles$smokes)

# ******************************************************
#plot Smoking habits distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$smokes))

ggplot(data = plotDF, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Smokes") +
  labs(title = "Smoking habits by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(reverse = TRUE, order = 2))
  


#########################################
###### PROFILES _ SPEAKS ##################
#########################################

# find unique values. There are 6 unique values
unique(profiles$speaks) 

# NA values
sum(is.na(profiles$smokes))
#there are no NA

#frequency table
table(profiles$speaks)

#clean to remove words in brackets and non_language words
profiles$speaks_copy <- profiles$speak
profiles$speaks_copy <- gsub("\\(.*?\\)", "", profiles$speaks_copy)
table(profiles$speaks_copy)

profiles$speaks_copy <- gsub("(okay|fluently|poorly|lisp|yiddish|ilongo|esperanto|c\\+\\+),? ?", "", profiles$speaks)
table(profiles$speaks_copy)

#### split each observation by "," to get a list of categories
speaks_list <- strsplit(profiles$speaks_copy, ",\\s*")
speaks_list

#remove () from list
speaks_list <- lapply(speaks_list, function(x) gsub("\\(.*?\\)", "", x))
speaks_list

#remove empty spaces
speaks_list <- lapply(speaks_list, function(x) gsub("\\s+", "", x))

####flatten the list of categories
speaks_flat <- unlist(speaks_list)
speaks_flat

####  count frequency of each category
speaks_freq <- table(speaks_flat)
top_language <- head(sort(speaks_freq, decreasing = TRUE), 5)

### Step 4.1: barplot of top 5
#choose pallette
speak_pallette <- colorRampPalette(c("grey", "steelblue"))(length(top_language))

barplot(top_language, main = "Method 1: Top 5 Languages spoken", 
        xlab = "Language", 
        ylab = "Frequency",
        col = speak_pallette)

# ******************************************************

# Another way to deal with this is to strip the first letter from each observation

#Update the speaks column with the first description
profiles$speaks <- str_extract(profiles$speaks, "\\w+")
profiles$speaks

#frequency table. White is the most frequent  speaks. 
table(profiles$speaks)
sort(table(profiles$speaks), decreasing = TRUE)

# create a bar plot visualization
ggplot(data = profiles, aes(x = speaks, fill = speaks)) +
  geom_bar() +
  labs(title = "Method 2: Language",
       x = "Ethnicity ",
       y = "Number of Users",
       fill = "Ethnicity") +
  theme_bw()


####################################
###### PROFILES _ STATUS############
####################################

# find unique values
unique(profiles$status)  # there are 5 unique status

#frequency table
table(profiles$status)

# NA values
sum(is.na(profiles$status))
#there are no observations with NA

#find the percentage of NA Job
na_count_status <- sum(is.na(profiles$status))
total_count <- nrow(profiles)
percent_na_status <- na_count_status / total_count * 100

#frequency table. Most people are Single. 
table(profiles$status)
sort(table(profiles$status), decreasing = TRUE)

#frequency barplot
freq <- table(profiles$status)
barplot(freq, main = "Status Distribution", xlab = "status", ylab = "Frequency",
        names.arg = levels(factor(profiles$status)))

# ******************************************************
#status title distribution by gender
plotDF <- data.frame(table(profiles$sex,  profiles$status))

ggplot(data = plotDF, aes(fill=reorder(Var2,Freq), y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_discrete(name = "Status") +
  labs(title = "Status Distribution by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "") +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE, order = 2))


#########################################
###### PROFILES _ ESSAY0 ################
#########################################

# Reference:
# https://www.pluralsight.com/guides/visualization-text-data-using-word-cloud-r


# The essay0 variable will only be analysed for the wordcloud.
# The variable will be dropped prior to peforming joining of data


#split the text column into individual words
text <- unlist(strsplit(as.character(profiles$essay0), " "))

#create corpus
corpus <- Corpus(VectorSource(text))

#clean the words

##conversion to lowercase
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, content_transformer(tolower))

## remove punctuation
corpus <- tm_map(corpus, removePunctuation)

## remove stopwords
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))

# Stemming
corpus = tm_map(corpus, stemDocument)

# Eliminate white spaces
corpus = tm_map(corpus, stripWhitespace)

#eliminate numbers
corpus <- tm_map(corpus, removeNumbers)

#clean text
text_clean <- unlist(strsplit(as.character(corpus), " "))

#plot the wordcloud
set.seed(100)
library(wordcloud)
wordcloud(text, colors = rainbow(15), 
          max.words = 150, 
          random.order = FALSE)


###########################################
######  SHARED CENSUS 2010 ################
###########################################

#view the head
head(sharedcensus)

#view column names
colnames(sharedcensus)

#the census contains population, population by race
#select columns up to total population

sharedcensus_selected <- sharedcensus[, 1:6]

#view column names
colnames(sharedcensus_selected )


############################################################################################
#############################   JOINING DATA    ############################################
############################################################################################

##### Enriching dataset by joining profiles with latlon csv, addr, and shared census

#delete essay from the profiles data frame
profiles <- select(profiles, -(essay0))
summary(profiles)

#select columns in profiles of interest for further analysis

selected_cols <- c("age", "sex", "education", "religion","job", "income","status", 
                   "offspring_new" , "location","ethnicity","orientation", "pets",
                   "body_type", "height", "diet", "drugs", "drinks","smokes" )

subset_profiles <- profiles[selected_cols]

#view column names
colnames(subset_profiles)

# First join subset_profiles with latlon
profile_latlon <- left_join(subset_profiles, latlon, by ='location')
head(profile_latlon)

#view column names
colnames(profile_latlon)

# Second join profile_latlon with addr
profile_latlon_addr <- left_join(profile_latlon, addr, by ='location')
head(profile_latlon_addr)

#view column names
colnames(profile_latlon_addr)

# Final join profile_latlon_addr with sharedCensus
allData <- left_join(profile_latlon_addr, sharedcensus_selected, by ='location')
head(allData)

#view column names for all joined data
colnames(allData)

#rename "P0010001_Total_population"
allData$population <- allData$P0010001_Total_population
allData <- select(allData, -(P0010001_Total_population))

# find columns with NA
sapply(allData, function(x) sum(is.na(x)))

# find names with NA
names(allData)[colSums(is.na(allData)) > 0]

# *********************************************************************

#### Missing in income & quick median imputation example
sum(is.na(allData$income))
allData$income[is.na(allData$income)] <- median(allData$income, na.rm=TRUE)

### delete duplication in city, county postal code and state
allData <- select(allData, -(postalCode.y), -(city.y), -(county.y), -(state.y))

# *********************************************************************

#### Missing in postalCode & quick mode imputation example
#frequency table. 
table(allData$postalCode.x)
sort(table(allData$postalCode.x), decreasing = TRUE)

# replace with the mode code 94102
allData$postalCode.x <- ifelse(is.na(allData$postalCode.x), 94102, allData$postalCode.x)

#rename postalCode.x to postalCode
allData <- rename(allData, postalCode = postalCode.x)

# *********************************************************************

#### Missing in city & quick mode imputation example
#frequency table.
table(allData$city.x)
sort(table(allData$city.x), decreasing = TRUE)

# replace with the mode code 94102
allData$city.x <- ifelse(is.na(allData$city.x), "San Francisco" , allData$city.x)

#rename city x to city
allData <- rename(allData, city = city.x)

# *********************************************************************

#### Missing in county & quick mode imputation example
#frequency table.
table(allData$county.x)
sort(table(allData$county.x), decreasing = TRUE)

# replace with the mode code 94102
allData$county.x <- ifelse(is.na(allData$county.x), "San Francisco County" , allData$county.x)

# remove County description from all the data
allData$county.x <- gsub(" County", "", allData$county.x)

#rename county x to county
allData <- rename(allData, county = county.x)

# *********************************************************************

#### Missing in state & quick mode imputation example
#frequency table. 
table(allData$state.x)
sort(table(allData$state.x), decreasing = TRUE)

# replace with the mode code 94102
allData$state.x <- ifelse(is.na(allData$state.x), "San Francisco County" , allData$state.x)

#rename county x to county
allData <- rename(allData, state = state.x)


# *********************************************************************

#### Missing in population & quick median imputation example
sum(is.na(allData$population))
allData$population[is.na(allData$population)] <- median(allData$population, na.rm=TRUE)


#### You can use complete.cases() to identify records without NA if that is the route you want to explore.  
complete.cases(allData)

completeprofile_latlon <- profile_latlon[complete.cases(profile_latlon),]


### check data type
str(allData)

############################################################################################
############################# MORE EDA PLOTTING AFTER JOINING DATA  ########################
############################################################################################

#view column names for all joined data
colnames(allData)

str(allData)

# ******************************************************************

# MAP
#load world data
world <- map_data("world")

#plot map
gg <- ggplot() + 
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = region, group = group), 
           fill = 'white', color = 'black', size = 0.25) + 
  coord_map('mollweide') +
  theme_map()

#view map
gg

# Add points layer
gg +
  geom_point(data  = allData, 
             aes(x = lon, y=lat), 
             color = 'red', alpha=0.5) 
#view map
gg

# ***************

# states map by users
map("state", interior = T)
points(allData$lon,allData$lat, col='red')



# ******************************************************************
## CORRELATIONS

# compute correlation matrix for numerical data
numerical_col <- c("age", "income", "height", "population")

#do dataframe for the numerical data
df_num <- allData[numerical_col]

cor_matrix <- cor(df_num)

corrplot(cor_matrix, method = "color", 
         tl.col = "darkgrey",
         addCoef.col = "black")


#plot correlation matrix
corrplot(cor_matrix, 
         method = "number",
         type = "upper",
         tl.cex = 0.8,
         tl.col = "black",
         col = colorRampPalette(c("#FFFFFF", "#7F0000", "#007F00"))(200),
         cl.pos = "n",
         addCoef.col = "black")


############################################################################################
#################### FEATURE ENGINEERING FOR RELATIONSHIPS  ################################
############################################################################################

# RELATIONSHIPS, SCATTERPLOT

# check for age and population
plot(allData$age, allData$population,
     xlab = "Age", ylab = "Population",
     main = "Population by Age")

# check for geographical distribution of users
plot(allData$lat, allData$lon,
     xlab = "Latitude", ylab = "Longitude",
     main = "Geographical Distribution of Users")


###############################################
### find total population by state, county ####
###############################################


#aggregate population by state and county
pop_by_county <- allData %>%
  group_by(county) %>%
  summarize(population = sum(population)) 


top10_counties <- pop_by_county %>%
  arrange(desc(population)) %>%
  slice(1:10)

ggplot(top10_counties, aes(x = reorder(county, -population), y = population)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "County", y = "Population", title = "Top 10 Counties by Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###############################################
##### density plot of age and education #######
###############################################

education_levels <- c("associate", "bachelor", "high school", 
                      "law degree", "med school", "ph.d", "masters",
                      "space camp")
# density plot
allData %>%
  filter(education %in% education_levels) %>%
  ggplot(aes(x = age, fill = education)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Education Level")

###############################################
##### density plot of age and offspring #######
###############################################

# density plot
allData %>%
  ggplot(aes(x = age, fill = offspring_new)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Offspring") +
  labs(fill = "Offspring") 

###############################################
##### density plot of age and status ##########
###############################################

# density plot
allData %>%
  ggplot(aes(x = age, fill = status)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Status") +
  labs(fill = "Status") 


###############################################
##### density plot of age and religion ##########
###############################################

# density plot
allData %>%
  ggplot(aes(x = age, fill = religion)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Religion") +
  labs(fill = "Religion") 

###############################################
##### density plot of age and body_type  ######
###############################################

# density plot
allData %>%
  ggplot(aes(x = age, fill = body_type)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Body Type") +
  labs(fill = "Body type") 

###############################################
##### density plot of age and pets  ######
###############################################

# density plot
allData %>%
  ggplot(aes(x = age, fill = pets)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Pet preference") +
  labs(fill = "Pet preference") 


###############################################
##### density plot of age and pets  ######
###############################################

# density plot with filter
allData %>%
  filter(pets %in% c("likes both", "likes dogs", "likes cats")) %>%
  ggplot(aes(x = age, fill = pets)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Pet preference") +
  labs(fill = "Pet preference") 


###############################################
##### density plot of age and job  ######
###############################################

# density plot
allData %>%
  ggplot(aes(x = age, fill = job)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Pet preference") +
  labs(fill = "Job title") 



# density plot
allData %>%
  filter(job %in% c("computer", "science", "sales", "student","medicine", "education" )) %>%
  ggplot(aes(x = age, fill = job)) +
  geom_density(alpha = 0.3) +
  ggtitle("Density plot of Age by Job Title") +
  labs(fill = "Job title") 


###############################################
##### ethnicity relationship to education #####
###############################################

# table
eth_edu_table <- table(allData$ethnicity, allData$education)

#perform chi-squared test
chisq_result = chisq.test(eth_edu_table)

#print result
print(chisq_result)

# results are:
# X-squared = 4490.2, df = 232, p-value < 0.00000000000000022
# Results indicate low likelihood that observed data is very inlikely
# due to chance alone. There is a relationship between ethnicity and educatio
# Reject null hypothesis that the events are independent

#plot

#make a matrix
eth_edu_df <- as.data.frame.matrix(eth_edu_table)
eth_edu_df$ethnicity <- rownames(eth_edu_df)

#convert data to long format
eth_edu_long <- melt(eth_edu_df, id.vars = "ethnicity")

# create a stacked bar chart
ggplot(eth_edu_long, aes(x = ethnicity, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Education Levels by Ethnicity", x = "Ethnicity", y = "Count", fill = "Education Level") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()


###############################################
##### diet relationship to body_type #####
###############################################

# table
diet_body_table <- table(allData$diet, allData$body_type)

#perform chi-squared test
chisq_result_d_b = chisq.test(diet_body_table)

#print result
print(chisq_result_d_b)

# results are:
# X-squared = 1689.5, df = 198, p-value < 0.00000000000000022
# Results indicate low likelihood that observed data is very inlikely
# due to chance alone. There is a relationship between body type and diet
# Reject null hypothesis that the events are independent

#plot

#make a matrix
diet_body_df <- as.data.frame.matrix(diet_body_table)
diet_body_df$diet <- rownames(diet_body_df)

#convert data to long format
diet_body_long <- melt(diet_body_df, id.vars = "diet")

# create a stacked bar chart
ggplot(diet_body_long, aes(x = diet, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Diet by Body Type", x = "Diet", y = "Count", fill = "Body Type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()


###############################################
########### offspring to status  ##############
###############################################

# table
offspr_status_table <- table(allData$offspring_new, allData$status)

#perform chi-squared test
chisq_result_offspr_status = chisq.test(offspr_status_table)

#print result
print(chisq_result_offspr_status)

# results are:
# X-squared = 625.74, df = 12, p-value < 0.00000000000000022
# Results indicate low likelihood that observed data is very inlikely
# due to chance alone. There is a relationship between status and offspring
# Reject null hypothesis that the events are independent

#plot

#make a matrix
offspr_status_df <- as.data.frame.matrix(offspr_status_table)
offspr_status_df$offspring_new <- rownames(offspr_status_df)

#convert data to long format
offspr_status_long <- melt(offspr_status_df, id.vars = "offspring_new")

# create a stacked bar chart
ggplot(offspr_status_long, aes(x = offspring_new, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Offspring by Status", x = "Offspring", y = "Count", fill = "Status") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()



###############################################
############## AGGREGATE ######################
###############################################

#### Aggregate by religion and status, calculate the mean income

#aggregate
agg_data_income <- aggregate(allData$income, by = list(allData$religion, allData$status), FUN = mean) 
agg_data_income


#plot
ggplot(agg_data_income, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Religion") + ylab("Income") +
  ggtitle("Income by Religion and Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Marital Status")

# ****************************************************

#### Aggregate by offspring and education, calculate the mean age

#aggregate age by offspring and education
agg_data_age <- aggregate(allData$age, by = list(allData$offspring_new, allData$education), FUN = mean) 

#plot
ggplot(agg_data_age, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Offsspring") + ylab("Age") +
  ggtitle("Age by Education and Offspring") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Education")

# ***********************************************
#aggregate age by offspring and status
agg_data_age_status <- aggregate(allData$age, by = list(allData$offspring_new, allData$status), FUN = mean) 

#plot
ggplot(agg_data_age_status, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Offsspring") + ylab("Age") +
  ggtitle("Age by Marital and Offspring") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Marital Status")


# ***********************************************


#aggregate age by body type and diet
agg_data_age_diet <- aggregate(allData$age, by = list(allData$body_type, allData$diet), FUN = mean) 

#plot
ggplot(agg_data_age_diet, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Body Type") + ylab("Age") +
  ggtitle("Age by Body type and diet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_discrete(name = "Diet")

# *********************************************************
#aggregate age for smokes and drink
agg_smoke_drink <- aggregate(allData$age, by = list(allData$smokes, allData$drinks), FUN = mean) 

#plot
ggplot(agg_smoke_drink, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Smoking habits") + ylab("Age") +
  ggtitle("Age by Smoking and Drinking Habits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_discrete(name = "Drinking habits")

# *********************************************************
#aggregate age for pets and diet
agg_pets_diet <- aggregate(allData$age, by = list(allData$pets, allData$diet), FUN = mean) 

#plot
ggplot(agg_pets_diet, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Pets") + ylab("Age") +
  ggtitle("Diet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_discrete(name = "Diet")

#aggregate age by job and education
agg_data_age_job <- aggregate(allData$age, by = list(allData$job, allData$education), FUN = mean) 

#plot
ggplot(agg_data_age_job, aes(x = Group.1, y = x, fill = Group.2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Job") + ylab("Age") +
  ggtitle("Age by Job and Offspring") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Education")


###############################################
######### FEATURE ENGINEERING #################
###############################################

##### Feature Engineer relationship status & education
allData$statEDU <- paste(allData$status, allData$education, sep = '_')
table(allData$statEDU)


##### Feature Engineer body type & diet
allData$bodydiet <- paste(allData$body_type, allData$diet, sep = '_')
table(allData$bodydiet)


##### Feature Engineer age & offspring
allData$agespring <- paste(allData$age, allData$offspring_new, sep = '_')
table(allData$agespring)



############################### END END END  ############################








