#' Author: Mokete Mashala
#' Date: March-22-2023
#' Purpose: Based on National City Bank the objective The research and modeling team 
#' at National Bank City has been tasked with creating a customer propensity model for a new product, 
#' specifically a line of credit against a household’s used car. The objective is to develop a model 
#' that will be able to identify the top 100 prospective customer lists to contact. 
#' These would be the top customers with the highest probability of accepting the offer. 

#' Steps taken include: Accessing the data, Exploring the data, Prepare data (Data preprocessing), Analyze Data
#' Develop Machine Learning algorithms, choose the best and then make recommendations
#' 
#'References: 
# https://github.com/togiberlin/data_mining_cup/blob/master/dmc1/results/predictions_Unic0rn.R
# https://bradleyboehmke.github.io/HOML/random-forest.html#prerequisites-9
# https://www.kaggle.com/datasets/kondla/carinsurance/code



# Options
options(scipen=999)

# Libs

library(dplyr)
library(ggplot2)
library(tidyr)
library(vtreat)
library(ggthemes)
library(ModelMetrics)
library(MLmetrics)
library(pROC)
library(ROSE)
library(base)
library(lubridate)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ranger)


# SetWD
setwd("C:/PERSONAL/02. Hult MBAN-DUAL Degree/12. Visializing Data with R/Hult_Visualizing-Analyzing-Data-with-R/personalFilesA2")

# Get the National city bank files
datadictionary <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/dataDictionary.csv')
prospcustomers <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/ProspectiveCustomers.csv', stringsAsFactors=T)
currentcustomer <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/CurrentCustomerMktgResults.csv', stringsAsFactors=T)
axiom <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdAxiomData.csv', stringsAsFactors=T)
credit <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdCreditData.csv', stringsAsFactors=T)
vehicle <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdVehicleData.csv', stringsAsFactors=T)

# Inspect the structure (names, data types, number or rows and columns) of the data
str(prospcustomers)
str(datadictionary )
str(currentcustomer)
str(axiom)
str(credit)
str(vehicle)

############################################################################################
#############################   JOINING DATA    ############################################
############################################################################################

# First join current customer with axiom data
cust_axiom <- left_join(currentcustomer, axiom, by ='HHuniqueID')
head(cust_axiom)

#view column names
colnames(cust_axiom)

# Second join cust_axiom with credit data
cust_axiom_credit <- left_join(cust_axiom, credit, by ='HHuniqueID')
head(cust_axiom_credit)

# View column names
colnames(cust_axiom_credit)

# Third join cust_axiom_credit with credit data
allData <- left_join(cust_axiom_credit, vehicle, by ='HHuniqueID')

head(allData)

##################################################################################
# clean the callstart and callEnd, change data type from chr to date and time values
##################################################################################

allData$CallStart <- as.POSIXct(allData$CallStart, format = "%H:%M:%S")
allData$CallEnd <- as.POSIXct(allData$CallEnd, format = "%H:%M:%S")


# Add a new column for the Duration of call
allData$Duration <- difftime(allData$CallEnd, allData$CallStart, units = "mins")

# Convert data type of duration to numeric
allData$Duration <- as.numeric(as.character(allData$Duration))

# View column names
colnames(allData)

# View information
str(allData)

############################################################################################
################################# STATISTICAL ANALYSIS  #####################################
############################################################################################

######## loop over each column to perform statistical summary for allData for numerical

for (col in names(allData)) {
  if (is.numeric(allData[[col]])) {
  print(paste("Summary statistics for", col))
  print(summary(allData[[col]]))
  }
}

# [1] "Summary statistics for dataID"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1    1001    2000    2000    3000    4000 

# [1] "Summary statistics for LastContactDay"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.00    8.00   16.00   15.72   22.00   31.00 

# [1] "Summary statistics for NoOfContacts"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.607   3.000  43.000 

# [1] "Summary statistics for DaysPassed"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.00   -1.00   -1.00   48.71   -1.00  854.00 

# [1] "Summary statistics for PrevAttempts"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.7175  0.0000 58.0000 

# [1] "Summary statistics for DigitalHabits_5_AlwaysOn"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    2.00    3.00    3.01    4.00    5.00 

# [1] "Summary statistics for Age"
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  18.00   32.00   39.00   41.21   49.00   95.00 

# [1] "Summary statistics for DefaultOnRecord"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.0145  0.0000  1.0000 

# [1] "Summary statistics for RecentBalance"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3058.0   111.0   551.5  1532.9  1619.0 98417.0 

# [1] "Summary statistics for HHInsurance"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.4928  1.0000  1.0000 

# [1] "Summary statistics for CarLoan"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   0.133   0.000   1.000 

# [1] "Summary statistics for carYr"
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1960    2005    2012    2010    2017    2019     202 

# [1] "Summary statistics for Duration"
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.08333  2.10000  3.86667  5.84740  7.66667 54.21667 

######################################
############## BOX PLOT ##############
######################################

# Observe outliers for different columns py plotting on boxplot: Age 
ggplot(data = allData, aes(x = "",y = Age )) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("Age") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: LastContactDay
ggplot(data = allData, aes(x = "",y = LastContactDay)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("Last Contact Day") +
  theme_bw(base_size = 14)


# Observe outliers for different columns py plotting on boxplot: NoOfContacts
ggplot(data = allData, aes(x = "",y = NoOfContacts)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("NoOfContacts") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: DaysPassed
ggplot(data = allData, aes(x = "",y = DaysPassed)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("DaysPassed") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: PrevAttempts
ggplot(data = allData, aes(x = "",y = PrevAttempts)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("PrevAttempts") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: DigitalHabits_5_AlwaysOn
ggplot(data = allData, aes(x = "",y = DigitalHabits_5_AlwaysOn)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("DigitalHabits_5_AlwaysOn") +
  theme_bw(base_size = 14)

# ********************************************

# Observe outliers for different columns py plotting on boxplot: RecentBalance
ggplot(data = allData, aes(x = "",y = RecentBalance)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("RecentBalance") +
  theme_bw(base_size = 14)

#Outlier identified of 98417
# Identify the outlier values in the RecentBalance column
outlier_value <- quantile(allData$RecentBalance, 0.99)

# Calculate the mean of the non-outlier values 
mean_value <- mean(allData$RecentBalance[allData$RecentBalance < outlier_value], na.rm = TRUE)

#Replace outlier values with mean
allData$RecentBalance[allData$RecentBalance > outlier_value] <- mean_value

# ********************************************

# Observe outliers for different columns py plotting on boxplot: HHInsurance
ggplot(data = allData, aes(x = "",y = HHInsurance)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("HHInsurance") +
  theme_bw(base_size = 14)


# Observe outliers for different columns py plotting on boxplot: carYr
ggplot(data = allData, aes(x = "",y = carYr)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("carYr") +
  theme_bw(base_size = 14)


# Observe outliers for different columns py plotting on boxplot: Duration 
ggplot(data = allData, aes(x = "",y = Duration )) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("Duration ") +
  theme_bw(base_size = 14)


####### loop over each column to perform statistical summary for prospective customers

for (col in names(prospcustomers)) {
  if (is.numeric(prospcustomers[[col]])) {
    print(paste("Summary statistics for", col))
    print(summary(prospcustomers[[col]]))
  }
}

# [1] "Summary statistics for dataID"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4001    4251    4500    4500    4750    5000 

# [1] "Summary statistics for LastContactDay"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   15.00   15.55   21.00   31.00 

# [1] "Summary statistics for NoOfContacts"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.508   3.000  34.000 

# [1] "Summary statistics for DaysPassed"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.00   -1.00   -1.00   51.65   -1.00  586.00 

# [1] "Summary statistics for PrevAttempts"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   0.806   0.000  20.000 


#######################################################################################
############## BOX PLOT - PROSPECTIVE CUSTOMERS - CHECKING FOR OUTLIERS  ##############
#######################################################################################

# Observe outliers for different columns py plotting on boxplot: LastContactDay
ggplot(data = prospcustomers, aes(x = "",y = LastContactDay)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("Last Contact Day") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: NoOfContacts
ggplot(data = prospcustomers, aes(x = "",y = NoOfContacts)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("NoOfContacts") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: DaysPassed
ggplot(data = prospcustomers, aes(x = "",y = DaysPassed)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("DaysPassed") +
  theme_bw(base_size = 14)

# Observe outliers for different columns py plotting on boxplot: PrevAttempts
ggplot(data = prospcustomers, aes(x = "",y = PrevAttempts)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue", size = 1.2) + 
  xlab("")+
  ylab("PrevAttempts") +
  theme_bw(base_size = 14)


############################################################################################
############################# EXPLORATORY DATA ANALYSIS  ###################################
############################################################################################

##########################################
############ EDA for all data  ###########
##########################################

# View the summary
summary(allData)

# Count the number of unique values
table(allData$Communication )
#  cellular telephone 
# 2831       267 

table(allData$past_Outcome)
#failure   other success 
# 437     195     326

table(allData$Y_AcceptedOffer)
#     Accepted DidNotAccept 
#  1604         2396 

table(allData$headOfhouseholdGender)
#  F    M 
#  1987 2013 

table(allData$EstRace)
# long table

table(allData$PetsPurchases)
#  FALSE  TRUE 
#  2047  1953

table(allData$AffluencePurchases)
#  FALSE  TRUE 
#  2038  1962 

table(allData$Job)
#  admin.   blue-collar  entrepreneur     housemaid    management       retired self-employed      services 
#  459           759           121           109           893           249           140           330 
#  student    technician    unemployed 
#  131           660           130 

table(allData$Marital)
#  divorced  married   single 
#  483     2304     1213 

table(allData$Education)
#  primary secondary  tertiary 
#  561      1988      1282 

table(allData$carMake)
# long table

table(allData$carModel)
# long table

table(allData$carYr)
#  1960 1964 1965 1967 1968 1971 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 
#  2    3    1    2    1    2    7    6    4    4    4    4    5    2    1    2    4    1    2    4    1    4    3 
#  1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
#  3    2    4    5   34   49   55   56   45   56   66   41  165  167  119  140  140  131  114  154  115  163  149 
#  2013 2014 2015 2016 2017 2018 2019 
#  157  150  145  185  178  189  752 


#view column names
colnames(allData)

############################################################################################
################################### CHECK NA VALUES  #######################################
############################################################################################

# [1] "dataID"                   "HHuniqueID"               "Communication"            "LastContactDay"          
# [5] "LastContactMonth"         "NoOfContacts"             "DaysPassed"               "PrevAttempts"            
# [9] "past_Outcome"             "CallStart"                "CallEnd"                  "Y_AcceptedOffer"         
# [13] "headOfhouseholdGender"    "annualDonations"          "EstRace"                  "PetsPurchases"           
# [17] "DigitalHabits_5_AlwaysOn" "AffluencePurchases"       "Age"                      "Job"                     
# [21] "Marital"                  "Education"                "DefaultOnRecord"          "RecentBalance"           
# [25] "HHInsurance"              "CarLoan"                  "carMake"                  "carModel"                
# [29] "carYr"    


# loop over each column to count if there are NA in the joined data 

for (col in names(allData)) {
  na_count <- sum(is.na(allData[[col]]))
  if (na_count > 0) {
    print(paste("The column of:", col, "has", na_count, "NA values"))
    
  }
}

# [1] "The column of: Communication has 902 NA values"
# [1] "The column of: past_Outcome has 3042 NA values"
# [1] "The column of: Job has 19 NA values"
# [1] "The column of: Education has 169 NA values"
# [1] "The column of: carYr has 202 NA values"


# loop over each column to count if there are NA in the prospective customer data 

for (col in names(prospcustomers)) {
  na_count <- sum(is.na(prospcustomers[[col]]))
  if (na_count > 0) {
    print(paste("The column of: ", col, "has", na_count, "NA values"))
  }
}

# [1] "The column of:  Communication has 221 NA values"
# [1] "The column of:  past_Outcome has 757 NA values"
# [1] "The column of:  Y_AcceptedOffer has 1000 NA values"

# Will allow Vtreat to deal with NA values later


############################################################################################
#################################### SHAPE OF DATA   #######################################
############################################################################################

########################################################
############ Histogram for allData #####################
########################################################

#histogram for Age
ggplot(data = allData, aes(x = Age)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(limits= c(0,100), breaks = seq(0, 100, by = 5))+
  labs(x = 'Age', y= 'Number of Customers', title = 'Distribution of Age')


#histogram for LastContactDay
ggplot(data = allData, aes(x = LastContactDay)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(breaks = seq(0, 50, by = 5))+
  labs(x = 'LastContactDay', y= 'Number of Customers', title = 'Distribution of Last ContactDay')


#histogram for NoOfContacts
ggplot(data = allData, aes(x = NoOfContacts)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(breaks = seq(0, 40, by = 5))+
  labs(x = 'NoOfContacts', y= 'Number of Customers', title = 'Distribution of No Of Contacts')


#histogram for DaysPassed
ggplot(data = allData, aes(x = DaysPassed)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(limits= c(0,800), breaks = seq(0, 900, by = 50))+
  labs(x = 'DaysPassed', y= 'Number of Customers', title = 'Distribution of Days Passed')

#histogram for PrevAttempts
ggplot(data = allData, aes(x = PrevAttempts)) +
  geom_histogram(bins = 30, fill = "blue", color = 'white') +
  scale_x_continuous(breaks = seq(0, 30, by = 5))+
  labs(x = 'PrevAttempts', y= 'Number of Customers', title = 'Distribution of Prev Attempts')


#histogram for DigitalHabits_5_AlwaysOn
ggplot(data = allData, aes(x = DigitalHabits_5_AlwaysOn)) +
  geom_histogram(bins = 20, fill = "blue", color = 'white') +
  scale_x_continuous(limits= c(0,10), breaks = seq(0, 10, by = 1))+
  labs(x = 'DigitalHabits_5_AlwaysOn', y= 'Number of Customers', title = 'Distribution of DigitalHabits_5_AlwaysOn')


#histogram for RecentBalance
ggplot(data = allData, aes(x = RecentBalance)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  labs(x = 'RecentBalance', y= 'Number of Customers', title = 'Distribution of Recent Balance')

#histogram for Duration
ggplot(data = allData, aes(x = Duration)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  labs(x = 'Duration', y= 'Number of Customers', title = 'Distribution of Duration')

#histogram for HHInsurance
ggplot(data = allData, aes(x = HHInsurance)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  labs(x = 'HHInsurance', y= 'Number of Customers', title = 'Distribution of HHInsurance')

#histogram for CarLoan
ggplot(data = allData, aes(x = CarLoan)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  labs(x = 'CarLoan', y= 'Number of Customers', title = 'Distribution of CarLoan')


####################################################################
############ Categorigal bar plot for allData  #####################
####################################################################

#plot of cummunication
ggplot(allData, aes(x = Communication, fill = Communication)) +
  geom_bar() +
labs(x = "Communication", y = "Number of Customers", title = " Distribution of type of Communication")

# *******************************************************************
#plot of LastContactMonth

# Convert the abbreviated month to month 
allData$LastContactMonth <- as.Date(paste("2022", allData$LastContactMonth, "01", sep = "-"), format = "%Y-%b-%d")
allData$LastContactMonth <- format(allData$LastContactMonth, "%b")

print(allData$LastContactMonth)

#order the months 
allData$LastContactMonth <- factor(allData$LastContactMonth, levels = month.abb, ordered = TRUE)

#plot of LastContactMonth
ggplot(allData, aes(x = LastContactMonth, fill = LastContactMonth)) +
  geom_bar() +
  labs(x = "LastContactMonth", y = "Number of Customers", title = " Distribution of type of LastContactMonth")

# *******************************************************************

#plot of past_Outcome
ggplot(allData, aes(x = past_Outcome, fill = past_Outcome)) +
  geom_bar() +
  labs(x = "past_Outcome", y = "Number of Customers", title = " Distribution of type of past_Outcome")


# *******************************************************************

#plot of DigitalHabits_5_AlwaysOn
ggplot(allData, aes(x = DigitalHabits_5_AlwaysOn, fill = DigitalHabits_5_AlwaysOn)) +
  geom_bar() +
  labs(x = "DigitalHabits_5_AlwaysOn", y = "Number of Customers", title = " Distribution of type of DigitalHabits_5_AlwaysOn")

# *******************************************************************

#plot of HHInsurance
hhicolor <- c("lightblue", "darkblue")
freq <- table(allData$HHInsurance)
barplot(freq, main="House Hold Insurance Distribution", 
        xlab="HHInsurance", 
        ylab = "Number of Customers",
        col = hhicolor,
        names.arg = c("No (0)", "Yes (1)")) 

# *******************************************************************

#plot of DefaultOnRecord
hhicolor <- c("lightblue", "darkblue")
freq <- table(allData$DefaultOnRecord)
barplot(freq, main="Default On Record Distribution", 
        xlab ="Default On Record", 
        ylab = "Number of Customers",
        col = hhicolor,
        names.arg = c("No (0)", "Yes (1)")) 


# *******************************************************************
#plot of Y_AcceptedOffer
ggplot(allData, aes(x = Y_AcceptedOffer, fill = Y_AcceptedOffer)) +
  geom_bar() +
  labs(x = "Y_AcceptedOffer", y = "Number of Customers", title = " Distribution of type of Accepted of Offer")

# *******************************************************************
#plot of CarLoan
hhicolor <- c("lightblue", "darkblue")
freq <- table(allData$CarLoan)
barplot(freq, main="Car Loan Distribution", 
        xlab ="Car Loan", 
        ylab = "Number of Customers",
        col = hhicolor,
        names.arg = c("No (0)", "Yes (1)")) 

ggplot(allData, aes(x = CarLoan, fill = CarLoan)) +
  geom_bar() +
  labs(x = "CarLoan", 
       y = "Number of Customers", 
       title = " Distribution of type of CarLoanr")

# *******************************************************************

#plot of headOfhouseholdGender
ggplot(allData, aes(x = headOfhouseholdGender, fill = headOfhouseholdGender)) +
  geom_bar() +
  labs(x = "Gender head Of household", 
       y = "Number of Customers", 
       title = " Distribution of head Of household Gender")

# *******************************************************************

#plot of EstRace
ggplot(allData, aes(x = EstRace, fill = EstRace)) +
  geom_bar() +
  labs(x = "EstRace", y = "Number of Customers", title = " Distribution of EstRace")

# *******************************************************************


#plot of PetsPurchases
ggplot(allData, aes(x = PetsPurchases, fill = PetsPurchases)) +
  geom_bar() +
  labs(x = "PetsPurchases", y = "Number of Customers", title = " Distribution of PetsPurchases")

# *******************************************************************

#plot of DigitalHabits_5_AlwaysOn
ggplot(allData, aes(x = DigitalHabits_5_AlwaysOn, fill = DigitalHabits_5_AlwaysOn)) +
  geom_bar() +
  labs(x = "DigitalHabits_5_AlwaysOn", y = "Number of Customers", title = " Distribution of DigitalHabits_5_AlwaysOn")

# *******************************************************************

#plot of Job
ggplot(allData, aes(x = Job, fill = Job)) +
  geom_bar() +
  labs(x = "Job", y = "Number of Customers", title = " Distribution of Job") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# *******************************************************************

#plot of Marital
ggplot(allData, aes(x = Marital, fill = Marital)) +
  geom_bar() +
  labs(x = "Marital Status", y = "Number of Customers", 
       title = " Distribution of Marital Status")


# *******************************************************************

#plot of Education
ggplot(allData, aes(x = Education, fill = Education)) +
  geom_bar() +
  labs(x = "Education", y = "Number of Customers", title = " Distribution of Education")


# *******************************************************************

#plot of DefaultOnRecord
ggplot(allData, aes(x = DefaultOnRecord, fill = DefaultOnRecord)) +
  geom_bar() +
  labs(x = "DefaultOnRecord", y = "Number of Customers", title = " Distribution of DefaultOnRecord")


# *******************************************************************

#plot of carMake
ggplot(allData, aes(x = carMake, fill = carMake)) +
  geom_bar() +
  labs(x = "carMake", y = "Number of Customers", title = " Distribution of carMake")


# *******************************************************************

########################################################
############ Histogram for Prospective Customers ######
########################################################

#histogram for LastContactDay
ggplot(data = prospcustomers, aes(x = LastContactDay)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(breaks = seq(0, 50, by = 5))+
  labs(x = 'LastContactDay', y= 'Number of Customers', title = 'Distribution of Last ContactDay for Prospective Customers')

# *******************************************************************

#histogram for NoOfContacts
ggplot(data = prospcustomers, aes(x = NoOfContacts)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(breaks = seq(0, 50, by = 5))+
  labs(x = 'NoOfContacts', y= 'Number of Customers', title = 'Distribution of NoOfContacts for Prospective Customers')

# *******************************************************************

#histogram for DaysPassed
ggplot(data = prospcustomers, aes(x = DaysPassed)) +
  geom_histogram(bins = 100, fill = "blue", color = 'white') +
  scale_x_continuous(limits= c(0,800), breaks = seq(0, 900, by = 50))+
  labs(x = 'DaysPassed', y= 'Number of Customers', title = 'Distribution of Days Passed for Prospective Customers')

# *******************************************************************

#histogram for PrevAttempts
ggplot(data = prospcustomers, aes(x = PrevAttempts)) +
  geom_histogram(bins = 50, fill = "blue", color = 'white') +
  scale_x_continuous(breaks = seq(0, 30, by = 5))+
  labs(x = 'PrevAttempts', y= 'Number of Customers', title = 'Distribution of Prev Attemptsfor Prospective Customers')


###############################################################################
############ Categorigal bar plot for Prospective Customers  ##################
###############################################################################

#plot of cummunication
ggplot(prospcustomers, aes(x = Communication, fill = Communication)) +
  geom_bar() +
  labs(x = "Communication", y = "Number of Customers", title = " Distribution of type of Communication for prospective customers")


# *******************************************************************
#plot of LastContactMonth

# Convert the abbreviated month to month 
prospcustomers$LastContactMonth <- as.Date(paste("2022", prospcustomers$LastContactMonth, "01", sep = "-"), format = "%Y-%b-%d")
prospcustomers$LastContactMonth <- format(prospcustomers$LastContactMonth, "%b")

print(prospcustomers$LastContactMonth)

#order the months 
prospcustomers$LastContactMonth <- factor(prospcustomers$LastContactMonth, levels = month.abb, ordered = TRUE)

#plot of LastContactMonth
ggplot(prospcustomers, aes(x = LastContactMonth, fill = LastContactMonth)) +
  geom_bar() +
  labs(x = "LastContactMonth", y = "Number of Customers", title = " Distribution of type of Last Contact Month for prospective customers")

# *******************************************************************

#plot of past_Outcome
ggplot(prospcustomers, aes(x = past_Outcome, fill = past_Outcome)) +
  geom_bar() +
  labs(x = "past_Outcome", y = "Number of Customers", title = " Distribution of type of past_Outcome for prospective customers")

# *******************************************************************


############################################################################################
############################### PEARSON CORRELATION  #######################################
############################################################################################


# compute correlation matrix for numerical data
numerical_col <- c("Age", "RecentBalance","LastContactDay", "NoOfContacts", "DaysPassed", 
                   "PrevAttempts", "DefaultOnRecord", "HHInsurance", "CarLoan")

#do dataframe for the numerical data
df_num <- allData[numerical_col]

#calculate Pearson correlation
cor_matrix <- cor(df_num, method = "pearson")

#create correlation plot
corrplot(cor_matrix, method = 'shade', order = 'AOE', type = 'lower',
         tl.col = "black",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.8,
         tl.srt = 45,
         diag = FALSE,
         main = "Pearson Correlation of Current customers")


############################################################################################
############################### SPEARMAN CORRELATION  #######################################
############################################################################################

#calculate Spearman correlation
cor_matrix <- cor(df_num, method = "spearman")

#create correlation plot
corrplot(cor_matrix, method = 'shade', order = 'AOE', type = 'lower',
         tl.col = "black",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.8,
         tl.srt = 45,
         diag = FALSE,
         main = "Spearman Correlation of Current customers")

# There is not much of a correlation among different variables

# DaysPassed and PrevAttempts have a positive correlation with each other
#     It means that as the number of days since the last contact with a customer increases
#     the number of previous attempts to contact the customer also increases.
#     It could mean that customer is hard to reach or less responsive to attempts in
#     communication resulting in more attempts being made over a longer period of time



############################################################################################
############################### FEATURE ENGINEERING ########################################
############################################################################################


##################################################################################
# Add an Age band and Age bins
##################################################################################

# Create Age Band with four equal width bins 
allData$AgeBand <- cut(allData$Age, breaks = 4)

#view table of data
table(allData$AgeBand)

# Create Age Bin variable with custom bin ranges

allData$AgeBin <- 0
allData$AgeBin[allData$Age >= 17 & allData$Age < 26] <- 1
allData$AgeBin[allData$Age >= 26 & allData$Age < 40] <- 2
allData$AgeBin[allData$Age >= 40 & allData$Age < 61] <- 3
allData$AgeBin[allData$Age >= 61] <- 4
allData$AgeBin <- as.integer(allData$AgeBin)

#view table of data
table(allData$AgeBin)

#plot Age Bin
ggplot(allData, aes(x = factor(AgeBin), fill = factor(AgeBin))) +
  geom_bar() +
  xlab("Age Group") +
  ylab("Number of Customers") +
  ggtitle("Distribution of Age Groups") +
  scale_fill_discrete(name = "Age Group", labels = c("1: 17-25", "2: 26-39", "3: 40-60", "4: >= 61"))


##################################################################################
# Add a Balance Bin
##################################################################################

allData$BalanceBand <- cut(allData$RecentBalance, breaks=5)
table(allData$BalanceBand)

allData$BalanceBin <- 0
allData$BalanceBin[(allData$RecentBalance < 1000)] <- 1
allData$BalanceBin[(allData$RecentBalance >= 1000) & (allData$RecentBalance < 2000)] <- 2
allData$BalanceBin[(allData$RecentBalance >= 2000) & (allData$RecentBalance < 3000)] <- 3
allData$BalanceBin[(allData$RecentBalance >= 3000) & (allData$RecentBalance < 5000)] <- 4
allData$BalanceBin[(allData$RecentBalance >= 5000)] <- 5
allData$BalanceBin <- as.factor(allData$BalanceBin)

#view table
table(allData$BalanceBin)

#plot Age BalanceBin
ggplot(allData, aes(x = factor(BalanceBin), fill = factor(BalanceBin))) +
  geom_bar() +
  scale_fill_discrete(name = "Recent Balance Group", 
                      labels = c("1: < 1000", "2: 1000 - 2000", "3: 2000 - 3000", 
                                 "4: 3000 - 5000 ", "> 5000")) +
  xlab("Recent Balance Group") +
  ylab("Number of Customers") +
  ggtitle("Distribution of Balance Groups")


##################################################################################
# INTERACTION EFFECTS
##################################################################################
# Some of the interaction effects to consider would include:

# Communication and NoOfContacts
#communication can vary depending on the number of times that a customer has been contacted

#first change communication from factor to numeric
allData$Communication <- as.numeric(as.character(allData$Communication))

# Add a communication and contact interaction effect
allData$comm_contact <- allData$Communication * allData$NoOfContacts

# ***********************************************************************

# past_Outcome with DaysPassed
# It is possible that there is an effect of previous outcome on the time that has passed
# since the previous outcome

#first change past_Outcome from factor to numeric
allData$past_Outcome  <- as.numeric(as.character(allData$past_Outcome ))

allData$outcome_dayspassed <- allData$past_Outcome * allData$DaysPassed

# ***********************************************************************

#RecentBalance with HHInsurance
# It may be possible that there is an impact of recent balance on acceptance of offer
# depending on whether the customer has a household insurance or not
  
allData$balance_HHinsurance <- allData$RecentBalance * allData$HHInsurance


# DaysPassed with NoOfContact Interaction for allData
allData$dayspassed_contact <- allData$DaysPassed * allData$NoOfContacts

# DaysPassed with NoOfContact Interaction for Prospective customers data
prospcustomers$dayspassed_contact <- prospcustomers$DaysPassed * prospcustomers$NoOfContacts


##################################################################################
# NORMALIZE
##################################################################################

# Define normalize function
normalize_minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

########## Apply normalization function for NoOfContacts #########

# Normalize NoOfContats for allData
allData$NoOfContacts_normalized <- normalize_minmax(allData$NoOfContacts)

# Normalize NoOfContats for prospcustomers
prospcustomers$NoOfContacts_normalized <- normalize_minmax(prospcustomers$NoOfContacts)

######### Apply normalization function for DaysPassed #########

# Normalize DaysPassedfor allData
allData$DaysPassed_normalized <- normalize_minmax(allData$DaysPassed)

# Normalize DaysPassed for prospcustomers
prospcustomers$DaysPassed_normalized <- normalize_minmax(prospcustomers$DaysPassed)

######### Apply normalization function for LastContactDay #########

# Normalize LastContactDay for allData
allData$LastContactDay_normalized <- normalize_minmax(allData$LastContactDay)

# Normalize LastContactDay for prospcustomers
prospcustomers$LastContactDay_normalized <- normalize_minmax(prospcustomers$LastContactDay)


##################################################################################
# SUCCESS RATIO
##################################################################################

# Add a feature that looks at the no of contacts based on the success (Offer accepted)

# Success ratio for allData
allData$success_ratio <- allData$NoOfContacts / sum(allData$Y_AcceptedOffer == 1 & allData$past_Outcome == "success")

# Success ratio for prospcustomers
prospcustomers$success_ratio <- prospcustomers$NoOfContacts / sum(prospcustomers$Y_AcceptedOffer == 1 & prospcustomers$past_Outcome == "success")

colnames(allData)

############################################################################################
############################### FEATURE SELECTION  #########################################
############################################################################################


# Select the variables, intially select all before selecting optimum based on feature importance
keeps <- c("Communication", "LastContactDay", "LastContactMonth", "NoOfContacts", 
           "DaysPassed","PrevAttempts", "past_Outcome","Duration", "headOfhouseholdGender",
           "annualDonations", "EstRace", "PetsPurchases", "DigitalHabits_5_AlwaysOn", "Age", 
           "AffluencePurchases", "Job", "Marital", "Education","CarLoan", "DefaultOnRecord", "HHInsurance",
           "AgeBin", "BalanceBin", "comm_contact", "outcome_dayspassed", "balance_HHinsurance",
           "NoOfContacts_normalized", "DaysPassed_normalized", "dayspassed_contact", 
           "LastContactDay_normalized", "success_ratio","Y_AcceptedOffer" )

# From correlation plot, PrevAttempts is highly correlated with DaysPassed, therefore may need to exclude one of the factors

############################################################################################
############################### VTREAT #####################################################
############################################################################################


# Split to get 10% variable treatment, 75% training & 15% validation
set.seed(1407) # for reproducibility
trainPercentRows      <- round(nrow(allData) %*% .75)
validationPercentRows <- round(nrow(allData) %*% .15)


# Sample index for training
trainIdx <- sample(1:nrow(allData), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(allData), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- allData[trainIdx,  names(allData) %in% keeps]
validationSet <- allData[validationIdx, names(allData) %in% keeps]

# Combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
prepData <- allData[-c(trainIdx, validationIdx), names(allData) %in% keeps]

### THIS IS CLASSIFICATION SO C IS USED, treatment with cross-validation frame to avoid overfitting
plan <- designTreatmentsC(dframe        = prepData, 
                          varlist       = keeps, 
                          outcomename   = "Y_AcceptedOffer",
                          outcometarget = "Accepted")


# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)

# treatedTrain is the training set for modeling
# treatedValidation is the validation set for assessment
# treatedProspective is the set to choose prospective customer to choose from

# Observe the data converted to onehotencoding
str(treatedTrain)
str(treatedValidation)
str(prepData)

# Col names
colnames(treatedTrain)


############################################################################################
################################## LOGISTIC REGRESSION   ###################################
############################################################################################

# Define the training control
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the logistiic model by using the Caret package
set.seed(1407)
logit_model <- train(as.factor(Y_AcceptedOffer) ~ ., data = treatedTrain,
                     method = "glm",
                     trControl = train_control,
                     family = "binomial")

# Examine the logistic regression model
logit_model

# Make predictions on the treated validation set
predicted_treatedVal <- predict(logit_model, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1   2
# Accepted              155  36
# DidNotAccept           74 335

# Calculate the accuracy or performance of the logistic model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy of logistic model on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.817

# Plot ROC curve
predicted_prob <- predict(logit_model, treatedValidation, type = "prob")
roc_obj_logit <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_logit, main = "ROC Curve of Logistic Regression Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_logit <- auc(roc_obj_logit)
cat("AUC:", round(AUC_logit, 4), "\n")
# AUC: 0.8945


############################################################################################
############### STEPWISE LOGISTIC REGRESSION MODEL - FOWARD ################################
############################################################################################

#Ref Datacamp
# https://campus.datacamp.com/courses/supervised-learning-in-r-classification/logistic-regression-f554c8d0-a6b9-4f33-b13e-fab145319b9e?ex=15


# Specify a null model with no predictors
null_model <- glm(Y_AcceptedOffer ~ 1, data = treatedTrain, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(Y_AcceptedOffer ~ ., data = treatedTrain, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")


# Examine the stepwise logistic regression model
step_model

# Make predictions on the validation set
validation_pred <- predict(step_model, newdata = treatedValidation, type = "response")

# Convert probabilities to class predictions (0 or 1)
validation_pred_pred_class <- ifelse(validation_pred >= 0.5, 1, 0)

# Create the confusion matrix
confusion_matrix <- table(treatedValidation$Y_AcceptedOffer, validation_pred_pred_class)

# Print the confusion matrix
print(confusion_matrix)

# validation_pred_pred_class
#              0   1
# Accepted     154  75
# DidNotAccept  36 335

# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.815 

# Plot the ROC curve
roc_obj_steplogit <- roc(treatedValidation$Y_AcceptedOffer, validation_pred)
plot(roc_obj_steplogit, main = "ROC Curve of stepwise logistic regression model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_steplogit <- auc(roc_obj_steplogit)
cat("AUC:", round(AUC_steplogit, 4), "\n")
# AUC: 0.8942 

# Stepwise logistic regression has lower accuracy than normal logistic regression

############################################################################################
############################# DECISION TREE RPART - MAXDEPTH_POST PRUNING  #################
############################################################################################

# Reference: https://campus.datacamp.com/courses/supervised-learning-in-r-classification/classification-trees-4?ex=2
# Reference: https://campus.datacamp.com/courses/supervised-learning-in-r-classification/classification-trees-4?ex=3


########################
##### pre-pruning #####
########################

# Create a decision tree model
# Consider pruning with cp set to 0 and maxdepth = 5 ; alternative try minsplit = 600 instead of maxdepth
# Pre-pruning is performed when building the decision tree model. 
# The rpart-dot-control function can be supplied with a maxdepth parameter that controls 
# the maximum depth of the decision tree, or a minsplit parameter that dictates the 
# minimum number of observations a branch must contain in order for the tree to be allowed to split.

treeModel <- rpart(Y_AcceptedOffer ~ ., data = treatedTrain, 
                   method = "class", 
                   control = rpart.control(cp = 0, maxdepth = 5))

# Get predicted probabilities from the model
predicted_prob <- predict(treeModel, newdata = treatedValidation)[, "DidNotAccept"]

# Make predictions on treatedValidation test set
predicted_class <- predict(treeModel, 
                           newdata = treatedValidation, 
                           type = "class")

# Examine the treeMOdel
treeModel

# Summary the treeMOdel
summary(treeModel)

# Plot the treeModel with default settings
rpart.plot(treeModel, main = "Decision Tree Model with Maxdepth of 5")

# Plot the treeMOdel with customized settings
rpart.plot(treeModel, type = 3, box.palette = c("green", "orange"), fallen.leaves = TRUE)

#Evaluate model performance on treatedValidation set
accuracy <- mean(predicted_class == treatedValidation$Y_AcceptedOffer)
cat("Accuracy on treeModel treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treeModel treatedValidation test set: 0.82

# ****************************************

#Create a confusion matrix
confusion_matrix <- table(predicted_class, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_class Accepted DidNotAccept
# Accepted          164         43
# DidNotAccept       65         328

# ****************************************

# Create a data frame with predicted probabilites
roc_treeModel <- data.frame(prob = predicted_prob, actual = treatedValidation$Y_AcceptedOffer)

# Sort dataframe 
roc_treeModel <- roc_treeModel[order(-roc_treeModel$prob), ]

# Calculate true 
TPR <- cumsum(roc_treeModel$actual == "DidNotAccept") / sum(roc_treeModel$actual == "DidNotAccept")
FPR <- cumsum(roc_treeModel$actual == "Accepted") / sum(roc_treeModel$actual == "Accepted")

# Plot ROC curve
plot(FPR, TPR, type = "l", col = "red",
     xlab = "False Positive Rate (FPR)", 
     ylab = "True Positive Rate (TPR)", main = "ROC Curve of the Decision Tree")
abline(0, 1, lty = 2, col = "gray")

# Calculate and print the AUC
roc_obj_treeModel <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob, levels = c("DidNotAccept", "Accepted"))
AUC_treeModel <- auc(roc_obj_treeModel)
cat("AUC:", round(AUC_treeModel, 4), "\n")
# AUC: 0.8498  


# ****************************************


########################
##### post-pruning #####
########################

# Post-pruning is applied to a decision tree model that has been previously built. 
# The plotcp function will generate a visualization of the error rate versus model 
# complexity, which provides insight into the optimal cutpoint for pruning. 
# When this value has been identified, it can be supplied to the prune function's 
# complexity parameter, cp, to create a simpler pruned tree.

treeModel <- rpart(Y_AcceptedOffer ~ ., data = treatedTrain, 
                   method = "class", 
                   control = rpart.control(cp = 0))


# Examine the complexity plot
plotcp(treeModel)

# Prune the tree
#Based on the complexity plot, the tree can be pruned at ~0.0035
treeModel_pruned <- prune(treeModel, cp = 0.0035)


# Summary the pruned treeMOdel
summary(treeModel_pruned)

# Plot the pruned treeModel with default settings
rpart.plot(treeModel_pruned, main = "Pruned Decision Tree Model")

# Plot the pruned treeMOdel with customized settings
rpart.plot(treeModel_pruned, type = 3, box.palette = c("green", "red"), fallen.leaves = TRUE)

# ****************************************

# Make predictions on treatedValidation test set
predicted_class <- predict(treeModel_pruned, 
                           newdata = treatedValidation, 
                           type = "class")

# Calculate actual class
actual_class <- treatedValidation$Y_AcceptedOffer

#Evaluate model performance on treatedValidation test set
accuracy <- sum(predicted_class == actual_class) / length(actual_class)
cat("Accuracy of pruned decision tree model on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy of pruned decision tree model on treatedValidation test set: 0.832  

# ****************************************
# Create confusion matrix
confusion_matrix <- table(predicted_class, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_class Accepted DidNotAccept
# Accepted          173         43
# DidNotAccept       56         328


# ****************************************

# Get predicted probabilities from the model
predicted_prob <- predict(treeModel_pruned, newdata = treatedValidation)[, "DidNotAccept"]

# Calculate the ROC curve
roc_obj_treeprune <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob)

# Plot the ROC curve
plot(roc_obj_treeprune, main = "ROC Curve for Pruned Decision Tree Model", col = "red")
# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate and print the AUC
auc_obj_treeprune <- auc(roc_obj_treeprune)
cat("AUC for Pruned Decision Tree Model:", round(auc_obj_treeprune, 3), "\n")
# AUC for Pruned Decision Tree Model: 0.853   


############################################################################################
############################ DECISION TREE RPART TRAIN  ####################################
############################################################################################


# Fit a decision tree with caret
set.seed(1407)
DecisionTreeModel <- train(as.factor(Y_AcceptedOffer) ~., #formula based
                           data = treatedTrain, #data in
                           #"recursive partitioning (trees)
                           method = "rpart", 
                           #Define a range for the CP to test
                           tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
                           #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
                           control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
DecisionTreeModel

# Plot the CP Accuracy Relationship to adjust the tune Grid inputs
plot(DecisionTreeModel)

# Plot a pruned tree
prp(DecisionTreeModel$finalModel, extra = 1)

# Make some predictions on the treated validation set
predicted_treatedVal <- predict(DecisionTreeModel, treatedValidation)
head(predicted_treatedVal)

#Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_treatedVal Accepted DidNotAccept
# Accepted          180           57
# DidNotAccept       49          314

# ****************************************

# Accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation set: 0.823

# ****************************************

# Plot ROC, to be able to plot the predicted values must be changed to numeric

# Convert the predicted_traedVal to factor
factor_vars <- sapply(treatedValidation, is.factor)

#Convert the factor to numeric
treatedValidation[factor_vars] <- lapply(treatedValidation[factor_vars], as.numeric)

# make predictions
predicted_treatedVal <- predict(DecisionTreeModel, treatedValidation)


#make predictions for probabilities
predicted_prob <- predict(DecisionTreeModel, treatedValidation, type = "prob")

# calculate the ROC curve
roc_obj_treetrain <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[,2])


#Plot ROC curve
plot(roc_obj_treetrain, main = "ROC Curve for Decision Tree Model Pruned", col = "red")
# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate and print the AUC
auc_value <- auc(roc_obj_treetrain)
cat("AUC for Decision Tree Model Pruned:", round(auc_value, 3), "\n")
# AUC for Decision Tree Model Pruned: 0.85 


############################################################################################
################################## RANDOM FOREST - RANGER   ################################
############################################################################################

# Fit a random forest model with Ranger
# Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data.
# Reference: 
# Use a method to find optimum parameters to obtain the best accuracy

# Define the parameter grid for the random forest
parameter_grid <- expand.grid(
  mtry = c(1, 2, 4, 6, 8), 
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Define the settings for the cross validation 
control_set <- trainControl(
  method = "cv", 
  number = 5, 
  verboseIter = FALSE,
  savePredictions = "final"
)

# Initial Training of the model
randomForestModel_ranger_initial <- train(as.factor(Y_AcceptedOffer) ~ .,
                                  data  = treatedTrain, 
                                  method = 'ranger',
                                  trControl = control_set,
                                  tuneGrid = parameter_grid,
                                  num.trees = 120,
                                  importance = 'permutation')

# Get the best hyperparameters with the associated accuracy
best_mtry <- randomForestModel_ranger_initial$bestTune$mtry
best_split_rule <- randomForestModel_ranger_initial$bestTune$splitrule
best_min_node_size <- randomForestModel_ranger_initial$bestTune$min.node.size
best_accuracy <- randomForestModel_ranger_initial$results$Accuracy[randomForestModel_ranger_initial$bestTuneRow]


# Final Training of the model with the best 
randomForestModel_ranger <- ranger(as.factor(Y_AcceptedOffer) ~ .,
                                  data  = treatedTrain, 
                                  num.trees = 120,
                                  importance = 'permutation',
                                  mtry = best_mtry,
                                  splitrule = best_split_rule,
                                  min.node.size = best_min_node_size)

# Look at improved var importance
varImpDF <- data.frame(variables = names(importance(randomForestModel_ranger)),
                       importance = importance(randomForestModel_ranger),
                       row.names = NULL)
varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]

ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', fill = '#3F51B5', alpha = 0.9, width = 0.8) + 
  ggtitle('Variable Importance using Random Forest Ranger') + 
  theme_gdocs() +
  theme(axis.text.y = element_text(size = 8))

# *************************************************
#Confusion matrix

# predict on the treated Validation set
predicted_treatedVal <- predict(randomForestModel_ranger, data = treatedValidation)$predictions


# Create confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1     2
# Accepted               185   48
# DidNotAccept           44   323

# *************************************************

# Accuracy
accuracy_randomForestModel_ranger <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on randomForestModel_ranger treatedValidation set:", round(accuracy_randomForestModel_ranger, 3), "\n")
# Accuracy on randomForestModel_ranger treatedValidation set: 0.847 

# *************************************************
# ROC curve

# Make prediction on the treated Validation set
predicted_prob <- predict(randomForestModel_ranger, data = treatedValidation, type = 'response')

# convert prediction to numeric
predicted_prob <- as.numeric(predicted_prob$predictions == "Accepted")

# Calculate the ROC curve
roc_obj_randomforest_ranger <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob)


# Plot ROC curve
plot(roc_obj_randomforest_ranger, 
     main = "ROC Curve for Random Forest Ranger Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")

# *************************************************
# Calculate AUC
AUC_randomForestModel_ranger <- auc(roc_obj_randomforest_ranger)
cat("AUC:", round(AUC_randomForestModel_ranger, 4), "\n")



############################################################################################
################################## RANDOM FOREST - TRAIN   #################################
############################################################################################

# Fit a random forest model with caret
set.seed(1407)
randomForestModel_train <- train(as.factor(Y_AcceptedOffer) ~ .,
                             data = treatedTrain,
                             method = "rf",
                             trControl = trainControl(method = "cv", number = 5),
                             tuneGrid = expand.grid(mtry = 1:4))

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 4.

# Examine the model
randomForestModel_train

# Check for the variable in order of importance
importance <- varImp(randomForestModel_train, scale = F)


# ************************************************

# Plot the variable importance
plot(importance, top = 30, 
     main = "Variable Importance using Random Forest Train Top 30", 
     ylab = "Importance", las = 2, cex.axis = 0.8)


# ************************************************

# Make predictions on the treated validation set
predicted_treatedVal <- predict(randomForestModel_train, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1     2
# Accepted               167   43
# DidNotAccept           62    328


# Calculate accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.825

# Plot ROC curve
predicted_prob <- predict(randomForestModel_train, treatedValidation, type = "prob")
roc_obj_randomforest_train <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_randomforest_train, main = "ROC Curve for Random Forest Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_randomforest <- auc(roc_obj_randomforest_train)
cat("AUC:", round(AUC_randomforest, 4), "\n")
# AUC: 0.9098


############################################################################################
######################################## KNN   #############################################
############################################################################################

# Define the training control
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the kNN model by using the Caret package
set.seed(1407)
knn_model <- train(as.factor(Y_AcceptedOffer) ~ ., data = treatedTrain,
                   method = "knn",
                   trControl = train_control,
                   tuneGrid = expand.grid(k = 1:20))

# Examine the kNN model
knn_model
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 9.

# Make predictions on the treated validation set
predicted_treatedVal <- predict(knn_model, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1   2
# Accepted              158  48
# DidNotAccept           71 323

# Calculate the accuracy or performance of the knn model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.802

# Plot ROC curve
predicted_prob <- predict(knn_model, treatedValidation, type = "prob")
roc_obj_knn <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_knn, main = "ROC Curve of k-NN Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_knn <- auc(roc_obj_knn)
cat("AUC:", round(AUC_knn, 4), "\n")
# AUC: 0.8636


##################################  PLOT ALL ROC ON ONE PLOT  ##########################

# plot the first ROC curve
plot(roc_obj_logit, main = "ROC Curves", col = "gray")

# Plot all the other ROC
plot(roc_obj_steplogit, add = TRUE, col = "blue")
plot(roc_obj_treeModel, add = TRUE, col = "green")
plot(roc_obj_treeprune, add = TRUE, col = "purple")
plot(roc_obj_treetrain, add = TRUE, col = "orange")
plot(roc_obj_randomforest_ranger, add = TRUE, col = "black")
plot(roc_obj_randomforest_train, add = TRUE, col = "red")
plot(roc_obj_knn, add = TRUE, col = "brown")


# Add Legend
legend("bottomright", legend = c("Logistic Regression", "Stepwise Logistic Regression",
                                 "Tree Model", "Pruned Tree Model", "Tree Train Model",
                                 "Random Forest Ranger Model", "Random Forest Train Model", 
                                 "K-Nearest Neighbors"),
       col = c("gray", "blue", "green", "purple", "orange", "black", "red", "brown"), lty = 1)


############################################################################################
############################### FEATURE SELECTION - PARSIMONIOUS  #########################################
############################################################################################
colnames(allData)
# Select the variables, that were found to be most important based on combination of Random Forest
# Ranger and Random Forest Train

keeps <- c("Duration", "Age", "LastContactMonth","LastContactDay", "LastContactDay_normalized", 
           "DaysPassed",  "DaysPassed_normalized", "dayspassed_contact" , "Job",
           "DigitalHabits_5_AlwaysOn","HHInsurance","NoOfContacts_normalized","AgeBin",
           "PrevAttempts" , "EstRace" , "BalanceBin", "Education",  "Marital",
            "Y_AcceptedOffer" )

############################################################################################
############################### VTREAT #####################################################
############################################################################################


# Split the data into training, validation and test. To get 10% variable treatment, 75% training & 15% validation
set.seed(1407) # for reproducibility
trainPercentRows      <- round(nrow(allData) %*% .75)
validationPercentRows <- round(nrow(allData) %*% .15)

# Sample index for training
trainIdx <- sample(1:nrow(allData), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(allData), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- allData[trainIdx,  names(allData) %in% keeps]
validationSet <- allData[validationIdx, names(allData) %in% keeps]

# Combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
prepData <- allData[-c(trainIdx, validationIdx), names(allData) %in% keeps]

### THIS IS CLASSIFICATION SO C IS USED, treatment with cross-validation frame to avoid overfitting
plan <- designTreatmentsC(dframe        = prepData, 
                          varlist       = keeps, 
                          outcomename   = "Y_AcceptedOffer",
                          outcometarget = "Accepted")


# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)

# treatedTrain is the training set for modeling
# treatedValidation is the validation set for assessment
# treatedProspective is the set to choose prospective customer to choose from

str(treatedTrain)
str(treatedValidation)
str(prepData)

############################################################################################
################################## LOGISTIC REGRESSION   ###################################
############################################################################################

# Define the training control
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the logistiic model by using the Caret package
set.seed(1407)
logit_model <- train(as.factor(Y_AcceptedOffer) ~ ., data = treatedTrain,
                     method = "glm",
                     trControl = train_control,
                     family = "binomial")

# Examine the logistic regression model
logit_model

# Make predictions on the treated validation set
predicted_treatedVal <- predict(logit_model, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1   2
# Accepted              152  34
# DidNotAccept           77 337

# Calculate the accuracy or performance of the logistic model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy of logistic model on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.798

# Plot ROC curve
predicted_prob <- predict(logit_model, treatedValidation, type = "prob")
roc_obj_logit <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_logit, main = "ROC Curve of Logistic Regression Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_logit <- auc(roc_obj_logit)
cat("AUC:", round(AUC_logit, 4), "\n")
# AUC: 0.8941


############################################################################################
############### STEPWISE LOGISTIC REGRESSION MODEL - FOWARD ################################
############################################################################################

#Ref Datacamp
# https://campus.datacamp.com/courses/supervised-learning-in-r-classification/logistic-regression-f554c8d0-a6b9-4f33-b13e-fab145319b9e?ex=15


# Specify a null model with no predictors
null_model <- glm(Y_AcceptedOffer ~ 1, data = treatedTrain, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(Y_AcceptedOffer ~ ., data = treatedTrain, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, 
                                            upper = full_model), direction = "forward")


# Examine the stepwise logistic regression model
step_model

# Make predictions on the validation set
validation_pred <- predict(step_model, newdata = treatedValidation, type = "response")

# Convert probabilities to class predictions (0 or 1)
validation_pred_pred_class <- ifelse(validation_pred >= 0.5, 1, 0)

# Create the confusion matrix
confusion_matrix <- table(treatedValidation$Y_AcceptedOffer, validation_pred_pred_class)

# Print the confusion matrix
print(confusion_matrix)
#              validation_pred_pred_class
#                            0    1
#               Accepted     151  78
#               DidNotAccept  35 336

# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.812

# Plot the ROC curve
roc_obj_steplogit <- roc(treatedValidation$Y_AcceptedOffer, validation_pred)
plot(roc_obj_steplogit, main = "ROC Curve of stepwise logistic regression model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_steplogit <- auc(roc_obj_steplogit)
cat("AUC:", round(AUC_steplogit, 4), "\n")
# AUC: 0.894

# Stepwise logistic regression has same accuracy than normal logistic regression

############################################################################################
############################# DECISION TREE RPART - MAXDEPTH_POST PRUNING  #################
############################################################################################

# Reference: https://campus.datacamp.com/courses/supervised-learning-in-r-classification/classification-trees-4?ex=2
# Reference: https://campus.datacamp.com/courses/supervised-learning-in-r-classification/classification-trees-4?ex=3


########################
##### pre-pruning #####
########################

# Create a decision tree model
# Consider pruning with cp set to 0 and maxdepth = 5 ; alternative try minsplit = 600 instead of maxdepth
# Pre-pruning is performed when building the decision tree model. 
# The rpart-dot-control function can be supplied with a maxdepth parameter that controls 
# the maximum depth of the decision tree, or a minsplit parameter that dictates the 
# minimum number of observations a branch must contain in order for the tree to be allowed to split.

treeModel <- rpart(Y_AcceptedOffer ~ ., data = treatedTrain, 
                   method = "class", 
                   control = rpart.control(cp = 0, maxdepth = 5))

# Get predicted probabilities from the model
predicted_prob <- predict(treeModel, newdata = treatedValidation)[, "DidNotAccept"]

# Make predictions on treatedValidation test set
predicted_class <- predict(treeModel, 
                           newdata = treatedValidation, 
                           type = "class")

# Examine the treeMOdel
treeModel

# Summary the treeMOdel
summary(treeModel)

# Plot the treeModel with default settings
rpart.plot(treeModel, main = "Decision Tree Model with Maxdepth of 5")

# Plot the treeMOdel with customized settings
rpart.plot(treeModel, type = 3, box.palette = c("green", "orange"), fallen.leaves = TRUE)

#Evaluate model performance on treatedValidation set
accuracy <- mean(predicted_class == treatedValidation$Y_AcceptedOffer)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.818

# ****************************************

#Create a confusion matrix
confusion_matrix <- table(predicted_class, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_class Accepted DidNotAccept
# Accepted          164         43
# DidNotAccept       65         327

# ****************************************

# Create a data frame with predicted probabilites
roc_treeModel <- data.frame(prob = predicted_prob, actual = treatedValidation$Y_AcceptedOffer)

# Sort dataframe 
roc_treeModel <- roc_treeModel[order(-roc_treeModel$prob), ]

# Calculate true 
TPR <- cumsum(roc_treeModel$actual == "DidNotAccept") / sum(roc_treeModel$actual == "DidNotAccept")
FPR <- cumsum(roc_treeModel$actual == "Accepted") / sum(roc_treeModel$actual == "Accepted")

# Plot ROC curve
plot(FPR, TPR, type = "l", col = "red",
     xlab = "False Positive Rate (FPR)", 
     ylab = "True Positive Rate (TPR)", main = "ROC Curve of the Decision Tree")
abline(0, 1, lty = 2, col = "gray")

# Calculate and print the AUC
roc_obj_treeModel <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob, levels = c("DidNotAccept", "Accepted"))
AUC_treeModel <- auc(roc_obj_treeModel)
cat("AUC:", round(AUC_treeModel, 4), "\n")
# AUC: 0.8491


# ****************************************


########################
##### post-pruning #####
########################

# Post-pruning is applied to a decision tree model that has been previously built. 
# The plotcp function will generate a visualization of the error rate versus model 
# complexity, which provides insight into the optimal cutpoint for pruning. 
# When this value has been identified, it can be supplied to the prune function's 
# complexity parameter, cp, to create a simpler pruned tree.

treeModel <- rpart(Y_AcceptedOffer ~ ., data = treatedTrain, 
                   method = "class", 
                   control = rpart.control(cp = 0))


# Examine the complexity plot
plotcp(treeModel)

# Prune the tree
#Based on the complexity plot, the tree can be pruned at 0.0034
treeModel_pruned <- prune(treeModel, cp = 0.0034)


# Summary the pruned treeMOdel
summary(treeModel_pruned)

# Plot the pruned treeModel with default settings
rpart.plot(treeModel_pruned, main = "Pruned Decision Tree Model")

# Plot the pruned treeMOdel with customized settings
rpart.plot(treeModel_pruned, type = 3, box.palette = c("green", "red"), fallen.leaves = TRUE)

# ****************************************

# Make predictions on treatedValidation test set
predicted_class <- predict(treeModel_pruned, 
                           newdata = treatedValidation, 
                           type = "class")

# Calculate actual class
actual_class <- treatedValidation$Y_AcceptedOffer

#Evaluate model performance on treatedValidation test set
accuracy <- sum(predicted_class == actual_class) / length(actual_class)
cat("Accuracy of pruned decision tree model on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy of pruned decision tree model on treatedValidation test set: 0.832 


# ****************************************
# Create confusion matrix
confusion_matrix <- table(predicted_class, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_class Accepted DidNotAccept
# Accepted          175         47
# DidNotAccept       54         324


# ****************************************

# Get predicted probabilities from the model
predicted_prob <- predict(treeModel_pruned, newdata = treatedValidation)[, "DidNotAccept"]

# Calculate the ROC curve
roc_obj_treeprune <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob)

# Plot the ROC curve
plot(roc_obj_treeprune, main = "ROC Curve for Pruned Decision Tree Model", col = "red")
# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate and print the AUC
auc_obj_treeprune <- auc(roc_obj_treeprune)
cat("AUC for Pruned Decision Tree Model:", round(auc_obj_treeprune, 3), "\n")
# AUC for Pruned Decision Tree Model: 0.854


############################################################################################
############################ DECISION TREE RPART TRAIN  ####################################
############################################################################################


# Fit a decision tree with caret
set.seed(1407)
DecisionTreeModel <- train(as.factor(Y_AcceptedOffer) ~., #formula based
                           data = treatedTrain, #data in
                           #"recursive partitioning (trees)
                           method = "rpart", 
                           #Define a range for the CP to test
                           tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
                           #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
                           control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
DecisionTreeModel
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.005.

# Plot the CP Accuracy Relationship to adjust the tune Grid inputs
plot(DecisionTreeModel)

# Plot the train_tree
prp(DecisionTreeModel$finalModel, extra = 1)

# Make predictions on the treated validation set
predicted_treatedVal <- predict(DecisionTreeModel, treatedValidation)
head(predicted_treatedVal)

#Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_treatedVal Accepted DidNotAccept
# Accepted          180           57
# DidNotAccept       49          314

# ****************************************
# Accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation set: 0.823  

# ****************************************

# Plot ROC, to be able to plot the predicted values must be changed to numeric

# Convert the predicted_traedVal to factor
factor_vars <- sapply(treatedValidation, is.factor)

#Convert the factor to numeric
treatedValidation[factor_vars] <- lapply(treatedValidation[factor_vars], as.numeric)

# make predictions
predicted_treatedVal <- predict(DecisionTreeModel, treatedValidation)


#make predictions for probabilities
predicted_prob <- predict(DecisionTreeModel, treatedValidation, type = "prob")

# calculate the ROC curve
roc_obj_treetrain <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[,2])


#Plot ROC curve
plot(roc_obj_treetrain, main = "ROC Curve for Decision Tree Model Pruned", col = "red")
# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate and print the AUC
auc_value <- auc(roc_obj_treetrain)
cat("AUC for Decision Tree Model Pruned:", round(auc_value, 3), "\n")
# AUC for Decision Tree Model Pruned: 0.850 


############################################################################################
################################## RANDOM FOREST - RANGER   ################################
############################################################################################

# Fit a random forest model with Ranger
# Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data.
# Reference: 
# Use a method to find optimum parameters to obtain the best accuracy

# Define the parameter grid for the random forest
parameter_grid <- expand.grid(
  mtry = c(1, 2, 4, 6, 8), 
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Define the settings for the cross validation 
control_set <- trainControl(
  method = "cv", 
  number = 5, 
  verboseIter = FALSE,
  savePredictions = "final"
)

# Initial Training of the model
randomForestModel_ranger_initial <- train(as.factor(Y_AcceptedOffer) ~ .,
                                          data  = treatedTrain, 
                                          method = 'ranger',
                                          trControl = control_set,
                                          tuneGrid = parameter_grid,
                                          num.trees = 120,
                                          importance = 'permutation')

# Get the best hyperparameters with the associated accuracy
best_mtry <- randomForestModel_ranger_initial$bestTune$mtry
best_split_rule <- randomForestModel_ranger_initial$bestTune$splitrule
best_min_node_size <- randomForestModel_ranger_initial$bestTune$min.node.size
best_accuracy <- randomForestModel_ranger_initial$results$Accuracy[randomForestModel_ranger_initial$bestTuneRow]


# Final Training of the model with the best 
randomForestModel_ranger <- ranger(as.factor(Y_AcceptedOffer) ~ .,
                                   data  = treatedTrain, 
                                   num.trees = 120,
                                   importance = 'permutation',
                                   mtry = best_mtry,
                                   splitrule = best_split_rule,
                                   min.node.size = best_min_node_size)

# Look at improved var importance
varImpDF <- data.frame(variables = names(importance(randomForestModel_ranger)),
                       importance = importance(randomForestModel_ranger),
                       row.names = NULL)
varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]

ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', fill = '#3F51B5', alpha = 0.9, width = 0.8) + 
  ggtitle('Variable Importance using Random Forest Ranger') + 
  theme_gdocs() +
  theme(axis.text.y = element_text(size = 8))

# *************************************************
#Confusion matrix

# predict on the treated Validation set
predicted_treatedVal <- predict(randomForestModel_ranger, data = treatedValidation)$predictions


# Create confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1     2
# Accepted               185   43
# DidNotAccept           44   328

# *************************************************

# Accuracy
accuracy_randomForestModel_ranger <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on randomForestModel_ranger treatedValidation set:", round(accuracy_randomForestModel_ranger, 3), "\n")
# Accuracy on randomForestModel_ranger treatedValidation set: 0.855 

# *************************************************
# ROC curve

# Make prediction on the treated Validation set
predicted_prob <- predict(randomForestModel_ranger, data = treatedValidation, type = 'response')

# convert prediction to numeric
predicted_prob <- as.numeric(predicted_prob$predictions == "Accepted")

# Calculate the ROC curve
roc_obj_randomforest_ranger <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob)


# Plot ROC curve
plot(roc_obj_randomforest_ranger, 
     main = "ROC Curve for Random Forest Ranger Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")

# *************************************************
# Calculate AUC
AUC_randomForestModel_ranger <- auc(roc_obj_randomforest_ranger)
cat("AUC:", round(AUC_randomForestModel_ranger, 4), "\n")
# AUC: 0.846 

############################################################################################
################################## RANDOM FOREST - TRAIN   #################################
############################################################################################

# Fit a random forest model with caret
set.seed(1407)
randomForestModel_train <- train(as.factor(Y_AcceptedOffer) ~ .,
                             data = treatedTrain,
                             method = "rf",
                             trControl = trainControl(method = "cv", number = 5),
                             tuneGrid = expand.grid(mtry = 1:4))

# Examine the model
randomForestModel_train

# Check for the variable in order of importance
importance <- varImp(randomForestModel_train, scale = F)


# ************************************************

# Plot the variable importance
plot(importance, top = 30, 
     main = "Variable Importance using Random Forest Train Top 30", 
     ylab = "Importance", las = 2, cex.axis = 0.8)


# ************************************************

# Make predictions on the treated validation set
predicted_treatedVal <- predict(randomForestModel_train, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1     2
# Accepted               176   46
# DidNotAccept           53    325

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.835

# Plot ROC curve
predicted_prob <- predict(randomForestModel_train, treatedValidation, type = "prob")
roc_obj_randomforest_train <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_randomforest_train, main = "ROC Curve for Random Forest Train Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_randomforest <- auc(roc_obj_randomforest_train)
cat("AUC:", round(AUC_randomforest, 4), "\n")
# AUC: 0.9126


############################################################################################
######################################## KNN   #############################################
############################################################################################

# Define the training control
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the kNN model by using the Caret package
set.seed(1407)
knn_model <- train(as.factor(Y_AcceptedOffer) ~ ., data = treatedTrain,
                   method = "knn",
                   trControl = train_control,
                   tuneGrid = expand.grid(k = 1:10))

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 9

# Examine the kNN model
knn_model

# Make predictions on the treated validation set
predicted_treatedVal <- predict(knn_model, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1   2
# Accepted              154  47
# DidNotAccept           75 324

# Calculate the accuracy or performance of the knn model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.797 

# Plot ROC curve
predicted_prob <- predict(knn_model, treatedValidation, type = "prob")
roc_obj_knn <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_knn, main = "ROC Curve of k-NN Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_knn <- auc(roc_obj_knn)
cat("AUC:", round(AUC_knn, 4), "\n")
# AUC: 0.8611 


##################################  PLOT ALL ROC ON ONE PLOT  ##########################

# plot the first ROC curve
plot(roc_obj_logit, main = "ROC Curves Parsimonious", col = "gray")

# Plot all the other ROC
plot(roc_obj_steplogit, add = TRUE, col = "blue")
plot(roc_obj_treeModel, add = TRUE, col = "green")
plot(roc_obj_treeprune, add = TRUE, col = "purple")
plot(roc_obj_treetrain, add = TRUE, col = "orange")
plot(roc_obj_randomforest_ranger, add = TRUE, col = "black")
plot(roc_obj_randomforest_train, add = TRUE, col = "red")
plot(roc_obj_knn, add = TRUE, col = "brown")

# Add Legend
legend("bottomright", legend = c("Logistic Regression", "Stepwise Logistic Regression",
                                 "Tree Model", "Pruned Tree Model", "Tree Train Model",
                                 "Random Forest Ranger Model", "Random Forest Train Model", 
                                 "K-Nearest Neighbors"),
       col = c("gray", "blue", "green", "purple", "orange", "black", "red", "brown"), lty = 1)



###########################################################################################
###########################################################################################
################ SELECTING MODEL TEST ON UNSEEN DATA - NEW CUSTOMERS   ####################
###########################################################################################
###########################################################################################

# View the columns 
colnames(allData)
colnames(prospcustomers)


# From Previous modelling, the Pruned Decision tree was the most optimum with accuracy of 0.832
# However as the prospective clients have less variables than in the existing customers,
# Update select to match columns in the prospective customers and redo the modeling
# to check the best model with updated variables

keeps_2 <- c("HHuniqueID", "Communication" , "LastContactDay", "LastContactMonth",
             "NoOfContacts", "NoOfContacts_normalized", "DaysPassed", "DaysPassed_normalized",
           "past_Outcome", "dayspassed_contact", "LastContactDay_normalized", "success_ratio",
           "Y_AcceptedOffer")

############################################################################################
############################### VTREAT #####################################################
############################################################################################


# Split the data into training, validation and test. To get 10% variable treatment, 75% training & 15% validation
set.seed(1407) # for reproducibility
trainPercentRows      <- round(nrow(allData) %*% .75)
validationPercentRows <- round(nrow(allData) %*% .15)

# Sample index for training
trainIdx <- sample(1:nrow(allData), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(allData), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- allData[trainIdx,  names(allData) %in% keeps_2]
validationSet <- allData[validationIdx, names(allData) %in% keeps_2]

# Combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
prepData <- allData[-c(trainIdx, validationIdx), names(allData) %in% keeps_2]

### THIS IS CLASSIFICATION SO C IS USED, treatment with cross-validation frame to avoid overfitting
plan <- designTreatmentsC(dframe        = prepData, 
                          varlist       = keeps_2, 
                          outcomename   = "Y_AcceptedOffer",
                          outcometarget = "Accepted")


# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)


# Apply treat to prospective customer data
treatedTestData <- prepare(plan, dframe = prospcustomers[, keeps_2])
treatedTestData$HHuniqueID <- prospcustomers$HHuniqueID


# treatedTrain is the training set for modeling
# treatedValidation is the validation set for assessment
# treatedProspective is the set to choose prospective customer to choose from

str(treatedTrain)
str(treatedValidation)
str(treatedTestData)

############################################################################################
################################## LOGISTIC REGRESSION   ###################################
############################################################################################

# Define the training control
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the logistiic model by using the Caret package
set.seed(1407)
logit_model <- train(as.factor(Y_AcceptedOffer) ~ ., data = treatedTrain,
                     method = "glm",
                     trControl = train_control,
                     family = "binomial")

# Examine the logistic regression model
logit_model

# Make predictions on the treated validation set
predicted_treatedVal <- predict(logit_model, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal Accepted DidNotAccept
# Accepted           64           35
# DidNotAccept      165          336

# Calculate the accuracy or performance of the logistic model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy of logistic model on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.798

# Plot ROC curve
predicted_prob <- predict(logit_model, treatedValidation, type = "prob")
roc_obj_logit <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_logit, main = "ROC Curve of Logistic Regression Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_logit <- auc(roc_obj_logit)
cat("AUC:", round(AUC_logit, 4), "\n")
# AUC: 0.8941


############################################################################################
############### STEPWISE LOGISTIC REGRESSION MODEL - FOWARD ################################
############################################################################################

#Ref Datacamp
# https://campus.datacamp.com/courses/supervised-learning-in-r-classification/logistic-regression-f554c8d0-a6b9-4f33-b13e-fab145319b9e?ex=15


# Specify a null model with no predictors
null_model <- glm(Y_AcceptedOffer ~ 1, data = treatedTrain, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(Y_AcceptedOffer ~ ., data = treatedTrain, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, 
                                            upper = full_model), direction = "forward")


# Examine the stepwise logistic regression model
step_model

# Make predictions on the validation set
validation_pred <- predict(step_model, newdata = treatedValidation, type = "response")

# Convert probabilities to class predictions (0 or 1)
validation_pred_pred_class <- ifelse(validation_pred >= 0.5, 1, 0)

# Create the confusion matrix
confusion_matrix <- table(treatedValidation$Y_AcceptedOffer, validation_pred_pred_class)

# Print the confusion matrix
print(confusion_matrix)

#validation_pred_pred_class
#               0   1
# Accepted      64 165
# DidNotAccept  35 336


# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.667

# Plot the ROC curve
roc_obj_steplogit <- roc(treatedValidation$Y_AcceptedOffer, validation_pred)
plot(roc_obj_steplogit, main = "ROC Curve of stepwise logistic regression model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_steplogit <- auc(roc_obj_steplogit)
cat("AUC:", round(AUC_steplogit, 4), "\n")
# AUC: 0.6652

# Stepwise logistic regression has same accuracy than normal logistic regression

############################################################################################
############################# DECISION TREE RPART - MAXDEPTH_POST PRUNING  #################
############################################################################################

# Reference: https://campus.datacamp.com/courses/supervised-learning-in-r-classification/classification-trees-4?ex=2
# Reference: https://campus.datacamp.com/courses/supervised-learning-in-r-classification/classification-trees-4?ex=3


########################
##### pre-pruning #####
########################

# Create a decision tree model
# Consider pruning with cp set to 0 and maxdepth = 5 ; alternative try minsplit = 600 instead of maxdepth
# Pre-pruning is performed when building the decision tree model. 
# The rpart-dot-control function can be supplied with a maxdepth parameter that controls 
# the maximum depth of the decision tree, or a minsplit parameter that dictates the 
# minimum number of observations a branch must contain in order for the tree to be allowed to split.

treeModel <- rpart(Y_AcceptedOffer ~ ., data = treatedTrain, 
                   method = "class", 
                   control = rpart.control(cp = 0, maxdepth = 5))

# Get predicted probabilities from the model
predicted_prob <- predict(treeModel, newdata = treatedValidation)[, "DidNotAccept"]

# Make predictions on treatedValidation test set
predicted_class <- predict(treeModel, 
                           newdata = treatedValidation, 
                           type = "class")

# Examine the treeMOdel
treeModel

# Summary the treeMOdel
summary(treeModel)

# Plot the treeModel with default settings
rpart.plot(treeModel, main = "Decision Tree Model with Maxdepth of 5")

# Plot the treeMOdel with customized settings
rpart.plot(treeModel, type = 3, box.palette = c("green", "orange"), fallen.leaves = TRUE)

#Evaluate model performance on treatedValidation set
accuracy <- mean(predicted_class == treatedValidation$Y_AcceptedOffer)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.7

# ****************************************

#Create a confusion matrix
confusion_matrix <- table(predicted_class, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_class Accepted DidNotAccept
# Accepted           78           29
# DidNotAccept      151          342

# ****************************************

# Create a data frame with predicted probabilites
roc_treeModel <- data.frame(prob = predicted_prob, actual = treatedValidation$Y_AcceptedOffer)

# Sort dataframe 
roc_treeModel <- roc_treeModel[order(-roc_treeModel$prob), ]

# Calculate true 
TPR <- cumsum(roc_treeModel$actual == "DidNotAccept") / sum(roc_treeModel$actual == "DidNotAccept")
FPR <- cumsum(roc_treeModel$actual == "Accepted") / sum(roc_treeModel$actual == "Accepted")

# Plot ROC curve
plot(FPR, TPR, type = "l", col = "red",
     xlab = "False Positive Rate (FPR)", 
     ylab = "True Positive Rate (TPR)", main = "ROC Curve of the Decision Tree")
abline(0, 1, lty = 2, col = "gray")

# Calculate and print the AUC
roc_obj_treeModel <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob, levels = c("DidNotAccept", "Accepted"))
AUC_treeModel <- auc(roc_obj_treeModel)
cat("AUC:", round(AUC_treeModel, 4), "\n")
# AUC: 0.6818 


# ****************************************


########################
##### post-pruning #####
########################

# Post-pruning is applied to a decision tree model that has been previously built. 
# The plotcp function will generate a visualization of the error rate versus model 
# complexity, which provides insight into the optimal cutpoint for pruning. 
# When this value has been identified, it can be supplied to the prune function's 
# complexity parameter, cp, to create a simpler pruned tree.

treeModel <- rpart(Y_AcceptedOffer ~ ., data = treatedTrain, 
                   method = "class", 
                   control = rpart.control(cp = 0))


# Examine the complexity plot
plotcp(treeModel)

# Prune the tree
#Based on the complexity plot, the tree can be pruned at 0.0034
treeModel_pruned <- prune(treeModel, cp = 0.0034)


# Summary the pruned treeMOdel
summary(treeModel_pruned)

# Plot the pruned treeModel with default settings
rpart.plot(treeModel_pruned, main = "Pruned Decision Tree Model")

# Plot the pruned treeMOdel with customized settings
rpart.plot(treeModel_pruned, type = 3, box.palette = c("green", "red"), fallen.leaves = TRUE)

# ****************************************

# Make predictions on treatedValidation test set
predicted_class <- predict(treeModel_pruned, 
                           newdata = treatedValidation, 
                           type = "class")

# Calculate actual class
actual_class <- treatedValidation$Y_AcceptedOffer

#Evaluate model performance on treatedValidation test set
accuracy <- sum(predicted_class == actual_class) / length(actual_class)
cat("Accuracy of pruned decision tree model on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy of pruned decision tree model on treatedValidation test set: 0.832 


# ****************************************
# Create confusion matrix
confusion_matrix <- table(predicted_class, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_class Accepted DidNotAccept
# Accepted           93           39
# DidNotAccept      136          332


# ****************************************

# Get predicted probabilities from the model
predicted_prob <- predict(treeModel_pruned, newdata = treatedValidation)[, "DidNotAccept"]

# Calculate the ROC curve
roc_obj_treeprune <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob)

# Plot the ROC curve
plot(roc_obj_treeprune, main = "ROC Curve for Pruned Decision Tree Model", col = "red")
# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate and print the AUC
auc_obj_treeprune <- auc(roc_obj_treeprune)
cat("AUC for Pruned Decision Tree Model:", round(auc_obj_treeprune, 3), "\n")
# AUC for Pruned Decision Tree Model: 0.697


############################################################################################
############################ DECISION TREE RPART TRAIN  ####################################
############################################################################################


# Fit a decision tree with caret
set.seed(1407)
DecisionTreeModel <- train(as.factor(Y_AcceptedOffer) ~., #formula based
                           data = treatedTrain, #data in
                           #"recursive partitioning (trees)
                           method = "rpart", 
                           #Define a range for the CP to test
                           tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
                           #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
                           control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
DecisionTreeModel
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.005.

# Plot the CP Accuracy Relationship to adjust the tune Grid inputs
plot(DecisionTreeModel)

# Plot the train_tree
prp(DecisionTreeModel$finalModel, extra = 1)

# Make predictions on the treated validation set
predicted_treatedVal <- predict(DecisionTreeModel, treatedValidation)
head(predicted_treatedVal)

#Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

#Print confusion matrix
print(confusion_matrix)

# predicted_treatedVal Accepted DidNotAccept
# Accepted           82           32
# DidNotAccept      147          339

# ****************************************
# Accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation set: 0.702   

# ****************************************

# Plot ROC, to be able to plot the predicted values must be changed to numeric

# Convert the predicted_traedVal to factor
factor_vars <- sapply(treatedValidation, is.factor)

#Convert the factor to numeric
treatedValidation[factor_vars] <- lapply(treatedValidation[factor_vars], as.numeric)

# make predictions
predicted_treatedVal <- predict(DecisionTreeModel, treatedValidation)


#make predictions for probabilities
predicted_prob <- predict(DecisionTreeModel, treatedValidation, type = "prob")

# calculate the ROC curve
roc_obj_treetrain <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[,2])


#Plot ROC curve
plot(roc_obj_treetrain, main = "ROC Curve for Decision Tree Model Pruned", col = "red")
# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate and print the AUC
auc_value <- auc(roc_obj_treetrain)
cat("AUC for Decision Tree Model Pruned:", round(auc_value, 3), "\n")
# AUC for Decision Tree Model Pruned: 0.672 

############################################################################################
################################## RANDOM FOREST - TRAIN   #################################
############################################################################################

# Fit a random forest model with caret
set.seed(1407)
randomForestModel_train <- train(as.factor(Y_AcceptedOffer) ~ .,
                                 data = treatedTrain,
                                 method = "rf",
                                 trControl = trainControl(method = "cv", number = 5),
                                 tuneGrid = expand.grid(mtry = 1:4))

# Examine the model
randomForestModel_train

# Check for the variable in order of importance
importance <- varImp(randomForestModel_train, scale = F)


# ************************************************

# Plot the variable importance
plot(importance, top = 18, 
     main = "Variable Importance using Random Forest Train Top 18", 
     ylab = "Importance", las = 2, cex.axis = 0.8)


# ************************************************

# Make predictions on the treated validation set
predicted_treatedVal <- predict(randomForestModel_train, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1   2
# Accepted     109  39
# DidNotAccept 120 332

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.735

# Plot ROC curve
predicted_prob <- predict(randomForestModel_train, treatedValidation, type = "prob")
roc_obj_randomforest_train <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_randomforest_train, main = "ROC Curve for Random Forest Train Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_randomforest <- auc(roc_obj_randomforest_train)
cat("AUC:", round(AUC_randomforest, 4), "\n")
# AUC: 0.7495


############################################################################################
######################################## KNN   #############################################
############################################################################################

# Define the training control
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train the kNN model by using the Caret package
set.seed(1407)
knn_model <- train(as.factor(Y_AcceptedOffer) ~ ., data = treatedTrain,
                   method = "knn",
                   trControl = train_control,
                   tuneGrid = expand.grid(k = 1:10))

# Accuracy was used to select the optimal model using the largest value.
# Fitting k = 8 on full training set

# Examine the kNN model
knn_model

# Make predictions on the treated validation set
predicted_treatedVal <- predict(knn_model, treatedValidation)

# Create a confusion matrix
confusion_matrix <- table(predicted_treatedVal, treatedValidation$Y_AcceptedOffer)

# Print the confusion matrix
print(confusion_matrix)

# predicted_treatedVal   1   2
# Accepted      97  68
# DidNotAccept 132 303

# Calculate the accuracy or performance of the knn model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy on treatedValidation test set:", round(accuracy, 3), "\n")
# Accuracy on treatedValidation test set: 0.667

# Plot ROC curve
predicted_prob <- predict(knn_model, treatedValidation, type = "prob")
roc_obj_knn <- roc(treatedValidation$Y_AcceptedOffer, predicted_prob[, 2])
plot(roc_obj_knn, main = "ROC Curve of k-NN Model", col = "red")
abline(a = 0, b = 1, lty = 2, col = "gray")
AUC_knn <- auc(roc_obj_knn)
cat("AUC:", round(AUC_knn, 4), "\n")
# AUC: 0.6885 


##################################  PLOT ALL ROC ON ONE PLOT  ##########################

# plot the first ROC curve
plot(roc_obj_logit, main = "ROC Curves Parsimonious", col = "gray")

# Plot all the other ROC
plot(roc_obj_steplogit, add = TRUE, col = "blue")
plot(roc_obj_treeModel, add = TRUE, col = "green")
plot(roc_obj_treeprune, add = TRUE, col = "purple")
plot(roc_obj_treetrain, add = TRUE, col = "orange")
plot(roc_obj_randomforest_train, add = TRUE, col = "red")
plot(roc_obj_knn, add = TRUE, col = "brown")

# Add Legend
legend("bottomright", legend = c("Logistic Regression", "Stepwise Logistic Regression",
                                 "Tree Model", "Pruned Tree Model", "Tree Train Model",
                                 "Random Forest Train Model", "K-Nearest Neighbors"),
       col = c("gray", "blue", "green", "purple", "orange",  "red", "brown"), lty = 1)

#############################################################################################
#############################################################################################
################################## RANDOM FOREST - TRAIN - FINAL SELECTED   #################
#############################################################################################
#############################################################################################


# Make predictions on the treated treated prospective customers (treatedtestData) set
predicted_treatedtest <- predict(randomForestModel_train, treatedTestData)


# Make probability predictions on Treated Test Data
predicted_prob <- predict(randomForestModel_train, treatedTestData, type = "prob")

# Convert the probability into a data frame
prob_df <- data.frame(ID = treatedTestData$HHuniqueID, Probability = predicted_prob[, 2])

# Reverse the order of the probability in decreasing value
prob_df <- prob_df[order(-prob_df$Probability), ]

# Print the probabilities
print(prob_df)


# Extract the top 100 rows showing the top 100 customers to reach out to
top_100 <- head(prob_df, n = 100)

# Print the to 100 to show the ID and Probability
print(top_100[, c("ID", "Probability")])

# Save into a csv file
write.csv(top_100[, c("ID", "Probability")], "top_100.csv", row.names = FALSE)



#############################################################################################
#############################################################################################
################################     ADDITIONAL PLOTS  ######################################
#############################################################################################
#############################################################################################

colnames(allData)

#Plot number of customers per acceptance or not acceptance based on number of contacts
ggplot(data = allData, aes(x = NoOfContacts, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. NoOfContacts", 
       x = "Number of Contacts", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot number of customers per acceptance or not acceptance based on Age
ggplot(data = allData, aes(x = Age, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. Age", 
       x = "Age", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot number of customers per acceptance or not acceptance based on Marital Status
ggplot(data = allData, aes(x = Marital, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. Marital", 
       x = "Marital", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot number of customers per acceptance or not acceptance based on HHInsurance
ggplot(data = allData, aes(x = HHInsurance, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. HHInsurance", 
       x = "HHInsurance", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot number of customers per acceptance or not acceptance based on LastContactMonth
ggplot(data = allData, aes(x = LastContactMonth, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. LastContactMonth", 
       x = "LastContactMonth", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot number of customers per acceptance or not acceptance based on PrevAttempts
ggplot(data = allData[allData$PrevAttempts <= 5,], aes(x = PrevAttempts, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. PrevAttempts", 
       x = "PrevAttempts", y = "Number of Customers") +
  theme_minimal()


#Plot number of customers per acceptance or not acceptance based on AgeBin

# Age bin
age_labels <- c("17-25", "26-39", "40-60", ">= 60", "53-62")

ggplot(data = allData, aes(x = factor(AgeBin), fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"),
                    name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. AgeBin", 
       x = "AgeBin", y = "Number of Customers") +
  theme_minimal() +
  scale_x_discrete(labels = age_labels)


#Plot number of customers per acceptance or not acceptance based on Job
ggplot(data = allData, aes(x = Job, fill = Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Accepted Offer") +
  labs(title = "Number of Customer Acceptance of Offer vs. Job", 
       x = "Job", y = "Number of Customers") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#boxplot to see the No of Contacts
ggplot(allData, aes(x = factor(Y_AcceptedOffer), y = NoOfContacts)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of No Of Contacts made with Y_AcceptedOffer",
       x = "Y_AcceptedOffer", y = "NoOfContacts")


#############################################################################################
############################################################################################
#######################     END  END  END END  END END #####################################
############################################################################################
#############################################################################################
