#Install Packages
install.packages("dpylr","magrittr","tidyr","ggplot2","stringr")

#Load Packages
library("dplyr")
library("tidyr")
library("magrittr")
library("ggplot2")
library("stringr")
library("RCurl")


#Grabs csv file from URL and saves as a string
URlPermits<- getURL('https://s3.amazonaws.com/cc-analytics-datasets/Building_Permits.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

#Loads data into a dataframe
Permit_Data <- read.csv(textConnection(URlPermits), header=T)

#Part 1
#Count Number of Rows and Columns
Num_of_Rows <- nrow(Permit_Data)
Num_of_Columns <- ncol(Permit_Data)

#List of all Types of Construction and count for each
Types_of_Construction <- Permit_Data %>% group_by(const_type) %>% summarise(n = n())

#
# Mean and Median of Number of Stories
mean_of_stories <- mean(Permit_Data$numberstories,na.rm = TRUE)
median_of_stories <- median(Permit_Data$numberstories, na.rm = TRUE)


#Standard Deviation of X and Y Coordinates of the permits
sd_long_perm <- sd(Permit_Data$longitude_perm,na.rm= TRUE)
sd_lat_perm <- sd(Permit_Data$latitude_perm, na.rm = TRUE)



# Part 2
#We can infer that the estimated cost is around 20,000,000-30,000,000 for most projects across the months.

#Graph Issue Date Month vs. Estimated Project Cost
plot(Permit_Data$issueddate_mth,Permit_Data$estprojectcost, main ="Project Cost by Month",
     xlab="Issue Month",ylab="Estimated Cost", pch =20, cex=.7, col= "blue")




#Part 3
#We can infer that the project cost over the yeat has steadily increased. There were dips around 2001-2004 but higher cost in 2013-2014.


#Filtered data frame where Workclass is New, the number stories is less than 3, and the construction type is V  B.
Permit_Data_for_ExecTeam <- Permit_Data %>% select(workclass,const_type,numberstories,issueddate_yr,estprojectcost)%>%filter(str_detect(workclass, "New"))
Permit_Data_ExecTeam_Stories<- Permit_Data_for_ExecTeam %>%filter(Permit_Data_for_ExecTeam$numberstories<3)
Final_Permit_Data_Exec <- Permit_Data_ExecTeam_Stories %>% filter(Permit_Data_ExecTeam_Stories $const_type == 'V  B')


#Graph Issue Date Year vs. Estimated Project Cost
plot(Permit_Data_for_ExecTeam$issueddate_yr,Permit_Data_for_ExecTeam$estprojectcost, main ="Project Cost by Year",
     xlab="Issue Year",ylab="Estimated Cost", pch =20, cex=.7, col= "blue")



#Case for removing NA and blank cells as this is show what is missing and how it affects the overall dataset. Some of the missing fields may be for a reason rather than it just being "missing"
#Removes rows containing NA and blanks
Data_without_NA<- Permit_Data_for_ExecTeam %>% filter(complete.cases(.))
Data_without_NA_1<- na.omit(Data_without_NA)

#Graph Issue Date Year vs. Estimated Project Cost with no NA or blanks data
plot(Data_without_NA$issueddate_yr,Data_without_NA$estprojectcost, main ="Project Cost by Year1",
     xlab="Issue Year",ylab="Estimated Cost", pch =20, cex=.7, col= "blue")


