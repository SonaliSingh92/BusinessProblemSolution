###################################################################
##############--------Author : Sonali Singh---------###############
###################################################################

#set working directory
setwd("C:\Users\chaud\Desktop\CapitalOneDataChallenge")

#Libraries which are required
install.packages("jsonlite")
library(jsonlite)
install.packages("RJSONIO")
library(RJSONIO)
install.packages("rjson")
library(rjson)
install.packages('plotly')
library(plotly)
install.packages("plyr",dependencies = T)
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)


#Univariate Analysis on both the datesets(Institutional and loans)

#Analysis on Institutional Data between 2012 to 2014
`2012_to_2014_institutions_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_institutions_data.csv", na.strings = "")
institutions_data <- `2012_to_2014_institutions_data`
summary(institutions_data)

#Analysis on loans Data between 2012 to 2014
`2012_to_2014_loans_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_loans_data.csv", na.strings = "")
loans_data <- `2012_to_2014_loans_data`
summary(loans_data)

###################################################################
#############--------------Data Munging-------------###############
###################################################################

#a) Merging two datasets and creating new column having 4 different buckets(Low, Medium, High, Very high) of loan amount

#Merging the institutions_data and loans_data into merged_data by Respondent_ID,Agency_Code,As_of_Year
merged_data <- merge(institutions_data,loans_data,by=c("Respondent_ID","Agency_Code","As_of_Year"))

#Creating a new column "Loan Category" with four bucket sizes - Low, Medium, High, Very High
merged_data <- transform(merged_data,Loan_Category=cut(Loan_Amount_000,breaks=c(1,500,2000,20000,100000),labels=c('Low','Medium','High',"Very High")))
View(merged_data)
summary(merged_data)


#b) APIs of 2 functions [ hmda_init() and hmda_to_json(data, states, conventional_conforming) ]

#-------------------------------Function: hmda_init()----------------------------------------
# Reads the data files and return a pointer or object containing the expanded HMDA data
#-------------------------------------------------------------------------------------------

hmda_init=function(){
  
   #reading the institution data
  `2012_to_2014_institutions_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_institutions_data.csv", na.strings = "")
  institutions_data <- `2012_to_2014_institutions_data`

  #reading the loans data
  `2012_to_2014_loans_data` <- read.csv("C:/Users/chaud/Desktop/CapitalOneDataChallenge/2012_to_2014_loans_data.csv", na.strings = "")
  loans_data <- `2012_to_2014_loans_data`
  
  #Merging both the above datasets
   merged_data <-merge(institutions_data,loans_data,by=c("Respondent_ID","Agency_Code","As_of_Year"))
   
   #Creating a new column in merged dataset
   merged_data <- transform(merged_data,Loan_Category=cut(Loan_Amount_000,breaks=c(1,153,235,347,99625.0),labels=c('Q1','Q2','Q3','Q4')))
   
   # String to Numeric Conversion
   merged_data$As_of_Year <- as.numeric(merged_data$As_of_Year)
   merged_data$Sequence_Number <- as.numeric(merged_data$Sequence_Number)
   
   merged_data$Loan_Amount_000 <- as.numeric(merged_data$Loan_Amount_000)
   merged_data$Applicant_Income_000 <-
     as.numeric(merged_data$Applicant_Income_000)
   merged_data$FFIEC_Median_Family_Income <-
     as.numeric(merged_data$FFIEC_Median_Family_Income)
   merged_data$Tract_to_MSA_MD_Income_Pct <-
     as.numeric(merged_data$Tract_to_MSA_MD_Income_Pct)
   merged_data$Number_of_Owner_Occupied_Units <-
     as.numeric(merged_data$Number_of_Owner_Occupied_Units)
   merged_data$Conforming_Limit_000 <-
     as.numeric(merged_data$Conforming_Limit_000)
   
   # String to Factor Conversion
   merged_data$Conventional_Status <-
     as.factor(merged_data$Conventional_Status)
   merged_data$Conforming_Status <-
     as.factor(merged_data$Conforming_Status)
   merged_data$Conventional_Conforming_Flag <-
     as.factor(merged_data$Conventional_Conforming_Flag)
   
   #Return the merged data frame
   return(merged_data)
}

#Calling the hmda_init() function and storing the result in a variable
readthedata = hmda_init()

# ----------------Function: hmda_to_json(data, states, conventional_conforming)---------------
# Export the expanded data set to disk for the states filtered by product segment.
#---------------------------------------------------------------------------------------------

hmda_to_json = function(data,states = levels(data$State),conventional_conforming= levels(data$Conventional_Conforming_Flag), year = levels(as.factor(institutions_data$As_of_Year)))
{
  loans_data = data[(data$State %in% states & data$Conventional_Conforming_Flag %in% conventional_conforming & data$As_of_Year %in% year),]
  x = toJSON(loans_data)
  write(x,"test.json")
}

#Calling the function hmda_to_json and storing the value in a variable
stored_data = hmda_to_json(readthedata,"VA","Y","2013")


###################################################################
#############--------------Data Quality-------------###############
###################################################################


############################################################################################################################################
#Q2

#Creating loan_quality_assessment to check the quality for the loan amount column
loan_quality_assessment = function(data = readthedata)
{
  #Checking for missing values
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(is.na(readthedata$Loan_Amount_000[i])== T)
    {
      print("Quality assessment failed ")
      print(paste(c("Missing value present at:",i),collapse = " "))
    }
    
  }
  
  #Checking for negative values
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(readthedata$Loan_Amount_000[i]<=0)
    {
      print("Quality assessment failed")
      print(paste(c("Negative value present at:",i),collapse = " "))
    }
  }
  
  #Checking for the data type of the column loan amount
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(class(readthedata$Loan_Amount_000[i])!="integer")
    {
      print("Quality assessment failed")
      print(paste(c("Unacceptable value present at:",i),collapse = " "))
    }
  }
  
  #Additional Column Check: Creating institutions_data new column ratio(loan amount/applicant income) and looking for possible data entry errors which may require further investigation
  readthedata$ratio = (readthedata$Loan_Amount_000/(as.numeric(readthedata$Applicant_Income_000)))
  for(i in 1:length(readthedata$Loan_Amount_000))
  {
    if(readthedata$ratio[i]>=10)
    {
      print((paste(c("Suspicious loan amount present at:",i),collapse = " ")))
      print("Further investigation required")
    }
  }
  
}


#Calling the function to check the variable loan amount and identifying the rows with abnormal values
loan_quality_assessment(readthedata)


#Creating respondent_quality_assessment function to assess the quality for the column respondent name
respondent_quality_assessment = function(data = readthedata)
{
  
  #Check for missing values
  for(i in 1:length(readthedata$Respondent_Name_TS))
  {
    if(is.na(readthedata$Respondent_Name_TS[i])== T)
    {
      print("Quality assessment failed ")
      print(paste(c("Missing value present at:",i),collapse = " "))
    }
  }
  
  
  special_ch = "[~!@#$%^*]"
  
  #Check for special characters
  for(i in 1:length(readthedata$Respondent_Name_TS))
  {
    ch = as.character(readthedata$Respondent_Name_TS[i]) 
    if((grepl(special_ch, ch))==TRUE)
    {
      print("Quality assessment failed ")
      print(ch)
      print(paste(c("Special character present at:",i),collapse = " "))
    }
    
  }
  
  
  #Check for the Respondent name 
  len1 = length(unique(readthedata$Respondent_Name_TS))
  len2 = length(unique(tolower((readthedata$Respondent_Name_TS))))
  if(len1-len2>0)
  {
    print("Data discrepancy in the  Repondent Name")
  }
  
}

#Additional COlumn Check: Check for the flags conforming limit 
checkflag = (readthedata$Loan_Amount_000>readthedata$Conforming_Limit_000 & readthedata$Conforming_Status != "Jumbo")
updateflag = nrow(readthedata[checkflag,])
updateflag


#Calling the function  to check the variable respondent name and identifying the rows with abnormal values
respondent_quality_assessment(readthedata)

###################################################################
#############--------------Data Visualization-------------#########
###################################################################

#Q3

#map to visualise how much total amount has been funded for all the states for all the years

TotalAmountFunded = readthedata %>%group_by(as.character(State)) %>%dplyr::summarise(TotalFundedAmount = sum(Loan_Amount_000, na.rm=TRUE))
colnames(TotalAmountFunded)[1] = "State"

# give state boundaries a red border
checkflag <- list(color = toRGB("red"), width = 2)
# specify some map projection/options
geography <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('red')
)

plot_geo(TotalAmountFunded, locationmode = 'USA-states') %>%
  add_trace(
    z = ~TotalFundedAmount, text = ~State, locations = ~State,
    color = ~TotalFundedAmount, colors = 'Blues', marker = list(line = checkflag)
  ) %>%
  colorbar(title = "In USD") %>%
  layout(
    title = 'Total Amount funded (in Thousands) <br>(Hover for details)',
    geo = geography
  )



###########################################################################################################

#map to visualise how much total amount has been funded for all the states for different years
FA1 = readthedata %>%
  group_by(As_of_Year,as.character(State)) %>%
  dplyr::summarise(TotalFundedAmount = sum(Loan_Amount_000, na.rm=TRUE))

colnames(FA1)[2] = "State"


geography <- list(
  scope = 'usa',
  showland = T,
  landcolor = toRGB("gray90"),
  showcountries = F,
  subunitcolor = toRGB("white")
)

#Function to create the graph
one_map = function(FA1)
{
  plot_geo(FA1, locationmode = 'USA-states') %>%
    add_trace(
      z = ~TotalFundedAmount, text = ~State, locations = ~State,
      color = ~TotalFundedAmount, colors = 'Blues'
    ) %>%
    add_text(x = -78, y = 47, text = ~unique(As_of_Year), color = I("black")) %>%
    layout(
      geo = geography,
      showlegend = FALSE
    )
}

FA1 %>%
  group_by(As_of_Year) %>%
  do(map = one_map(.)) %>%
  subplot(nrows = 3) %>%
  layout(
    showlegend = FALSE,
    title = 'Total amount funded for different States<br>hover over every state',
    width = 1000,
    height = 800,
    hovermode = TRUE
  )

#######################################################################################################################

#map for the mean applicants income for different States for different Years
FA2 = readthedata %>%
  group_by(As_of_Year,as.character(State)) %>%
  dplyr::summarise(TotalFundedAmount = mean(as.numeric(Applicant_Income_000), na.rm=TRUE))

colnames(FA2)[2] = "State"

geography <- list(
  scope = 'usa',
  showland = T,
  landcolor = toRGB("gray90"),
  showcountries = F,
  subunitcolor = toRGB("white")
)

#Function to create a map
one_map = function(FA2)
{
  plot_geo(FA2, locationmode = 'USA-states') %>%
    add_trace(
      z = ~TotalFundedAmount, text = ~State, locations = ~State,
      color = ~TotalFundedAmount, colors = 'Blues'
    ) %>%
    add_text(x = -78, y = 47, text = ~unique(As_of_Year), color = I("black")) %>%
    layout(
      geo = geography,
      showlegend = FALSE
    )
}

FA2 %>%
  group_by(As_of_Year) %>%
  do(map = one_map(.)) %>%
  subplot(nrows = 3) %>%
  layout(
    showlegend = FALSE,
    title = 'Mean applicants income for different States for different Years',
    width = 1000,
    height = 800,
    hovermode = TRUE
  )


###########################################################################################################################
#Ggplot for amount funded vs  years and states 
ts_amnt_year = ggplot(FA1,aes(x = As_of_Year , y = TotalFundedAmount,fill= State))
ts_amnt_year + geom_bar(stat = "identity") + xlab("Year issued")

###############################################################################################################################
#GGplot for amount funded vs  states and conventional_conforming states
FA3 = readthedata %>%
  group_by(Conventional_Conforming_Flag,as.character(State)) %>%
  dplyr::summarise(TotalFundedAmount = sum(as.numeric(Loan_Amount_000), na.rm=TRUE))


colnames(FA3)[2] = "State"

ts_amnt_year1 = ggplot(data = FA3, aes(x=State,y= TotalFundedAmount, fill=Conventional_Conforming_Flag))
ts_amnt_year1+geom_bar(stat = "identity")
##################################################################################################################################
#Ggplot for amount funded vs states and Loan purpose description
FA4 = readthedata %>%
  group_by(Loan_Purpose_Description,as.character(State)) %>%
  dplyr::summarise(TotalFundedAmount = sum(as.numeric(Loan_Amount_000), na.rm=TRUE))

colnames(FA4)[2] = "State"


ts_amnt_year2 = ggplot(data = FA4, aes(x=State,y= TotalFundedAmount, fill=Loan_Purpose_Description))
ts_amnt_year2+geom_bar(stat = "identity")

####################################################################################################################################3

#Ggplot for number of appicants vs  years and states 

FA5 = readthedata %>%group_by(As_of_Year,as.character(State)) %>%
  dplyr::summarise(NoofApplcants = length(Applicant_Income_000))

colnames(FA5)[2] = "State"

ts_amnt_year3 = ggplot(data = FA5, aes(x=As_of_Year,y= NoofApplcants , fill=State))
ts_amnt_year3+geom_bar(stat = "identity")+ xlab("year issued")+ylab("Number of Applicants")
