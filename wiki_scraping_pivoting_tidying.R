
#To scrape the data we use the `rvest` package
library(rvest)

#Scraping the data from the provided source 
content<- read_html("https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/United_States_medical_cases")

#List of html tables in the Wikipedia site
tables<-content %>% html_table(fill=TRUE)

#Pulling the table that we need from the tables list
first_table<-tables[[3]]

#Names of columns in the table we need
old_names<-names(first_table)

#Creating a new vector to include the new column names
new_names<-c("Date")

#Adding the new column names to the vector
#Tidying the columns names, each column includes the region and state 
update_name<-first_table[1,]
for (i in 2:length(old_names)) {
  new_name<-paste(update_name[i], old_names[i], sep="_")
  new_names<-c(new_names,new_name)
}

#Updating the new created names in the table
names(first_table)<-new_names

#Cleaning the data frame, removing all "Date" duplicates 
scraped_data<-first_table %>% filter(Date!='Date')

#Removing unnecessary data
scraped_data<-scraped_data %>% filter(row_number()<=n()-4)

#Removing the duplicate `Date` column 
scraped_data<- scraped_data[, ! names(scraped_data)=="Date_Date", drop=F]

#All the scraped data was of type `character`, here we convert it to the right type for it to be used in the analysis
#the date was converted from `character` to date
#and the other numeric values, were converted from `character` to numeric
date<-scraped_data$Date
scraped_data<-scraped_data %>%mutate_all(funs(gsub(",", "", .)))
scraped_data<- scraped_data %>% mutate_all(funs(type.convert(as.numeric(.))))
scraped_data$Date<-date
scraped_data<-scraped_data%>%mutate(Date=Date%>%dmy())

#We replaced the null values with 0, which means there were no covid cases
scraped_data[is.na(scraped_data)]<-0


#getting to know the final result 
glimpse(scraped_data)


#Pivoting
#We split the scraped_data df into to two dataframes, one is to_be_pivoted df
#which we will use to pivot longer it's content
to_be_pivoted=subset(scraped_data,select=-c(Daily_Confirmed,Total_Confirmed,Daily_Deaths,Total_Deaths,Daily_Recovered,Total_Recovered,Total_Active))

glimpse(to_be_pivoted)


#Pivoting
cases1<-to_be_pivoted%>%pivot_longer(cols = AK_West:VI_Territories,  names_to = "State_Region",  values_to = "Cases")

#result of pivoting
glimpse(cases1)


#Before this line code the state and region were in the same column
#Tidying the data, Here we made 2 different columns for each, one for the state, and another for the region
cases1<-cases1%>%
  separate(State_Region, c("State", "Region"), "_")

#In this df we noticed negative values in the cases, which does not make sense, after finding out why we decided to replace them with 0.
cases1$Cases[cases1$Cases <0] <- 0

#After manipulating and cleaning the data we save the two resulted dataframes.
#Saving the clean data 
#saveRDS(cases1, file = "cases1.rds")
#saveRDS(cases2, file = "cases2.rds")


