
library(data.table)
library(readxl)


ucla_data <- read_excel('./data/data_collection.xlsx', 'User_1_Data_collection')
csulb_data <- read_excel('./data/data_collection.xlsx', 'User_2_Data_collection')
cuny_data <- read_excel('./data/data_collection.xlsx', 'User_3_Data_collection')
ru_data <- read_excel('./data/data_collection.xlsx', 'User_4_Data_collection')



#only include these columns
limited_columns <- c("university_name", "university_brand",
                     "university_state", "good_resume", "job_id", 
                     "staggered_application", "date_applied", 
                     "call_back", "days_to_respond")

alldata <- rbind(ucla_data[limited_columns],
                 csulb_data[limited_columns],
                 cuny_data[limited_columns],
                 ru_data[limited_columns])

#0 = rejection, 1 = interview, blank/NA = no response
alldata$call_back_factor <- ifelse(is.na(alldata$call_back), "None",
                                   ifelse(alldata$call_back=='0', 'Rejection',
                                          'Interview'))


#binarize call bck variable
alldata$call_back_binary <- ifelse(alldata$call_back_factor=='Interview'&
                                     alldata$days_to_respond<=18,
                                   1,0)

alldata$reject_binary <- ifelse(alldata$call_back_factor=='Rejection'&
                                  alldata$days_to_respond<=18,
                                1,0)

#states are CA/NY/NJ, condense into west/east coast
alldata$coast <- ifelse(alldata$university_state=='CA', 'West', 'East')


#count how many jobs had 2 valid applicatons
#deemed valid if the "date_applied" column has just a date or number on t
#and not a blank or comment
alldata$valid <- ifelse(!is.na(as.numeric(alldata$date_applied)), 1, 0)
valid_agg <- with(alldata, aggregate(valid, list(job_id=job_id), sum))
colnames(valid_agg) <- c('job_id', 'both_applications_valid')
#create binary variable - if a job_id has 2 valid applications, 1, otherwise 0
valid_agg$both_applications_valid <- ifelse(valid_agg$both_applications_valid == 2, 1, 0)

#merge into original data frame
alldata <- merge(alldata, valid_agg, by='job_id')

alldata$phase <- ifelse(alldata$job_id <=80, 'Phase1', 'Phase2')

#get company size from "job openings" sheet
company_info <- read_excel('./data/data_collection.xlsx', 'Job Openings Condensed')

#bin them as S/M/L for 1-49, 50-999, 1000+
company_info$size_bin <- ifelse(is.na(company_info$company_size_n), NA, ifelse(company_info$company_size_n<100, 'Small',
                                                                               ifelse(company_info$company_size_n<1000, 'Medium', 'Large')))

#merge into original data frame
alldata <- merge(alldata, company_info[c("job_id", "size_bin", "company_size_n", 
                                         "same_city", "same_state", "at_hq_city")])