library(readxl)
data=read_excel('apps100k.xlsx')

# Remove frivilous values
library(dplyr)

data = data %>%
  filter(ssn!='737610282') %>%
  filter(dob!='19070626') %>%
  filter(homephone!='9105580920') %>%
  filter(address!='2602 AJTJ AVE')

# New variable "DAY" - day of the year
library(lubridate)

data$date = ymd(data$date)

data$DAY = yday(data$date)

# First name & last name & DOB "Person"
data$PERSON = paste(data$firstname,data$lastname,data$dob)

length(unique(data$PERSON))

# Address & zip (Address)
data$ADD_ZIP = paste(data$address,data$zip5)


### Function for building variables

##############################################################
variable_function <- function(df,variable,left,right){
  window_time = 3
  k = window_time+1
  TEMP = NULL
  j = 1
  
  for (k in ((window_time+1):(365-window_time))){
    
    subset = df %>%
      filter(df[,10] %in% (k:(k+window_time-1)))
    
    min_subset = min(subset$record)
    max_subset = max(subset$record)
    
    TEMP = NULL
    curr_record = max_subset+j
    
    while(df[curr_record, 10] == k+window_time && curr_record<=100000){
      print(curr_record)
      count = 1
      TEMP = df[curr_record,left]
      x = min_subset
      for(x in (min_subset:(curr_record-1))){
        #  print(x)
        if(df[curr_record,right] == df[x,right]){
          count = count + 1
          print(df[x,left])
          TEMP[count] = as.character(df[x,left])
        }
      }
      #  print(TEMP)
      df[curr_record,variable]=length(unique(TEMP))
      curr_record = curr_record + 1
    }  
  }
  return(df[,variable])
}
#############################################################

### ssn per PERSON
data$PERSON_SSN = 1
data$PERSON_SSN = variable_function(data, 13, 11, 3)

person_ssn = data %>%
  group_by(PERSON_SSN) %>%
  summarize(count = n()) 


### homehone per PERSON
data$PERSON_PHONE = 1
data$PERSON_PHONE = variable_function(data, 14, 11, 9)

person_phone = data %>%
  group_by(PERSON_PHONE) %>%
  summarize(count = n()) 


### ADD_ZIP per PERSON
data$PERSON_ADD = 1
data$PERSON_ADD = variable_function(data, 15, 11, 12)

person_add = data %>%
  group_by(PERSON_ADD) %>%
  summarize(count = n()) 
#### should we change ADD_ZIP to address? 


### Number of PERSON per ssn
data$SSN_PERSON = 1
data$SSN_PERSON = variable_function(data, 16, 3, 11)

ssn_person = data %>%
  group_by(SSN_PERSON) %>%
  summarize(count = n()) 
### This??? 

### Phone per ssn
data$SSN_PHONE = 1
data$SSN_PHONE = variable_function(data,17,3,9)

ssn_phone = data %>%
  group_by(SSN_PHONE) %>%
  summarize(count = n()) 


### ADD_ZIP per ssn
data$SSN_ADD = 1
data$SSN_ADD = variable_function(data,18,3,12)

ssn_add = data %>%
  group_by(SSN_ADD) %>%
  summarize(count = n()) 
### ??

### PERSON per homephone
data$PHONE_PERSON = 1
data$PHONE_PERSON = variable_function(data,19,9,11)

phone_person = data %>%
  group_by(PHONE_PERSON) %>%
  summarize(count = n()) 


### ssn per homephone
data$PHONE_SSN = 1
data$PHONE_SSN = variable_function(data,20,9,3)

phone_ssn = data %>%
  group_by(PHONE_SSN) %>%
  summarize(count = n()) 


### ADD_ZIP per homephone
data$PHONE_ADD = 1
data$PHONE_ADD = variable_function(data,21,9,12)

phone_add = data %>%
  group_by(PHONE_ADD) %>%
  summarize(count = n()) 


### PERSON per ADD_ZIP
data$ADD_PERSON = 1
data$ADD_PERSON = variable_function(data,22,12,11)

add_person = data %>%
  group_by(ADD_PERSON) %>%
  summarize(count = n()) 


### ssn per ADD_ZIP
data$ADD_SSN = 1
data$ADD_SSN = variable_function(data,23,12,3)

add_ssn = data %>%
  group_by(ADD_SSN) %>%
  summarize(count = n()) 


### homephone per ADD_ZIP
data$ADD_PHONE = 1
data$ADD_PHONE = variable_function(data,24,12,9)

add_phone = data %>%
  group_by(ADD_PHONE) %>%
  summarize(count = n()) 


####### Saving data
save(data,file="nf_expert_variables.Rda")





