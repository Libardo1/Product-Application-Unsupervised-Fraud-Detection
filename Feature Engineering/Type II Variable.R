library(dplyr)
library(lubridate)

# read data file
data = read.csv("apps100k.csv")

############################ Type II variable ############################ 

# load("expert_variables.Rda")
data$date = ymd(data$date)

# New variable "DAY" - day of the year
data$DAY = yday(data$date)

# First name & last name & DOB "Person"
data$PERSON = paste(data$firstname,data$lastname,data$dob)

length(unique(data$PERSON))

# Address & zip (Address)
data$ADD_ZIP = paste(data$address,data$zip5)


### Function for building variables
## DOB 19070626
## Address 2602 AJIT AVE 68138
## SSN 737610282
## Homephone 9105580920

### number of time PERSON occurs in past 3 days
data$TIME_PERSON = 1
window_time = 7 #3
k = window_time+1
j = 1
curr_record = 1

for (k in ((window_time+1):(366-window_time))){
  
  subset = data %>%
    filter(data[,"DAY"] %in% (k:(k+window_time-1)))
  
  min_subset = min(subset$record)
  max_subset = max(subset$record)
  
  curr_record = max_subset+j
  
  while(data[curr_record, "DAY"] == k+window_time){
    print(curr_record)
    count = 1
    
    x = min_subset
    for(x in (min_subset:(curr_record-1))){
      
      if(data[curr_record,"PERSON"] == data[x,"PERSON"]){
        count = count + 1
      }
    }
    
    data[curr_record,"TIME_PERSON"]=count
    curr_record = curr_record + 1
  }  
}


### number of time SSN occurs in past 3 days
data$TIME_SSN = 1
window_time = 7
k = window_time+1
j = 1

for (k in ((window_time+1):(366-window_time))){

    subset = data %>%
    filter(data[,"DAY"] %in% (k:(k+window_time-1)))
  
  min_subset = min(subset$record)
  max_subset = max(subset$record)
  
  curr_record = max_subset+j
  
  while(data[curr_record, "DAY"] == k+window_time){
    print(curr_record)
    count = 1
    x = min_subset
    for(x in (min_subset:(curr_record-1))){

      if(data[curr_record,"ssn"] == data[x,"ssn"]){
        count = count + 1
      }
    }
    data[curr_record,"TIME_SSN"]=count
    curr_record = curr_record + 1
  }  
}

for(x in (1:100000)){
  if(data[x,"ssn"] == 737610282){
    data[x,"TIME_SSN"] == mean(data$TIME_SSN)
  }
}

### number of time PHONE occurs in past 3 days
data$TIME_PHONE = 1

window_time = 7
k = window_time+1
j = 1

for (k in ((window_time+1):(366-window_time))){
  
  subset = data %>%
    filter(data[,"DAY"] %in% (k:(k+window_time-1)))
  
  min_subset = min(subset$record)
  max_subset = max(subset$record)
  
  curr_record = max_subset+j
  
  while(data[curr_record, "DAY"] == k+window_time){
  count = 1
  x = min_subset
  for(x in (min_subset:(curr_record-1))){
    if(data[curr_record,"homephone"] == data[x,"homephone"]){
      count = count + 1
    }
  }

  data[curr_record,"TIME_PHONE"]=count
  curr_record = curr_record + 1
}
}

for(x in (1:100000)){
  if(data[x,"homephone"] == 9105580920){
    data[x,"TIME_PHONE"] == mean(data$TIME_PHONE)
  }
}

### number of time ADD_ZIP occurs in past 3 days
data$TIME_ADD_ZIP = 1
window_time = 7
k = window_time+1
j = 1

for (k in ((window_time+1):(366-window_time))){
  
  subset = data %>%
    filter(data[,"DAY"] %in% (k:(k+window_time-1)))
  
  min_subset = min(subset$record)
  max_subset = max(subset$record)

  curr_record = max_subset+j
  
  while(data[curr_record, "DAY"] == k+window_time){
  count = 1
  x = min_subset
  for(x in (min_subset:(curr_record-1))){
    if(data[curr_record,"ADD_ZIP"] == data[x,"ADD_ZIP"]){
      count = count + 1
    }
  }
  data[curr_record,"TIME_ADD_ZIP"]=count
  curr_record = curr_record + 1
  }  
}

for(x in (1:100000)){
  if(data[x,"ADD_ZIP"] == "2602 AJIT AVE 68138"){
    data[x,"TIME_ADD_ZIP"] == mean(data$TIME_ADD_ZIP)
  }
}

save(data,file="BuildWindow7Type2.Rda")
