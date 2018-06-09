if (!file.exists('pml-training.csv')) {
  download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv','pml-training.csv')
  download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv','pml-testing.csv')
}
require(tidyverse)
require(lubridate)

training <- read.csv('pml-training.csv',
                     na.strings = "#DIV/0!",
                     stringsAsFactors = FALSE) %>%
  as_tibble()


# test set lacks the target classe but has an additional column problem_id
testing <- read.csv('pml-testing.csv',
                    stringsAsFactors = TRUE) %>%
  as_tibble()

all_data <- rbind(training %>% select(-classe),testing)

training_div0<-read.csv('pml-training.csv',
                     stringsAsFactors = FALSE) %>%
  as_tibble() 

str(training[,sapply(training,class) == 'numeric'])
str(training[,sapply(training,class) == 'integer'])
str(training[,sapply(training,class) == 'character'])

factors <- c('classe','user_name','new_window')
timestamp <- 'cvtd_timestamp'
training[,-which(names(training) %in% c(factors,timestamp))] <- lapply(training[,-which(names(training) %in% c(factors,timestamp))],as.numeric)
training[,which(names(training) %in% c(factors))] <- lapply(training[,which(names(training) %in% factors)],factor)

div0 <- apply((training == '#DIV/0!'),2,any)
div0[is.na(div0)]<-FALSE
str(training[,div0])

#raw_timestamp_part_1 and cvtd_timestamp match up to the minute part
training %>%
  mutate(tdiff = as.POSIXct(raw_timestamp_part_1,origin="1970-01-01") - dmy_hm(cvtd_timestamp)) %>%
  select(tdiff) %>%
  arrange(desc(abs(tdiff))) %>%
  head()

#raw_timestamp_part_1 could be microsecond as it is always below 10e6
training %>%
  select(raw_timestamp_part_2) %>%
  range()

training %>%
  mutate(cvtd_timestamp = dmy_hm(cvtd_timestamp)) %>%
  select(user_name,cvtd_timestamp) %>%
  arrange(cvtd_timestamp) %>%
  unique()

# There is training data for each subject for 3 to 4 minutes
training %>%
  mutate(cvtd_timestamp = dmy_hm(cvtd_timestamp)) %>%
  select(user_name,cvtd_timestamp) %>%
  arrange(cvtd_timestamp) %>%
  unique()

# Window numbers are unique across users
training %>% group_by(num_window) %>% summarise(nws = n_distinct(user_name)) %>% arrange(desc(nws))


grep('yaw',names(training),value = TRUE) %>% strsplit(split = '_') %>% sapply(function(x) x[length(x)]) %>% unique()

grep('roll.*belt',names(training),value = TRUE)
grep('pitch',names(training),value = TRUE)

grep('picth',names(training),value = TRUE)

grep('gyros.*belt',names(training),value=TRUE)

original<-grep('^(pitch|yaw|roll|accel|gyros|total|magnet)',names(training),value=TRUE)

nas <- colMeans(is.na(training_div0))
names(nas[nas != 0])

#aggregate statistics are only contained in the new_window columns
training_div0 %>%
  filter(new_window=='yes') %>%
  is.na() %>%
  colMeans()

training %>%
  filter(new_window=='yes') %>%
  is.na() %>%
  colMeans() %>%
  {.[. != 0]}

training %>%
  filter(new_window == 'yes',stddev_yaw_belt!=0) %>%
  select(X,stddev_yaw_belt,kurtosis_yaw_belt,skewness_yaw_belt)

training_div0 %>%
  filter(X==52) %>%
  select(X,var_yaw_belt,stddev_yaw_belt,kurtosis_yaw_belt,skewness_yaw_belt)
  
#testing data has no aggregates
cs <- colSums(is.na(testing))
features <- names(cs[cs != 20])
exlist <- c('X','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp','new_window','num_window','problem_id')
features <- features[!features %in% exlist]
  




#only one class per window => leackage in num_window
training_div0 %>%
  group_by(num_window) %>%
  summarize(diff_cl = n_distinct(classe)) %>%
  arrange(desc(diff_cl)) %>%
  head()

#aggregate only in new_window tagged col?
training_div0 %>%
  select(new_window,num_window,yaw_arm,avg_yaw_arm) %>%
  filter(num_window == 2) %>%
  arrange(num_window,avg_yaw_arm) %>%
  mutate(myavg = mean(yaw_arm))

### leackage

# plotting leackage
scoreTimeDependence <- function(name) {
  training %>% 
    filter(user_name == name) %>% 
    select(raw_timestamp_part_1,classe) %>%
    {plot(x=.[['raw_timestamp_part_1']],y=factor(.[['classe']]))}
  
  testing %>%
    filter(user_name == name) %>% 
    select(raw_timestamp_part_1) %>%
    {abline(v=.[['raw_timestamp_part_1']],col='red')}
  
  title(main = name)
}

names <- training %>% select(user_name) %>% unique()
print(names)


par(mfrow=c(3,2))
for (n in names$user_name) {
  scoreTimeDependence(n)
}


# prediction based on leackage
exerciseProgression <- training %>%
  mutate(full_ts = (raw_timestamp_part_1 %% 1000) *1000000 + raw_timestamp_part_2) %>%
  group_by(user_name,classe) %>%
  summarise(start = min(full_ts),end = max(full_ts)) %>%
  ungroup()

predict <- function(name,ts) {
  exerciseProgression %>%
    filter(user_name == name,start<=ts,end >= ts) %>%
    select(classe) %>%
    {.$classe[1]}
}

predict('pedro',2868349)

preds <- testing %>%
  mutate(full_ts = (raw_timestamp_part_1 %% 1000) *1000000 + raw_timestamp_part_2) %>%
  select(problem_id,user_name,full_ts) %>%
  mutate(prediction = map2_chr(user_name,full_ts,predict))

