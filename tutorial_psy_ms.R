### Some tidy verse actions

library(tidyverse)

## read in dataset with readr -----------
#werewolf
werewolf <- read_csv("https://raw.githubusercontent.com/RMHogervorst/werewolf/master/data/werepeople.csv")

# pipes make it easy to change the action (summary, count, str, etc)  ####
werewolf %>% summary()


# combining ggplot2 and dplyr
## plotting the personality measures. #######
ggplot(werewolf)


# personality measures are locked in several columns.
werewolf %>% 
  gather(key = "scale", value = "value", BFI_O,BFI_C, BFI_E, BFI_A, BFI_N) %>% 
  select(scale, value)


## gather and display  personality measures #####
werewolf %>% 
  gather(key = "scale", value = "value", BFI_O,BFI_C, BFI_E, BFI_A, BFI_N) %>% 
  ggplot()+
  #geom_density(aes(value, fill = scale), alpha = 0.2)+ 
  geom_density(aes(value, color = scale))+
  facet_grid(sex~.) # small multiples


### working with psychology dataset
# connecting to database.
database <- src_sqlite("data/psy_staff.sqlite3")
# see all tables (there are only 2, one database management, and one we really use.)
database$con %>% db_list_tables()
# create pointer to table in database
psychologists <- tbl(database, "all_psy")
# work with this pointer as if normal data frame
psychologists
psychologists %>% View()
psychologists %>% count(department)

## count the number of drs in each department
psychologists %>% 
    collect() %>% 
    mutate(dr = ifelse(grepl("Dr.", official_name), TRUE, FALSE)) %>% 
    group_by(department) %>% 
    summarise(N_dr = sum(dr), 
            N = n())

## try to find all the 
count(psychologists, title) %>% 
  arrange(desc(n)) %>% 
  View()

## create function that takes title and parses it to one of fixed categories
## start with most likely work back.
job_parser <- function(vector){
  result <- ifelse(grepl("Assist[a|e]nt Professor",vector), "Assistant Professor", 
                   ifelse(grepl("[C|c]andidate", vector), "PhD Candidate", 
                          ifelse(grepl("Researcher", vector), "Researcher", 
                                 ifelse(grepl("Lecturer", vector), "Lecturer", 
                                        ifelse(grepl("Associate Professor",vector), "Associate Professor", 
                                               ifelse(grepl("Professor", vector, ignore.case = TRUE), "Professor", vector))))))
  result
}

# continueing the example
fixed_psy <- psychologists %>%
  collect() %>% 
  mutate(dr = ifelse(grepl("Dr.", official_name), TRUE, FALSE),
         newscount = as.numeric(gsub("[^0-9]", "", news)),
         job = job_parser(title)) 

fixed_psy %>% 
  ggplot(aes(x = job, y = newscount))+
  geom_bar(stat = "identity")+
  coord_flip()

fixed_psy %>% 
  group_by(job) %>% 
  summarize(total_N = n(),
            news = sum(newscount),
            perc_news = news/total_N) %>% 
  arrange(desc(perc_news))

# so it seems the professors and assistant professors and jobless 
# people have highest percentage
# take 
selection <- c("Professor", NA, "Assistant Professor", "Researcher")
fixed_psy %>% 
  filter(job %in% selection) %>% 
  group_by(job,department ) %>% 
  count()
# could be more helpful if transposed

fixed_psy %>% 
  filter(job %in% selection) %>% 
  group_by(job,department ) %>% 
  count() %>% 
  spread(key = department, value = n)

fixed_psy %>% 
  filter(job %in% selection) %>% 
  group_by(job,department ) %>% 
  count() %>% 
  ggplot()+
  geom_bar(aes( job,n), stat = "identity")+
  facet_wrap(~department)+
  coord_flip()
