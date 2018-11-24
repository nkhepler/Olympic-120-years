library(skimr)
library(ggplot2)
library(dplyr)

df <- read.csv('Olympic-120-years/data/raw_data/120-years-of-olympic-history-athletes-and-results/athlete_events.csv')

## getting overview of data
skim(df)

## looking at distribution of ages
ggplot(data=df, aes(x=Age)) + 
  geom_histogram(color='black', fill='white')

## looking at ages of athletes by gender
ggplot(data=df, aes(x=Sex, y=Age)) + 
  geom_boxplot()

## calculating the proportion of althletes in a given year by sex
## group by Year and Sex, then determine the number of participants (N) for each Year, seperated by sex
## then calculate the frequency by dividing N by sum(N)
  ## N corresponds to individual N, so one for Male and one for Female
  ## sum(N) means the sum of both Ns, so Male and Female
share <- df %>%
  group_by(Year, Sex) %>%
  summarise(N=n()) %>% 
  mutate(freq = N / sum(N))

## limiting to just women
share_women <- share %>% 
  filter(Sex=='F')

ggplot(data=share_women, aes(x=Year, y=freq)) + 
  geom_line() + 
  scale_y_continuous(breaks=seq(0,1,by=0.1))

## are taller athletes more likely to win a metal?
## create new column 'had.medal'
  ## returns True if "Medal' column contains either Gold, Silver or Bronze
  ## returns False if not (basically if contains data = True, if not = False)
  ## what we see is that the number of False reponses == the number of missing in the Medal column (skim function)

df <- df %>% 
  mutate(has.medal = Medal %in% c('Gold', 'Silver', 'Bronze'))

## visualize data
table(df$has.medal)

ggplot(data=df, aes(x=has.medal, y=Height)) + 
  geom_boxplot()
