# Exponential growth of Human Population
# Example
# Christopher L Jerde
# Notes:Data from https://ourworldindata.org/population-growth#introduction
########################################

#Clear the R environment
rm(list = ls())

#Libraries needed
library(janitor) #cleans data and names
library(here) #allows for localized file directory
library(tidyverse) #makes R work nicely

#get the data
w_pop_data<-read_csv(here("Portfolio1","population-and-demography.csv"))

#clean the data for only the global human population
w_pop_data<- w_pop_data |> clean_names() |>
  filter(country_name=="Denmark") |> 
  select(year, population) |> drop_na()

#Always plot your data
####################################

#On the observed scale
human_ts<-ggplot(w_pop_data, aes(x=year, y=population))+
  geom_point()+
  xlab("Year")+
  ylab("Human count (N)")+
  xlim(1949, 2023)+
  ggtitle("Human Population in Denmark (1950-2022)")+
  theme_bw()
human_ts

#on the transformed log scale
human_ts_log<-ggplot(w_pop_data, aes(x=year, y=population))+
  geom_point()+
  xlab("Year")+
  ylab("Human count (N)")+
  scale_y_continuous(trans="log")+ # log scale for linear transformation
  xlim(1949, 2023)+
  ggtitle("Human Population in Denmark (1950-2022)")+
  theme_bw()
human_ts_log


#use lm() to find estimates
##########################################
human.lm_fit<- lm(log(population)~year,data=w_pop_data)
summary(human.lm_fit) #how well does it fit? 

NO<-exp(human.lm_fit$coefficients[1])
r<-human.lm_fit$coefficients[2]

#Plot the data and the model
###########################################

# On the transformed scale
human_ts_model_trans<-ggplot(w_pop_data, aes(x=year, y=population))+
  geom_point()+
  xlab("Year")+
  ylab("Human count (N)")+
  scale_y_continuous(trans="log")+
  geom_smooth(method="lm",color="blue")+
  xlim(1949, 2023)+
  ggtitle("Human Population in Denmark (1950-2022)")+
  theme_bw()
human_ts_model_trans


# On the observed scale

# Predicted population values from the model
predicted_df <- data.frame(pop_pred = predict(human.lm_fit, w_pop_data), year=w_pop_data$year)
predicted_df<- predicted_df |> mutate(N_est=exp(pop_pred)) #transformed back to observed popualtion estimates


human_ts_model<-ggplot(w_pop_data, aes(x=year, y=population))+
  geom_point(color="black")+
  geom_line(color='red',data = predicted_df, aes(x=year, y=N_est))+
  xlab("Year")+
  ylab("Human count (N)")+
  xlim(1949, 2023)+
  ggtitle("Human Population in Denmark (1950-2022)")+
  theme_bw()
human_ts_model



