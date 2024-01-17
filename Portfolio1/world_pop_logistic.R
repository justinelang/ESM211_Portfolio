# Logistic growth of Human Population & Bison
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
library(growthcurver) # older package, for logistic growth

#get the data
w_pop_data<-read_csv(here("Portfolio1","population-and-demography.csv"))

#clean the data for only the global human population
w_pop_data<- w_pop_data |> clean_names() |>
  filter(country_name=="World") |> 
  select(year, population) |> drop_na()

#transform year to time
w_pop_data <- w_pop_data |> mutate(time = year - year[1])

#using the growthcurver package
#here: https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html

human_fit<-SummarizeGrowth(w_pop_data$time,w_pop_data$population)
human_fit
plot(human_fit) #what is funny with this graph?  N at 0 is what?  
w_pop_data$population[1] # need to add this to the y population values.  
est_K_human<-human_fit$vals$k + w_pop_data$population[1] #estimated K for the data

#bison data
bison_data<-read_csv(here("Portfolio1","bison.csv"))

#transform year to time
bison_data <- bison_data |> mutate(time = year - year[1])

bison_fit<-SummarizeGrowth(bison_data$time,bison_data$bison)
bison_fit
plot(bison_fit)
est_K_bison<-bison_fit$vals$k + bison_data$bison[1] #estimated K for the data













