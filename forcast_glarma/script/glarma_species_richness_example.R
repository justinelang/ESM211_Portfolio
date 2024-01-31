#GLARMA example
#CL Jerde
################

library(glarma)
library(here) # makes a local directory for the R project
library(tidyverse)

#imports eBird species richness data for Pepperwood Preserve
SR<-read.csv(here("data","SR_df.csv"))

Y<-SR$S # makes the response variable only the species richness
X.1<-SR %>% select(intercept, dist) #makes model 1's explanatory variables of the intercept (1) 
#and presence absence of a fire event
X.1<-as.matrix(X.1) #makes the data frame into matrix


X.0<-SR %>% select(intercept) #makes a null model without fire information
X.0<-as.matrix(X.0) #makes the data frame into a matrix



# null model without fire but with a 1 time step lag
glarmamod.0 <- glarma(Y, X.0, phiLags = c(1), type = "Poi", method = "FS",
                      residuals = "Pearson", maxit = 100, grad = 1e-6)

# model with fire events included
glarmamod.1 <- glarma(Y, X.1, phiLags = c(1), type = "Poi", method = "FS",
                      residuals = "Pearson", maxit = 100, grad = 1e-6)

#summary statistics just like getting information out of linear regression
summary(glarmamod.0)
summary(glarmamod.1)



#### Modified SR plot for Pepperwood
SR_mod<-SR

SR_mod$est<-glarmamod.1$fitted.values #adds the fitted values of the Best fit GLARMA model to the data

#plots the model (black) and the species richness data (gray)
SR_plot_mod<-ggplot(SR_mod, aes(x=year)) + 
  geom_point(col="gray",aes(y=S),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Species Richness") +
  ylim(0,150) + xlim(2005,2023)+
  theme_bw()

#Adds two red horizontal lines for the occurance and duration of the fires
SR_plot_mod<-SR_plot_mod + annotate("rect", xmin = 2017.767, xmax =2017.833 , ymin = 0, ymax = 150, alpha = .75,fill = "red")+annotate("rect", xmin = 2019.808, xmax =2019.855 , ymin = 0, ymax = 150, alpha = .75,fill = "red")

ggsave(here("plots","Pepperwood_SR_Glarma.jpg"),SR_plot_mod, dpi=500, width=6, height=3, unit="in")
