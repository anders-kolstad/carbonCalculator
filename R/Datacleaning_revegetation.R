#Packages ####
library("readxl")
library(data.table)
library(tidyr)
library(tibble)
library(dplyr)
library(tidyverse)
library(janitor)
library(magrittr)

#Importing data for 2020 and 2021 ####
VSM_ruter <- read_excel("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/Reveg_2021.xlsx",sheet = "VSM_ruter", na = "0")
#Added extra line above date, but date is still converted to unreadable numbers.

VSM_ruter_s <- read_excel("C:/Users/martef/DokumenterIntern/GitHub/PhDGRAN/Data/Reveg_2021.xlsx",sheet = "VSM_ruter_snudd", na = "0")


#Create wide format and clean up data types ####
VSM_ruter_t <-t(VSM_ruter) #creates wide format
VSM_ruter_t <- as.data.table(VSM_ruter_t) #set as data.table
VSM_ruter_t2 <- row_to_names(VSM_ruter_t,1) #set 1st row as headers

VSM <- as.data.frame(VSM_ruter_t2)

sapply(VSM, class) #checking the classes, all character now
VSM[is.na(VSM)] = 0 #entering 0s for all missing values
VSM[VSM == "<1"] <- "0.1" #replacing the <1 with 0.1
VSM[,6:33] <- sapply(VSM[,6:33],as.numeric) #works
VSM[,2:5] <- lapply(VSM[,2:5],as.factor) 
#why will this one run with lapply and the other with sapply?

sapply(VSM, class) #all are transformed

# Fixing the date issue ####
VSM[,1] <- sapply(VSM[,1],as.numeric)
VSM$Dato<-as.POSIXct(as.Date(VSM$Dato,
                   origin = "1899-12-30"), 
           "%Y-%m-%d ")
#get the correct date, but it also includes time (here 02:00)
#deal with that later
VSM_2 <-VSM

#Melt to long format ####
VSM_m1 = melt(VSM_2, id.vars = c("Dato", "Lokalitet", "Sublokalitet", "Blokk", "Behandling"),
             measure.vars = c(6:33))
#This runs with warnings of being outdated

VSM_m2 = pivot_longer(VSM_2, cols=6:33, names_to="variable", values_to="value", 
                      values_drop_na = FALSE )
#This runs smoothly, and is easier to get a grasp on than the melt-function
