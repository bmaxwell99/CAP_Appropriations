library(dplyr)
library(magrittr)
library(tidyr)

individual_cases <- 
  read.csv('Individual Cases CAP.csv', stringsAsFactors = FALSE)

i_r <- 
  read.csv('I&R CAP.csv', stringsAsFactors = FALSE)

systemic <-
  read.csv('Systemic Data CAP.csv', stringsAsFactors = FALSE)

demogrpahic <-
  read.csv('demographic data CAP.csv', stringsAsFactors = FALSE)


