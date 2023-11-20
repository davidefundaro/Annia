#install.packages("openxlsx", repos = "https://cloud.r-project.org")
#install.packages("SmartEDA", repos = "https://cloud.r-project.org")



library(tidyr)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(str2str)
library(conflicted)
library(rlang)
library(openxlsx)
library(ggplot2)
library(SmartEDA)

conflict_prefer('select','dplyr','MASS')
conflict_prefer('filter','dplyr','MASS')
conflict_prefer('filter','dplyr','stats')
conflicts_prefer(dplyr::filter)