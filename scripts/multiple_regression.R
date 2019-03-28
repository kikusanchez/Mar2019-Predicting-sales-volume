#### 0. INCLUDES ####
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(rstudioapi, dplyr,magrittr, tidyr, reshape2, readxl, stringi,
               ggplot2)

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

#Data sets
existing<-read.csv("../multiple_regression/blackwells_multiple_regression/datasets/existingproductattributes2017.csv")
new<-read.csv("../multiple_regression/blackwells_multiple_regression/datasets/newproductattributes2017.csv")

