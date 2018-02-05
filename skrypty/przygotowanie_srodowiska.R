#### PRZYGOTOWANIE SRODOWISKA ####

# usuniecie wszystkiego
rm(list=ls())

# ustawienie sciezki do katalogu:
setwd("C:/Users/Kuba/Documents/magisterka")

# funkcja do pakietow:
is.installed <- function(pakiet){
   if (pakiet %in% installed.packages()[,1] == F){
      install.packages(pakiet)
   } else{
      cat(paste("Pakiet ", pakiet, " jest zainstalowany", sep = ""), "\n")
   }
}

# potrzebne pakiety
lista_pakietow <- list(
   "dplyr",
   "caret",
   "plyr",
   "data.table",
   "ggplot2",
   "glmnet",
   "ggthemes",
   "jpeg",
   "leaps",
   "Matrix",
   "MatrixModels",
   "ModelMetrics",
   "moments",
   "PCAmixdata",
   "purrr",
   "reshape2",
   "stringr",
   "tidyr",
   "cluster",
   "lattice",
   "MASS",
   "parallel",
   "rpart",
   "splines",
   "nnet",
   "randomForest",
   "DescTools",
   "Rcpp",
   "forcats",
   "wrapr",
   "lubridate",
   "stringr",
   "stringi",
   "rebus",
   "readxl",
   "tidytext",
   "wordcloud",
   "scales",
   "knitr",
   "markdown",
   "rmarkdown",
   "unbalanced",
   "foreach",
   "MLmetrics",
   "pROC",
   "PRROC",
   "gridExtra",
   "ipred",
   "kernlab"
)

# spr. czy sa pakiety
lapply(X = lista_pakietow, FUN = is.installed)

# ladowanie najczesciej potrzebnych:
library(dplyr)
library(caret)
library(ggplot2)

# ustawienie formatu czasowego
Sys.setlocale("LC_TIME", "C")