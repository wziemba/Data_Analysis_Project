library(tidyverse)
library(tidymodels)
library(readxl)
library(dplyr)
library(tibble)
library(DataExplorer)

load("NSDUH_2024.RData")

ls()
df <- NSDUH_2024

dim(df)

head(df[, 1:10])
names(df)

View(df)

var_labels <- sapply(df, function(x) attr(x, "label"))

dictionary <- data.frame(
  Variable = names(df),
  Description = ifelse(sapply(var_labels, is.null), "No label", unlist(var_labels)),
  row.names = NULL
)

View(dictionary)
