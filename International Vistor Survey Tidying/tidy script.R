# Tidying and reformatting the International Vistor Survey
# Source: Japan Tourism Agency website https://www.mlit.go.jp/kankocho/en/siryou/toukei/syouhityousa.html
# Script by Alex Elfering

setwd("~/Documents/GitHub/Other-Projects/International Vistor Survey Tidying")

library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(hms) # to convert datetimes to hms class
library(zoo) # for the super useful na.locf0 function

options(scipen = 999)

# Loading the full excel spreadsheet

'https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames'
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

ivs <- read_excel_allsheets('ivs.xlsx')

sample_df <- ivs[[3]]

mark1 <- sample_df %>%
  slice(-1:-2, -4:-5)


