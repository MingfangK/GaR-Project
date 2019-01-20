

#Growth at Risk Research -Test Script
library("dplyr")
library("ggplot2")
library("tsa")
library("readxl")
library("quantereg")
library("lubridate")
library("xts")


rm(list = ls())

setwd("C:/Users/Miguel Arquez Abdala/Documents/GitHub/GaR-Project")

FCI_index <- read_excel("gfsr-financial-conditions-indices.xlsx", sheet = 2,
                            col_names = TRUE)

FCI_col <- FCI_index %>%
              select(date, COL)

GDP_base <- c(1994,1994,2005, 2005)
GDP_def <- c("current", "constant","current", "constant")
GDP_file <- paste0(GDP_def, "_", GDP_base)

for (w in seq_along(GDP_file)) {
  
  assign(GDP_file[w], read_excel(paste0("GDP_",GDP_file[w],".xlsx")))
  
}


ggplot(data = FCI_col, aes(x = date, y = COL)) + 
        geom_line() + ggtitle("Indice de condiciones financieras Colombia 1991-2016")








