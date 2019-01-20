

#Growth at Risk Research -Test Script
library("dplyr")
library("ggplot2")
library("tsa")
library("readxl")


setwd("C:/Users/Miguel Arquez Abdala/Documents/GitHub/GaR-Project")

FCI_index <- read_excel("gfsr-financial-conditions-indices.xlsx", sheet = 2,
                            col_names = TRUE)

FCI_col <- FCI_index %>%
              select(date, COL)

ggplot(data = FCI_col, aes(x = date, y = COL)) + 
        geom_line() + ggtitle("Indice de condiciones financieras Colombia 1991-2016")








