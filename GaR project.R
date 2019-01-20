

#Growth at Risk Research -Test Script


library("dplyr")
library("ggplot2")
library("tsa")
library("readxl")
library("quantereg")
library("lubridate")
library("xts")
library("magrittr")
library("ggfortify")
library("tidyverse")


rm(list = ls())

setwd("C:/Users/Miguel Arquez Abdala/Documents/GitHub/GaR-Project")

FCI_index <- read_excel("gfsr-financial-conditions-indices.xlsx", sheet = 2,
                        col_names = TRUE)

FCI_col <- FCI_index %>%
  select(date, COL) %>%
      mutate(Year = year(date), Quarter = quarter(date)) %>%
        group_by(Year,Quarter) %>%
          summarise(FCI_q = mean(COL)) %>%
        ungroup() %>%
            mutate(Date = seq(as.Date("1991/03/30"),as.Date("2016/09/30"),"quarter")) %>%
          select(Date, FCI_q)

GDP_base <- c(1994,1994,2005, 2005)
GDP_def <- c("current", "constant","current", "constant")
GDP_file <- paste0(GDP_def, "_", GDP_base)

for (w in seq_along(GDP_file)) {
  
  assign(GDP_file[w], read_excel(paste0("GDP_",GDP_file[w],".xlsx")))
  
}



GDP_1994_current <-  current_1994 %>%
  filter(`RAMAS DE ACTIVIDAD ECONOMICA` == "Producto Interno Bruto")  %>%
  select(-c(`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`,`2001`,
            `2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
            `RAMAS DE ACTIVIDAD ECONOMICA`))%>%
  t() %>% as.data.frame() %>%
  rownames_to_column()%>%
  mutate(Growth = V1/lag(V1,4) -1,
         quarter = seq(as.Date("1994/03/30"),as.Date("2007/12/30"),"quarter")) %>%
  rename(GDP_current = V1 , Year = rowname)



ggplot(data = FCI_col, aes(x = Date, y = FCI_q)) + 
  geom_line( color = "#00AFBB") + 
  ggtitle("Índice de condiciones financieras Colombia 1991-2016") +
  theme_minimal() +stat_smooth( method = "loess")


ggplot(data = GDP_1994_current[-(1:4),], aes(x = quarter, y = Growth)) +
  geom_line() + ggtitle("Crecimiento del PIB Precios corrientes 1994 - 2007") +
  theme_minimal() +stat_smooth( method = "loess")








