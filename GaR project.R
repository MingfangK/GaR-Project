

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

FCI_index <- read_excel("excel_data/gfsr-financial-conditions-indices.xlsx", sheet = 2,
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
  
  assign(GDP_file[w], read_excel(paste0("excel_data/GDP_",GDP_file[w],".xlsx")))
  
}


## Empalme series PIB de distinta base

time_period <- as.data.frame(seq(as.Date("1994/03/30"),as.Date("2017/12/30"), "quarter")) %>%
  rename(date =`seq(as.Date("1994/03/30"), as.Date("2017/12/30"), "quarter")`)

GDP_all_series <-time_period %>%
  left_join(constant_1994 %>%
              filter(`RAMAS DE ACTIVIDAD ECONOMICA` == "PRODUCTO INTERNO BRUTO") %>%
              select(-c(`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`,`2001`,
                        `2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
                        `RAMAS DE ACTIVIDAD ECONOMICA`)) %>%
              t() %>% as.data.frame() %>%
              rownames_to_column()%>%
              mutate(Growth_1994 = V1/lag(V1,4) -1,
                     quarter_1994 = seq(as.Date("1994/03/30"),as.Date("2007/12/30"),"quarter")) %>%
              rename(GDP_1994 = V1 , date = quarter_1994) %>%
              select(-rowname)) %>%
              left_join(constant_2005 %>%
              filter(`RAMAS DE ACTIVIDAD ECONOMICA` == "PRODUCTO INTERNO BRUTO") %>%
              select(-c(`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,
                        `2008`,`2009`,`2010`,`2011`, `2012`, `2013`, `2014`, `2015`,
                        `2016`,`2017`,`RAMAS DE ACTIVIDAD ECONOMICA`)) %>%
                t() %>% as.data.frame() %>%
                rownames_to_column()%>%
                mutate(Growth_2005 = V1/lag(V1,4) -1,
                       quarter_2005 = seq(as.Date("2000/03/30"),as.Date("2017/12/30"),"quarter")) %>%
                rename(GDP_2005 = V1 , date = quarter_2005) %>%
                        select(-rowname)) 

GDP_definitive_series <- GDP_all_series  %>%
                  mutate(index_94 = GDP_1994/GDP_all_series[1,2],  
                         index_05 = GDP_2005/GDP_all_series[45,4])
            
            
              ggplot(data = FCI_col, aes(x = Date, y = FCI_q)) + 
              geom_line( color = "#00AFBB") + 
              ggtitle("Índice de condiciones financieras Colombia 1991-2016") +
              theme_minimal() +stat_smooth( method = "loess")
            
            
              ggplot(data = GDP_all_series[-(1:4),], aes(x = date, y = Growth_1994)) +
              geom_line() + ggtitle("Crecimiento del PIB Precios corrientes 1994 - 2007") +
              theme_minimal() +stat_smooth( method = "loess")
            
            
            
 ## Otras series de tiempo           
            
            
housing_prices <- read_excel("excel_data/housing_prices.xlsx", col_names = TRUE,
                             sheet =  1)    

cop_currency <- read_excel("excel_data/COP_currency.xlsx", col_names = TRUE,
                             sheet =  1)

oil <- read_excel("excel_data/oil_series.xlsx", col_names = TRUE,
                             sheet =  1)

policy_int_rate <- read_excel("excel_data/policy_rate.xlsx", col_names = TRUE,
                             sheet =  1)



            
            
            
            
            