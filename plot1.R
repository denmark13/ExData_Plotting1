library(readr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(shiny)
library(kableExtra)
library(lubridate)
library(xlsx)
library(DT)
library(readxl)
library(janitor)
library(dplyr)

df <- read_delim("exdata_data_household_power_consumption/household_power_consumption.txt", 
                 delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                 trim_ws = TRUE)


# df1<- df %>%
#   drop_na() %>% 
#   mutate_at(vars(Global_active_power,
#                                 Global_reactive_power,
#                                 Voltage,
#                                 Global_intensity,
#                                 Sub_metering_1,
#                                 Sub_metering_2), as.numeric)

df0<- df %>%                
  filter(grepl("2007-02-01", Date) | grepl("2007-02-02", Date)) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.numeric) %>% 
  mutate(week = wday(Date, label = TRUE))



# 
#   geom_histogram(Bins= 15)+
#   # geom_bar(aes(y = ..prop.., group = 1))+
#   
#   theme_ipsum()

p<-ggplot(df2, aes(x= Global_active_power)) +
  geom_histogram(binwidth = .35, fill="red", color="Black") +
  labs(x="Global Active Power in Kilowatts",y="Frequency", title = "Global Active Power")+
  theme_bw()+theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black", size = 0.5),
    panel.background = element_rect(colour = "black", size = 0.5)
  )

p


