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



df2 <- df0 %>%
  unite("DM", Date:Time, sep= " ", 
        remove = FALSE) %>% 
  mutate(DM = as_datetime(DM),"%Y-%m-%d %H:%M:%S")


# mutate(format(as_datetime(DM), "%Y-%m-%d %H:%M:%S"))

# mutate_at(vars(DM),ymd_hms)


library(ggplot2)
library(gridExtra)

p2<-ggplot(df2, aes(DM, Global_active_power, group = 1))+
  geom_line()+
  scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "1 hour",
                   date_labels = "%a")+
  labs(x="Day",y="Global Active power (Kilowatts")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black", size = 0.5),
    panel.background = element_rect(colour = "black", size = 0.5)
  )

ggplotly(p2)
p2


library(ggplot2)
library(reshape2)


df4<-melt(df2, id.vars = 'DM', measure.vars=grep("^Sub_", colnames(df2)))


group.colors<-c(Sub_metering_1 = "Black", Sub_metering_2 = "red", Sub_metering_3 = "Blue")

p3<-ggplot(df4)+
  geom_line(aes(DM, value, colour=variable))+
  scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "1 hour",
                   date_labels = "%a")+
  scale_color_manual(values=group.colors)+
  labs(x="Day",y="Energy Sub Metering", colour="")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black", size = 0.5),
    panel.background = element_rect(colour = "black", size = 0.5)
  )

p3
ggplotly(p3)

# 
# ggplotly(p3)
# 
# 
# theme_bw() + theme(panel.border = "Black", panel.grid.major = element_blank(),
#                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "Black"),
#                    legend.box.background = element_rect(color = "black"),
#                    legend.position = c(1, 1), legend.justification = c(1, 1))







dff<-merge(df2,df4, by= c("DM"))


p4.1<-ggplot(dff)+
  geom_line(aes(DM, Voltage))+
  scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "1 hour",
                   date_labels = "%a")+
  scale_color_manual(values=group.colors)+
  labs(x="Day",y="Voltage", colour="")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black", size = 0.5),
    panel.background = element_rect(colour = "black", size = 0.5)
  )

p4.1

p4.2<-ggplot(dff)+
  geom_line(aes(DM, Global_reactive_power))+
  scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "1 hour",
                   date_labels = "%a")+
  scale_color_manual(values=group.colors)+
  labs(x="Day",y="Global Reactive Power", colour="")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black", size = 0.5),
    panel.background = element_rect(colour = "black", size = 0.5)
  )

p4.2


par(mfrow=c(2,2))
p4.1
p4.2


# install.packages("ggpubr")


library(ggpubr)
ggarrange(p2,p4.1,p3,p4.2, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


