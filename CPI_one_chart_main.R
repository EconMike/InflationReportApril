
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(scales)
setwd("G:/YOUR DIRECTORY/CPI")


#load data
df<-read_excel("data.xlsx", sheet = "prices")
class(df)
tail(df3)
str(df)

df<- df%>%mutate(date = mdy(date))
df2<-df%>%select(date,cpi_mfe,cpi)


df3<-df2%>%gather(measure, value, cpi_mfe,cpi,convert = FALSE)
df3<-df3 %>% rename(Prices=measure)
df4<-df3 %>% mutate(Prices = ifelse(Prices == "cpi", "CPI","*CORE CPI"))


ggplot(df4,aes(x=date, y=value, group=Prices))+
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-03-01"), ymin = -Inf, ymax = 4, fill = "gray", alpha = 0.2)+
  geom_line(aes(linetype=Prices, color=Prices))+
    geom_hline(yintercept=2.0, linetype="dashed", 
             color = "blue", size=.5)+
    scale_linetype_manual(values=c("solid", "twodash"))+
  scale_color_manual(values=c('red','black'))+theme_bw()+
  scale_x_date(date_breaks = "5 months",limits=c(as.Date("2019-01-01"),as.Date("2021-03-01")),labels = date_format("%b/%Y"),expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1),expand = c(0, 0),labels = function(value) paste0(value, "%"))+
  labs(title = "The Economic Monitor: Inflation", 
       caption = "*CORE Consumer Price Index excluding food and energy commodities\nThe CPI displayed is the All Urban Consumers measure\nSource: Bureau Of Labor Statistics")+xlab("")+ylab("")+
    theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 16, face="bold"),
        plot.caption = element_text(hjust = 0,size = 9),
        legend.text=element_text(size=8),
        axis.text.x = element_text(vjust = -2))+
   annotate("text", x = as.Date("2020-05-01"), y = 3,colour = "black", label = "COVID-19 Recession",fontface = 'italic',size = 2.3)+
  annotate("text", x = as.Date("2019-05-01"), y = 3.8,colour = "black", label = "Consumer Price Index",fontface = 'plain',size = 4.1)+
annotate("text", x = as.Date("2019-04-15"), y = 3.5,colour = "black", label = "12-month Percent Change",fontface = 'plain',size = 2.6)




 ggsave("CPI march.png")
warnings()
 