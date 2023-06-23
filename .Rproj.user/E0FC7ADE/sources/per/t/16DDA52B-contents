library(tidyverse)

##Exercise 2
library(readr) # These functions are part of the readr package in the tidyverse

TRADE<-read_csv("TRADE DATA.csv")

library(dplyr) # These functions are part of the dplyr package in the tidyverse

TOTAL_IMPORTS<-filter(TRADE,PartnerName=="All countries  All --- All" & 
                        ReporterRegion!="Others")

TOTAL_IMPORTS

TOTAL_EXPORTS<-filter(TRADE,ReporterName=="All countries  All --- All" & PartnerRegion!="Others")

TOTAL_EXPORTS

EU_EXPORTS_2007<-filter(TRADE,PartnerRegion=="Europe & Central Asia" & ReporterRegion!="Others" & Year==2007)

EU_EXPORTS_2007

library(dplyr)

TOTAL_EXPORTS_ARRANGE<-arrange(TOTAL_EXPORTS,desc(Year),desc(TradeValuein1000USD))

TOTAL_EXPORTS_ARRANGE
library(dplyr)
library(readr)

EU_EXPORTS_2007_SUMMARY<-summarise(EU_EXPORTS_2007,
                                   mean = mean(TradeValuein1000USD),
                                   sd =sd(TradeValuein1000USD) ,
                                   number_obs = n())

EU_EXPORTS_2007_SUMMARY


TOP_100 <-TRADE %>% 
  filter(ReporterName=="All countries  All --- All" & PartnerRegion!="Others" & Year==2007)%>%
  top_n(100,TradeValuein1000USD)%>%
select(PartnerName,TradeValuein1000USD,PartnerRegion)

library(ggplot2)

TOP_100 <-TRADE %>% 
  filter(ReporterName=="All countries  All --- All" & PartnerRegion!="Others" & Year==2007)%>%
  top_n(100,TradeValuein1000USD)%>%
  select(PartnerName,TradeValuein1000USD,PartnerRegion)%>%
  arrange(desc(TradeValuein1000USD))

TOP_100

TOP_100 <- arrange(select(top_n(filter(TRADE,ReporterName=="All countries  All --- All" & PartnerRegion!="Others" & Year==2007), 100,TradeValuein1000USD),PartnerName,TradeValuein1000USD,PartnerRegion),desc(TradeValuein1000USD))

tiff("ggplot_bar.tiff",units="in", width=14, height=8, res=300)
ggplot(data=TOP_100,aes(x=PartnerRegion,y=TradeValuein1000USD) )+
  geom_bar(stat = "summary", fun = "mean",fill="red")+
  ggtitle("Average exports by Region for top 100 exports in 2007")+
  xlab("Region")+ylab("Average Exports")
dev.off()

tiff("ggplot_boxplot.tiff",units="in", width=14, height=8, res=300)
ggplot(data=TOP_100,aes(x=PartnerRegion,y=TradeValuein1000USD,color=PartnerRegion) )+
  geom_boxplot()+
  ggtitle("Boxplot")+
  xlab("Region")+ylab("Exports")+
  guides(color=guide_legend(title="Region"))
dev.off()
  