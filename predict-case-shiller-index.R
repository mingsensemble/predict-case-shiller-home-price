rm(list = ls())
require(quantmod)
require(zoo)
# download case-shiller housing price index Chicago seasonally adjusted
getSymbols('CHXRSA', src='FRED')
# ================================
# load redfin data
redfin <- read.csv("~/working_datasets/redfin/data_crosstab-chicago-asof-201812.csv", fileEncoding = "UCS-2LE", sep = "\t")
rfDf <- subset(redfin, Region == "Chicago, IL", select = c("Month.of.Period.End", "Median.Sale.Price", "Homes.Sold", "New.Listings", "Inventory", "Days.on.Market", "Average.Sale.To.List"))

rfDf$Month.of.Period.End <- as.yearmon(rfDf$Month.of.Period.End)
# process strings into numbers
rfDf$New.Listings <- as.numeric(gsub(",", "", rfDf$New.Listings))
rfDf$Inventory <- as.numeric(gsub(",", "", rfDf$Inventory))
rfDf$Median.Sale.Price <- as.numeric(gsub("\\D", "", rfDf$Median.Sale.Price)) * 1000
rfDf$Average.Sale.To.List <- as.numeric(gsub("\\D", "", rfDf$Average.Sale.To.List)) * 0.001
# ================================
require(ggplot2)
df <- as.data.frame(CHXRSA)
df$mon <- as.yearmon(row.names(df))

pltDf <- merge(df, rfDf, by.x = "mon", by.y = "Month.of.Period.End")

p1 <- ggplot(pltDf, aes(x = mon, y = Median.Sale.Price/1000)) + geom_bar(stat = "identity", fill = "#00a0dc") + expand_limits(y = 0) + scale_x_yearmon(breaks =  scales::pretty_breaks(n = 4)) + xlab("") + ylab("Median Sale Price ($K)") + theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(colour = "grey80"),
  panel.border = element_rect(colour = "grey80", fill = NA)
) + ggtitle("RedFin Single-Family Housing Sale Price")

# p1 <- ggplot(pltDf, aes(x = mon, y = Median.Sale.Price/1000)) + geom_bar(stat = "identity", fill = "white") + expand_limits(y = 0) + scale_x_yearmon(breaks =  scales::pretty_breaks(n = 4)) + xlab("") + ylab("") + theme(
  # panel.background = element_rect(fill = "#00a0dc"),
  # panel.grid.major.y = element_line(colour = "grey80"),
  # panel.grid.minor.y = element_line(colour = "grey80"),
  # panel.border = element_rect(colour = "grey80", fill = NA), 
  # plot.background = element_rect(fill = "#00a0dc"),
  # axis.text = element_text(colour = "white")
# ) 
# ggsave(p1, file = "~/Downloads/redfin.png", width = 3, height = 3)
# ==================================
p2 <- ggplot(pltDf, aes(x = mon, y = CHXRSA)) + geom_line(colour = "#00a0dc", size = 2) + expand_limits(y = 100) + scale_x_yearmon(breaks =  scales::pretty_breaks(n = 4))+ theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(colour = "grey80"),
  panel.grid.minor.y = element_line(colour = "grey80"),
  panel.border = element_rect(colour = "grey80", fill = NA)
)  + xlab("Month") + ylab("Index - Jan 2000 = 100") + ggtitle("Case-Shiller Home Price Index - Chicago")

# p2 <- ggplot(pltDf, aes(x = mon, y = CHXRSA)) + geom_line(colour = "white", size = 3) + expand_limits(y = 100) + scale_x_yearmon(breaks =  scales::pretty_breaks(n = 4))+ theme(
  # panel.background = element_rect(fill = "#00a0dc"),
  # panel.grid.major.y = element_line(colour = "grey80"),
  # panel.grid.minor.y = element_line(colour = "grey80"),
  # panel.border = element_rect(colour = "grey80", fill = NA), 
  # plot.background = element_rect(fill = "#00a0dc"),
  # axis.text = element_text(colour = "white")
# )  + xlab("") + ylab("") 

# ggsave(p2, file = "~/Downloads/case-shiller.png", width = 3, height = 3)

