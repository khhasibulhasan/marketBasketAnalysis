# marketBasketAnalysis
#installing necessary packages
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")
install.packages("readxml")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")
install.packages("dplyr")
install.packages("shiny")
install.packages("shinydashboard")

#activating the installed packages
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(plyr)
library(dplyr)
library(shiny)
library(shinydashboard)


#data preprocessing
retail<- read_excel('D:/0002. M.Sc Data Science/0005. Courseworks/0001 ASDM/0002. TASK 02/PRACTICE 0002/Online Retail.xlsx');
retail<- retail[complete.cases(retail), ]
retail%>% mutate(Description=as.factor(Description))
retail %>% mutate(Country=as.factor(Description))
retail$Date <- as.Date(retail$InvoiceDate)
TransaTime <- format(retail$InvoiceDate, "%H: %M: %S")
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail, TransaTime)
cbind(retail, InvoiceNo)

glimpse(retail)

transactionData <- ddply(retail, c("InvoiceNo", "Date"),
                         function(df1) paste(df1$Description,
                          collapse = ","))
transactionData

transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL

colnames(transactionData) <- c("items")
transactionData

write.csv(transactionData, "D:/0002. M.Sc Data Science/0005. Courseworks/0001 ASDM/0002. TASK 02/market_basket_transaction.csv",
          quote =FALSE, row.names = FALSE)

tr <- read.transactions('D:/0002. M.Sc Data Science/0005. Courseworks/0001 ASDM/0002. TASK 02/market_basket_transaction.csv', format = 'basket', sep = ',')
tr

summary(tr)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

itemFrequencyPlot(tr, topN=20, type="absolute", col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(tr, topN=20, type="relative", col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))

summary(association.rules)

inspect(association.rules[1:10])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=3))
subset.rules <- which(colSums(is.subset(association.rules, association.rules))> 1)
length(subset.rules)

subset.association.rules <- association.rules[-subset.rules]

metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8), appearance=list(default="lhs", rhs="METAL"))

inspect(head(metal.association.rules))

metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8), appearance=list(lhs="METAL", default="rhs"))

inspect(head(metal.association.rules))

subrules <- association.rules[quality(association.rules)$confidence>0.4]

plot(subrules)

plot(subrules, method = "two-key plot")

plot(subrules, method = "matrix3D")

plot(subrules, engine = "plotly")

top10subrules <- head(subrules, n=10, by="confidence")
plot(top10subrules, method = "graph", engine = "htmlwidget")

saveAsGraph(head(subrules, n=1000, by="lift"), file = "rules.graphml")

subrules2 <- head(subrules, n=20, by="lift")

plot(subrules2, method = "paracoord")
