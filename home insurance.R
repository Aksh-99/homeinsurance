install.packages("readr")
install.packages("knitr")
install.packages("car")
install.packages("data.table")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("ggpubr")
install.packages("lubridate")
library(readr) # CSV fxile I/O, e.g. the read_csv function
library(knitr)
library(car)
library(data.table)
library(plotrix)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(lubridate)
Model_Data <- data.table(read.csv("/Users/gopakumar/Downloads/home_insurance.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL")))
t(head(Model_Data))
Model_Data$CAMPAIGN_DESC <- NULL
Model_Data$i <- NULL
total <- nrow(Model_Data)
total
str(Model_Data)
status_groups <- Model_Data[, .(count=.N, percent=round((.N/total)*100, 2)), by = POL_STATUS]
status_groups <- status_groups[order(count, decreasing = TRUE)]
status_groups
pieLabels <- paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
pie3D(status_groups$count,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
      main="Pie Chart of Policy Status ")
status_groups[POL_STATUS != 'Lapsed', POL_STATUS:= "Non Resiliated"]
status_groups[POL_STATUS == 'Lapsed', POL_STATUS:= "Resiliated"]
status_groups <- status_groups[, .(count=sum(count), percent = round((.N*100)/sum(count), 2)), by = POL_STATUS]
status_groups[,percent := round((count*100)/sum(count), 2)]
status_groups <- status_groups[order(count, decreasing = TRUE)]
status_groups
pieLabels <- paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
pie <- pie3D(status_groups$percent,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
             main="Pie Chart of Resiliation")
Model_Data$Resiliated[Model_Data$POL_STATUS == 'Lapsed'] <- 1
Model_Data$Resiliated[Model_Data$POL_STATUS != 'Lapsed'] <- 0
dt <- Model_Data[, .(SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED)]
name <- 'total_coverage'
Model_Data[, (name):= SUM_INSURED_BUILDINGS+ SUM_INSURED_CONTENTS+ SPEC_SUM_INSURED]
ordered_table <- Model_Data[order(total_coverage, decreasing = TRUE), .(Police, SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED, total_coverage)]
ordered_table <- data.table(ordered_table)
head(ordered_table, 8)
status_client <- Model_Data[!is.na(P1_EMP_STATUS), .(count=.N), by = P1_EMP_STATUS]
status_client <- status_client[order(count, decreasing = TRUE)]
status_client
palette_colors <-  c("#5e482c", "#f7d2a7", "#df45a4", "#d1223d", "#fbdb50", "#cd500d", "#d5addf", "#206536", "#b98d9b", "#ebaa7b", "#85a664", "#ef99fa")
ordered_status <- status_client$P1_EMP_STATUS
ggbarplot(status_client, x= "P1_EMP_STATUS", y= "count", xlab="Client’s professional status", ylab ="quantity policies", fill = "P1_EMP_STATUS", label=TRUE, title = "Clients professional status", label.pos = "out", order = ordered_status, palette = palette_colors)
ggscatter(Model_Data, x = "SUM_INSURED_BUILDINGS", y = "RISK_RATED_AREA_B", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Assured Sum - Building", ylab = "Geographical Classification of Risk - Building")
risk <- Model_Data[LAST_ANN_PREM_GROSS > 0]
#head(risk, 2)
ggqqplot(Model_Data$LAST_ANN_PREM_GROSS, ylab = "Premium - Total for the previous year")
month_order <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
day_order <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
Model_Data$quotemonth_n <- month(as.POSIXlt(Model_Data$QUOTE_DATE, format="%m/%d/%Y"))
Model_Data$covermonth_n <- month(as.POSIXlt(Model_Data$COVER_START, format="%d/%m/%Y"))
#i name the columns
quotesmonthDF <- data.frame(month_n = Model_Data$quotemonth_n )
coversmonthDF <- data.frame(month_n = Model_Data$covermonth_n)
#head(quotesmonthDF, 2)
#i avoid the null values and make the group by each month to get the monthly total
quotesmonthgroup <- data.table(quotesmonthDF)
quotesmonthgroup <-quotesmonthgroup[month_n <= 12]
quotesmonthgroup <-quotesmonthgroup[(order(month_n)), .(count=.N), by=month_n]

coversmonthgroup <- data.table(coversmonthDF)
coversmonthgroup <-coversmonthgroup[month_n <= 12]
coversmonthgroup <-coversmonthgroup[(order(month_n)), .(count=.N), by=month_n]
#i add the name of the month
quotesmonthgroup$month_s <- month_order[quotesmonthgroup$month_n ]
coversmonthgroup$month_s <- month_order[coversmonthgroup$month_n ]
head(quotesmonthgroup, 12)
head(coversmonthgroup, 12)
Model_Data$quotemonth_s <- month_order[Model_Data$quotemonth_n ]
Model_Data$covermonth_s <- month_order[Model_Data$covermonth_n ]
ggbarplot(quotesmonthgroup, x= "month_s" , y= "count", xlab="Month", ylab ="quantity policies", label=TRUE, title = "Most successful months in Quotation date", label.pos = "out", fill = "month_s", color = "month_s", palette = palette_colors)
ggbarplot(coversmonthgroup, x= "month_s" , y= "count", xlab="Month", ylab ="quantity policies", fill = "month_s", palette = palette_colors, label=TRUE, title = "Most successful months in Coverage date", label.pos = "out")
boxplotDF <- Model_Data[covermonth_n <= 12, .(covermonth_n, covermonth_s, total_coverage)]
ggboxplot(boxplotDF, x = "covermonth_s", y = "total_coverage", xlab="Coverage month", ylab="total coverage amount", width = 0.8, fill="covermonth_s", palette = palette_colors, order=month_order)
year_built <- Model_Data[Model_Data$YEARBUILT != 'null' & Model_Data$YEARBUILT > 0, .(count=.N), by = YEARBUILT]
year_built <- year_built[order(YEARBUILT)]
year_built
qplot(x = year_built$YEARBUILT, y = year_built$count, xlab="Year of building construction", ylab="Number of Policies", main = "Number of buildings by year construction" , geom="line")
#set client age
birthday_year <- year(as.POSIXlt(Model_Data$P1_DOB, format="%d/%m/%Y"))
cover_year <- year(as.POSIXlt(Model_Data$COVER_START, format="%d/%m/%Y"))
Model_Data$client_age <- cover_year-birthday_year
#set policy duration
cancelation_year <- year(as.POSIXlt(Model_Data$MTA_DATE, format="%d/%m/%Y"))
Model_Data$police_duration <- cancelation_year-cover_year
age_bar <- Model_Data[!is.na(client_age) & client_age > 0, .(count=.N), by = client_age]
age_bar <- age_bar[order(client_age)]
head(age_bar, 20)
years_table <- Model_Data[!is.na(police_duration) & police_duration > 0 & !is.na(client_age) & client_age > 0, .(client_age,police_duration)]

ggdensity(years_table, x = "client_age", y = "..count..", xlab = "Age of client", ylab="Policies", add = "mean", fill="#00AFBB", rug = TRUE, palette = c("#00AFBB", "#E7B800"))
ggscatter(years_table, x = "client_age", y = "police_duration", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Client Age", ylab = "Policy duration")
