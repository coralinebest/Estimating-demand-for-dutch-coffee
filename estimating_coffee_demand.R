#Loading packages
library(pacman)
p_load(tidyverse, data.table, broom, parallel, here, plotly, ggplot2, stargazer, magrittr,skimr,janitor,  tidymodels, ADAPTS, caret, yardstick, rlang, parsnip, sandwich, lmtest, haven, tinytex, rdrobust,dplyr, plotrix, plyr,readxl, usmap, stringr, finalfit, scales,tidyr, gridExtra, patchwork, EventStudy, fixest,kableExtra,wesanderson, gtsummary,tsibble, lfe, corrplot, Hmisc,hrbrthemes, dygraphs, lubridate, lfe, plm, GGally,ggfortify, feasts, cowplot, AER, extrafont, glue)

#Setting directory
setwd("/Users/coraline/Desktop/SMU/empirical industrial economics/assignment")

#Loading the dataset
dutch_coffee <- read_csv("dutch_coffee.csv")

#Deleting the first column 
dutch_coffee <- dutch_coffee[,-1]

library(extrafont)

#Summary statistic table 1 

summarystat_dutch_coffee= dutch_coffee %>% select(qu,cprice, tprice, incom, bprice, wprice, oprice)

table1= tbl_summary(summarystat_dutch_coffee,
                    statistic = list(all_continuous()  ~"{mean}, {sd}, {min}, {max}"),
                    digits = all_continuous() ~ 2, label = list(qu~"Per capita consumption of roasted coffee in kg", cprice~ "Price of roasted coffee per kg", tprice~"Price of tea per kg", incom~"Income per capita", bprice~"Price of coffee beans per kilo", wprice~"Wage for an individual (monthly)", oprice~"Price index for other goods") )%>%
  add_n()%>%
  modify_header(label="**Variable**")%>%
  modify_caption("**Descriptive statistics of the Dutch coffee market, 1990-1996**") 
table1

#Graph to see the correlation between the prices of other goods
try2<- dutch_coffee[, c("cprice", "tprice", "bprice", "incom", "wprice", "oprice", "qu")]

m<- cor(try2)

try1<- corrplot(m, method = "circle",addCoef.col = "black",title = "\n\n Correlation Plot Of Dutch coffee data \n", tl.srt = 25, type = "full",tl.col = "red", bg = "White",mar=c(0,0,1,0)) 

graph_4 <- ggplot(dutch_coffee, aes(x=cprice, y=bprice)) +
  geom_point(color="#CC79A7") +
  geom_smooth(method=lm , color="yellow", fill="#69b3a2", se=TRUE) +
  theme_ipsum(base_family = 'Helvetica')+ xlab("Price of roasted coffee per kg") + ylab("Price of coffee beans per kg")+ ggtitle("Graph 1: Price of roasted coffee and the price of coffee beans")+ theme(plot.title = element_text( size=10))
graph_4

graph_5 <- ggplot(dutch_coffee, aes(x=cprice, y=tprice)) +
  geom_point(color= "#CC79A7") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum(base_family = 'Helvetica')+xlab("Price of roasted coffee per kg") + ylab("Price of tea per kg") + ggtitle("Graph 2: Price of roasted coffee and price of tea")+theme(plot.title = element_text( size=8), axis.title.x = element_text(size=7), axis.title.y = element_text(size=7))

graph_6<- ggplot(dutch_coffee, aes(x=cprice, y= wprice)) +
  geom_point(color= "#CC79A7") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum(base_family = 'Helvetica')+xlab("Price of roasted coffee per kg")+ ylab("Wage for an individual") + ggtitle("Graph 3: Price of roasted coffee and wage") + theme(plot.title = element_text( size=8), axis.title.x = element_text(size=7), axis.title.y = element_text(size=7))


graph_total<- graph_5+graph_6 
graph_total

graph_7<- ggplot(dutch_coffee, aes(x=cprice, y= qu)) +
  geom_point(color= "#69b3a2") +
  geom_smooth(method=lm , color="red", fill="#CC79A7", se=TRUE) +
  theme_ipsum(base_family = 'Helvetica')+ ggtitle("Graph 4: Price of roasted coffee and quantity consumed") + theme(plot.title = element_text( size=7),axis.title.x = element_text(size=7), axis.title.y = element_text(size=7))+ xlab("Price of roasted coffee per kg")+ ylab("Quantity of roasted coffee consumed in kg")


graph_8<- ggplot(dutch_coffee, aes(x=tprice, y= qu)) +
  geom_point(color= "#69b3a2") +
  geom_smooth(method=lm , color="red", fill="#CC79A7", se=TRUE) +
  theme_ipsum(base_family = 'Helvetica') + ggtitle("Graph 5: Consumption of roasted coffee and price of tea") + theme(plot.title = element_text( size=7), axis.title.x = element_text(size=7), axis.title.y = element_text(size=7)) + xlab("Price of tea per kg") + ylab("Quantity of roasted coffee consumed in kg")


graphtotal2<- graph_7+graph_8
graphtotal2

dutch_coffee$date <- as.Date(paste(dutch_coffee$year, dutch_coffee$month, "01", sep="-"))
dutch_coffee2<- dutch_coffee %>% mutate(DATE = yearmonth(date)) %>% 
  as_tsibble(index=DATE) %>%
  select(qu)
plot1 <- dutch_coffee2 %>% ggplot() + geom_line(aes(x=DATE, y=qu), color="pink") + theme_bw()+ xlab("Time")+ ylab("Per capita consumption of roasted coffee in kg")
plot1+ ggtitle("Per capita consumption of roasted coffee over time")

#Seasonal plot
seasonalplot <- dutch_coffee2%>%
  gg_season(qu, labels = "both") +
  labs(y = "Consumption of roasted coffee in kg",
       title = "Seasonal plot: Consumption of roasted coffee in kg")+ theme_bw()
seasonalplot

#Simple OLS regression
reg1_dutch_coffee<- lm(data= dutch_coffee, log(qu)~ log(cprice))

#Creating the table
stargazer(reg1_dutch_coffee,
          type = "latex",
          header = FALSE,
          dep.var.labels = c("Per capita consumption of roasted coffee in kg"),
          covariate.labels = c("Price of roasted coffee per kg", "(Intercept)"),
          title= "Table 1: Estimates using simple OLS regression",
          align=TRUE)

#Regression adding quarter dummy variables
reg_dummies <- lm(log(qu)~log(cprice)+q1+q2+q3, data = dutch_coffee)

#We omit q4 to avoid perfect collinearity
#Creating the table
stargazer(reg_dummies,
          type = "latex", 
          dep.var.labels = c("Per capita consumption of roasted coffee in kg"),
          covariate.labels = c("Price of roasted coffee per kg", "(Intercept)"),
          title= "Table 2: Estimates using simple OLS regression adding quarter dummies",
          align=TRUE)

reg2_dutch_coffee <- lm(log(qu)~log(cprice)+log(wprice)+log(incom)+ log(tprice)+q1+q2+q3, data = dutch_coffee)

#Creating the table
stargazer(reg2_dutch_coffee,
          dep.var.labels = c("Per capita consumption of roasted coffee in kg"),
          covariate.labels = c("Price of roasted coffee per kg", "Wage for an individual", "Income per capita", "Price of tea per kg"),
          type = "latex", 
          title= "Table 3: Estimates using OLS regression adding controls",
          align=TRUE)

#First stage regression
firststage<- lm(log(cprice)~log(bprice)+q1+ q2+q3, data = dutch_coffee)
firststage
stargazer(firststage, type="latex",title= "First stage regression results",
          align=TRUE, dep.var.labels = c("Per capita consumption of roasted coffee in kg"),
          covariate.labels = c("Price of coffee beans per kg"))




# Second stage regression with Instrumental variable using ivreg

reg_iv<- ivreg(log(qu) ~ log(cprice)| bprice, data = dutch_coffee)

#Creating the table
stargazer(reg_iv,
          dep.var.labels = c("Per capita consumption of roasted coffee in kg"),
          covariate.labels = c("Price of roasted coffee per kg"),
          type = "text", 
          title= "Table 2: Estimates of OLS regression adding controls",
          align=TRUE)

# Creating marginal cost variable
dutch_coffee<- dutch_coffee %>% mutate(marginal_cost= 4)

# Creating the Lerner index variable
dutch_coffee<- dutch_coffee %>% mutate(lerner_index=(cprice-marginal_cost)/cprice)

#Creating the adjusted Lerner index

dutch_coffee <- dutch_coffee %>% mutate(lerner_adjusted_index= lerner_index * (0.195))

dutch_coffee$date <- as.Date(paste(dutch_coffee$year, dutch_coffee$month, "01", sep="-"))

dutch_coffee1<- dutch_coffee %>% mutate(DATE = yearmonth(date)) %>% 
  as_tsibble(index=DATE) %>%
  select(lerner_index, lerner_adjusted_index)


p3 <- dutch_coffee1 %>% ggplot() + geom_line(aes(x=DATE, y=lerner_index), color="pink") + theme_bw() + ylab("Lerner and adjusted Lerner index")+ geom_line(aes(x=DATE, y=lerner_adjusted_index), color="#69b3a2") + theme_bw() + geom_line(aes(x=DATE, y=lerner_adjusted_index), color="#69b3a2") + theme_bw() + scale_y_continuous(limits = c(0,1))
p3

# Creating marginal cost variable
dutch_coffee<- dutch_coffee %>% mutate(lambda= 0.195-(0.195*marginal_cost)/cprice)

#In order to estimate the lambda we can take the mean of the variable we just created 

mean(dutch_coffee$lambda)
