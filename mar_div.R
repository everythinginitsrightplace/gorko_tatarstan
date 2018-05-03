library(lubridate)
library(dplyr)
library(reshape2)
library(readxl)
library(ggthemes)
library(scales)
library(ggplot2)
marriage_and_divorce_2016_2017 <-  read_excel("браки и разводы данные за 2017 год.xls")

##### max % marriage growth
is.data.frame(marriage_and_divorce_2016_2017)
as.numeric(marriage_and_divorce_2016_2017$X__10)
apply(marriage_and_divorce_2016_2017[, 11], 2, max, na.rm = T)
marriage_and_divorce_2016_2017$`Статистическая отчетность по государственной регистрации актов гражданского состояния в Республике Татарстан за 2017 г.` <- NULL
row.names(marriage_and_divorce_2016_2017)[apply(marriage_and_divorce_2016_2017[, 11], 2, which.max)]
marriage_and_divorce_2016_2017<-marriage_and_divorce_2016_2017[-c(50,51,52),]
marriage_and_divorce_2016_2017[50,1] <- "Казань"

population <- c(NA, NA, NA, 2017, 35439, 62313, 29134, 30370, 25654, 19179, 206350, 20054, 52441, 13080, 35258, 33490, 106417, 43302, 16387, 48733, 22546, 85688, 55420, 165384, 13962, 15292, 51004, 42072, 83205, 42788, 30327, 28519, 20051, 274890, 13298, 57315, 35478, 25607, 31272, 35113, 19424, 22626, 13926, 40578, 19260, 77662, 20637, 528300,1231878)
length(population)

marriage_and_divorce_2016_2017 <- marriage_and_divorce_2016_2017[-c(48,51),]
length(population)
marriage_and_divorce_2016_2017$population <- population
marriages2017 <- marriage_and_divorce_2016_2017[-c(1:3),-c(2:8,10:11,13:16)]
names(marriages2017) <- c("Место", "Браки", "Разводы", "Общая численность населения")
marriages2017 <- marriages2017[-1, ]
marriages2017$`Браки на тысячу человек` <- (as.numeric(marriages2017$`Браки`) / marriages2017$`Общая численность населения`)*1000
marriages2017$`Разводы на тысячу человек` <- (as.numeric(marriages2017$`Разводы`) / marriages2017$`Общая численность населения`)*1000
marriages2017[ ,which.max(marriages2017$`Браки на тысячу человек`)]
rownames(marriages2017)[which.max(marriages2017$`Браки на тысячу человек`)]
# Kazan - 9.236304
ggplot(marriages2017, aes(`Браки на тысячу человек`)) + 
  geom_histogram(fill = "red", color = "green") + 
  labs(title = "Гистограмма распределения количества браков на тысячу человек в районах и городах Татарстана")+
  scale_y_continuous("Частота значений")+
  scale_x_continuous("Количество браков на тысячу населения")
  theme_fivethirtyeight()

rownames(marriages2017)[which.min(marriages2017$`Браки на тысячу человек`)]
# Laishevo - 2.757178

rownames(marriages2017)[which.max(marriages2017$`Разводы на тысячу человек`)]
# Bugulma - 4.510558
ggplot(marriages2017, aes(`Разводы на тысячу человек`)) + 
  geom_histogram(fill = "blue", color = "yellow") + 
  labs(title = "Гистограмма распределения количества разводов на тысячу человек в районах и городах Татарстана")+
  scale_y_continuous("Частота значений")+
  scale_x_continuous("Количество разводов на тысячу населения")
theme_fivethirtyeight()


rownames(marriages2017)[which.min(marriages2017$`Разводы на тысячу человек`)]
# Droshanoe - 1.552382


# Here we rearrange dataframe by decreasing of marriages
marriages_decreasing <- marriages2017[order(marriages2017$`Браки на тысячу человек`, decreasing = T),]
marriages_decreasing$`Разводы на тысячу человек` <- NULL
marriages_decreasing$Разводы <- NULL

barplot(marriages_decreasing[1:10,]$`Браки на тысячу человек`, las = 1, axes = T, names.arg = marriages_decreasing[1:10,]$Место,
        col = c("green", "red", "blue", "yellow", "orange", "lightblue", "lavender", "cornsilk", "lavender", "lightcyan"), main ="Районы/города -лидеры по количеству браков за 2017 год",
        ylab = "Браки на тысячу человек", horiz = F, cex.names = 0.65, cex.axis = 0.7, ylim= c(0,10))





# Here we rearrange dataframe by decreasing of divorces
marriages_divorce_decreasing <- marriages2017[order(marriages2017$`Разводы на тысячу человек`, decreasing = T),]
marriages_divorce_decreasing$Браки  <- NULL
marriages_divorce_decreasing$`Браки на тысячу человек`<- NULL


barplot(marriages_divorce_decreasing[1:10,]$`Разводы на тысячу человек`, las = 1, axes = T, names.arg = marriages_divorce_decreasing[1:10,]$Место,
        col = c("green", "red", "blue", "yellow", "orange", "lightblue", "lavender", "cornsilk", "lavender", "lightcyan"), main ="Районы/города -лидеры по количеству разводов за 2017 год",
        ylab = "Разводы на тысячу человек", horiz = F, cex.names = 0.65, cex.axis = 0.7, ylim= c(0,5))

