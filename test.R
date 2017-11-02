library(flexdashboard)



#library(xlsx)
setwd("C:/Users/johnalva/Box Sync/My Documents/POOLS/AECOM/Daily Report/AECOM Queue Mgmt")
aecom <- read.csv("CloseToWatson.csv")
attach(aecom)
library(dplyr)
library(lubridate)
library(plotly)
aecom$new_date <- mdy_hm(aecom$inc_closed_at)
aecom$day <- day(aecom$new_date)
aecom$month <- month(aecom$new_date)

p1 <- aggregate(data = aecom, Real_Total_Duration_Hours ~ inc_priority + 
                    day + month, mean)

p1 <- p1[order(p1$month, p1$day),]

#p3 <- aggregate(data = aecom, inc_priority ~ day, count)
#str(aecom)
p2e <- aggregate(data = subset(aecom, month == 10), Real_Total_Duration_Hours ~ day, mean)

p3 <- tbl_df(table(aecom$inc_priority, aecom$day, aecom$month))
p3$Var2 <- as.numeric(p3$Var2)
p3 <- p3[order(p3$Var2),]

# #View(p3)
# class(p3$Var2)
# pp <- function(monthVar){
#     plot_ly(data = subset(p3, Var3 == monthVar),
#             x = ~Var2,
#             y = ~n,
#             type = 'bar',
#             color = ~Var1)
#     
# }


# ####################
# library(plotly)
# ac2 <- subset(aecom, month == 10)
# p1 <- aggregate(data = ac2, Real_Total_Duration_Hours ~ inc_priority +
#                     day, mean)
# totals <- tbl_df(table(ac2$inc_priority, ac2$day, ac2$month))
# View(totals)
# names(totals) <- c('inc_priority', 'day', 'month', 'total')
# t2 <- merge(p1, totals)
# 



# t4 <- ggplot(data = t2, aes(x = day, y = total, fill = inc_priority)) +
#     geom_bar(stat = 'identity', position = position_dodge())
# 
# 
# t4 <- ggplot(t2) +
#     geom_bar(aes(x = day, y = total, fill = inc_priority), stat = 'identity', position = position_dodge()) +
#     geom_line(aes(x = day, y = Real_Total_Duration_Hours, color = inc_priority))
# 
# 
# 
# ggplotly(t4)
# ##
# 
# install.packages("ggplot2")

    #geom_line(aes(x = day, y = Real_Total_Duration_Hours, colour = inc_priority), stat = 'identity')






# 
# class(t2)
# 
# plot_ly(t2,
#         x = ~day,
#         y = ~total,
#         type = 'bar',
#         color = ~inc_priority
#         ) %>%
#     add_trace(y = ~Real_Total_Duration_Hours, line = list(shape = "spline"))
# 



####################

#  ------------------------------------------------------------------------

ac2 <- aecom
p1 <- aggregate(data = ac2, Real_Total_Duration_Hours ~ inc_priority +
                    day + month, mean)
totals <- tbl_df(table(ac2$inc_priority, ac2$day, ac2$month))

names(totals) <- c('inc_priority', 'day', 'month', 'total')
t2 <- merge(p1, totals)
t2$day <- as.numeric(t2$day)
t2 <- merge(p1, totals)
t2 <- t2[order(t2$month, t2$day),]
t2$Real_Total_Duration_Hours <- round(t2$Real_Total_Duration_Hours, digits = 2)


pl <- function(monthP, priorityP){
    plot_ly(data = subset(t2, month == monthP & inc_priority == priorityP)) %>%
            add_trace(x = ~day, y = ~total, type = 'bar', name ='Volume INC', hoverinfo = 'text',
                      text = ~paste("Day:", day, "Volume:", total)) %>%
            add_trace(x = ~day, y = ~Real_Total_Duration_Hours, hoverinfo = 'text', 
                      line = list(shape = 'spline'), 
                      text = ~paste("Day:", day,"AV MTTR:", Real_Total_Duration_Hours),
                      yaxis = 'y2', name = 'Average INC') %>%
            layout(title = paste0(priorityP, ', AECOM - Volume vs MTTR accross by ', month.name[monthP]),
                xaxis = list(title = "Days"),
                yaxis = list(side = 'left', title = "Volume INC Closed", showgrid = FALSE, zeroline = FALSE),
                yaxis2 = list(side = 'right', overlaying = "y", title = 'Average MTTR', showgrid = FALSE, zeroline = FALSE)
            )
}

pl(10, 'P1 - Critical')
pl(10, 'P2 - Major')
pl(7, 'P3 - Minor')
pl(10, 'P4 - Low')


priority1 <- pl(8, 'P1 - Critical')
priority2 <- pl(10, 'P2 - Major')
priority3 <- pl(10, 'P3 - Minor')
priority4 <- pl(10, 'P4 - Low')

subplot(priority1, priority2)
priority1
priority2
priority3
priority4


#  ------------------------------------------------------------------------




airquality_sept <- airquality[which(airquality$Month == 9),]
airquality_sept$Date <- as.Date(paste(airquality_sept$Month, airquality_sept$Day, 1973, sep = "."), format = "%m.%d.%Y")

p <- plot_ly(airquality_sept) %>%
    add_trace(x = ~Date, y = ~Wind, type = 'bar', name = 'Wind',
              marker = list(color = '#C9EFF9'),
              hoverinfo = "text",
              text = ~paste(Wind, ' mph')) %>%
    add_trace(x = ~Date, y = ~Temp, type = 'scatter', mode = 'lines', name = 'Temperature', yaxis = 'y2',
              line = list(color = '#45171D'),
              hoverinfo = "text",
              text = ~paste(Temp, '°F')) %>%
    layout(title = 'New York Wind and Temperature Measurements for September 1973',
           xaxis = list(title = ""),
           yaxis = list(side = 'left', title = 'Wind in mph', showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', overlaying = "y", title = 'Temperature in degrees F', showgrid = FALSE, zeroline = FALSE))


p


























plot_ly(
    data = p2e,
    x = ~day,
    y = ~Real_Total_Duration_Hours,
    line = list(shape= "spline")
)



p <- plot_ly(subset(p1, month == 10), 
             x = ~day, 
             y = ~Real_Total_Duration_Hours, 
             line = list(shape = "spline"),
             #type = 'bar',
             color = ~inc_priority)
p


subplot(p2ePlot, p, counting, nrows = 3, shareX = F)





p2 <- plot_ly(subset(p1, day = 1), labels = ~inc_priority, values = ~Real_Total_Duration_Hours, 
              type = 'pie')

p2


library(qcc)
p1
qcc(data = p1$Real_Total_Duration_Hours)


################
p <- plot_ly(p1, x = ~day, y = ~Real_Total_Duration_Hours, 
             type = 'scatter',
             #mode = 'lines',
             split = ~inc_priority,
             line = list(shape = "bar"),
             color = ~inc_priority, frame = ~day)

p

p2 <- plot_ly(subset(p1, day = 1), labels = ~inc_priority, values = ~Real_Total_Duration_Hours, 
              type = 'pie')
              
p2
p

subplot(p2, nrows = 2)

library(car)
a2 = aecom[, c(1,2,3,4,5)]
View(a2)



library(plotly)
View(midwest)

summary(midwest)
hist(midwest$popblack)
hist(midwest$popadults)
hist(midwest$popwhite)

plot(density(midwest$popadults))
library(car)
car::Baumann

a2 <- a1[,c(1,2,3,4,5)]

dim(midwest)
mi <- midwest[,-c(2,3,28)]
View(mi)
mi2 <- mi[,c(5:8)]
scatterplotMatrix(mi2)

pc1 <- princomp(mi2, cor = T)
pc1
plot(pc1)
summary(pc1)

biplot(pc1)
View(mi2)
a.cp = pc1$scores[,1:2]
plot(a.cp[,1], a.cp[,2])
a.cp[,1] <- -a.cp[,1]
plot(a.cp[,1], a.cp[,2])
loadings(pc1)


library(cluster)

ag1 <- agnes(a2, method = "single")


library(plotly)
packageVersion('plotly')

uspe <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
uspe
data <- uspe[, c('Categorie', 'X1960')]
p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie',
             #hoverinfo = 'text',
             textinfo = 'label+percent',
             text = ~paste('$', X1960, 'billions'),
             showlegend = FALSE)
p


library(car)



#  ------------------------------------------------------------------------

library(car)
excel <- abs(rnorm(80,0.2,0.27))
powerpoint <- abs(rnorm(80,0.2,0.27))
word <- abs(rnorm(80,0.2,0.27))
internet <- abs(rnorm(80,0.2,0.27))

excel
powerpoint
word
internet

dataExample <- data.frame(excel, powerpoint, word, internet)
dataExample


p <- dataExample %>%
    plot_ly(
        x = ~excel,
        y = ~powerpoint,
        frame = ~internet,
        type = 'line',
        mode = 'markers',
        showlegend = F
    )
p

hist(dataExample$internet)


scatterplotMatrix(dataExample)

pc1 <- princomp(dataExample, cor = T)
pc1
plot(pc1)

summary(pc1)
biplot(pc1)
View(dataExample)
a.cp <- pc1$scores
a.cp2 <- pc1$scores[,1:2]
a.cp2
plot(a.cp2)
a.cp2[,1] <- -a.cp2[,1]
plot(a.cp2[,1], a.cp2[,2])

# Analysis of conglomerado
library(cluster)
# aglomerativo

ag1 <- agnes(dataExample, method = 'single')
ag2 <- agnes(dataExample, method = 'complete')
ag3 <- agnes(dataExample, method = 'average')

plot(ag1)
plot(ag2)
plot(ag3)


div.grupo.ag2 <- cutree(ag2,4)
div.grupo.ag3 <- cutree(ag2,4)

View(a.cp2)
plot(a.cp2, col = div.grupo.ag2)
plot(a.cp2, col = div.grupo.ag3)


newData <- read.csv('https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/Titanic.csv')
attach(newData)
names(newData)
library(dplyr)
nd <- select(newData, PClass, Sex, Age, SexCode,Survived)
scatterplotMatrix(nd)
names(nd)
biplot(nd)
names(nd)
nd2 <- nd[,3:5]
nd2 <- na.omit(nd2)
pc2 <- princomp(nd2, cor = T)
summary(pc2)
biplot(pc2)
a.cp3 <- pc2$scores
a.cp3
plot(a.cp3)
library(car)
library(cluster)
ag1.1 <- agnes(newData, method = 'single')
ag1.2 <- agnes(newData, method = 'complete')
ag1.3 <- agnes(newData, method = 'average')

plot(ag1.1)
plot(ag1.2)
plot(ag1.3)

View(tbl_df(table(nd$PClass, nd$Sex, nd$Survived)))
tbl_df(table(nd$Age))
summary(nd)




div.grupo.ag1.2 <- cutree(ag1.2, 4)

plot(a.cp3, col = div.grupo.ag1.2)


crime <- read.csv('https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/plm/Crime.csv')
View(crime)

setwd("C:/Users/johnalva/Downloads")
aecom <- read.csv('incident (54).csv')
View(aecom)








