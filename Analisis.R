#Preparación del espacio de trabajo y obtencion de datos
wd=getwd() 
unzip(zipfile = "activity.zip", exdir = wd)
data=read.csv("activity.csv")
#La columna "date" se conviertea tipo Date.
data$date=as.Date(data$date)

#steps for day
library(ggplot2)
library(dplyr)
stepsday=data.frame()
stepsday=data %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 

head(stepsday)

#qplot(stepsday$sumsteps,data=stepsday,xlab="Spets for day",ylab="Frequency in days")
ggplot(stepsday,aes(y=stepsday$sumsteps,x=stepsday$date))+
      geom_bar(stat="identity")+
      ylab("Total Steps")+
      xlab("Date")+
      ggtitle("Total Steps by date")
  
#Calculo de la media y mediana

mean.day=round(mean(stepsday$sumsteps),digits = 4)
median.day=round(median(stepsday$sumsteps),digits = 4)

#Media de pasos por intervalo

steps.interval=data %>%
               group_by(interval) %>%
               summarize(mean.steps = mean(steps, na.rm = TRUE))

head(steps.interval)

plot(steps.interval$mean.steps ~ steps.interval$interval,
     col="black", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")

#cual es el intervalo con mas pasos

steps.interval$interval[which.max(steps.interval$mean.steps)]
round(max(steps.interval$mean.steps),digits=2)

#nuemro de datos faltantes

sum(is.na(data$steps))

#eliminar los valores N.A
#Se cambiaran los valores faltantes asignando, el valor de la media de pasos
#dados en el intervalo correspondiente

new.data=data
for (i in 1:nrow(data)){
        if(is.na(data$steps[i])){
                new.data$steps[i]=steps.interval$mean.steps[new.data$interval[i] == steps.interval$interval]
        }
}
head(new.data)

#realizando el histograma de pasos por dia de los nuevos datos
new.stepsday=data.frame()
new.stepsday=new.data %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
head(new.stepsday)

ggplot(new.stepsday,aes(y=new.stepsday$sumsteps,x=new.stepsday$date))+
       geom_bar(stat="identity")+
       ylab("Total Steps")+
       xlab("Date")+
       ggtitle("Total Steps by date")

#Calculo de le madia y mediana del nyevo conjunto de datos
new.mean.day=round(mean(new.stepsday$sumsteps),digits = 4)
new.median.day=round(median(new.stepsday$sumsteps),digits = 4)

# ¿Estos valores difieren de las estimaciones de la primera parte de la tarea?

#¿Existen diferencias en los patrones de actividad entre los días de semana y los fines de semana?


data.dayweek=new.data
data.dayweek$date=as.Date(data.dayweek$date)
#crea un factor dependiendo si es dia entre semana o fin de semana
data.dayweek$day=ifelse(weekdays(data.dayweek$date) %in% c("sabado", "domingo"), "weekend", "weekday")
data.dayweek$day=as.factor(data.dayweek$day)


#filtra por tipo de dia
Weekday=filter(data.dayweek, data.dayweek$day == "weekday")
Weekend=filter(data.dayweek, data.dayweek$day == "weekend")


Weekday=Weekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
Weekday$day="weekday"

Weekend=Weekend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
Weekend$day="weekend"

wkdayWkend=rbind(Weekday, Weekend)
wkdayWkend$day=as.factor(wkdayWkend$day)


g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))





