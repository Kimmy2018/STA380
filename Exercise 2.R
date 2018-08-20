library(ggplot2)

#What is the best time of day to fly to minimize delays?

ABIA = read.csv("C:/Users/zjfhz/Desktop/predictive model/ABIA.csv")
summary(ABIA)

# Overall, how does flgihts fly out of Austin delay? 
hist(fly_out$ArrDelay, xlim = c(-40, 250), breaks =100)
abline(v=median(fly_out$ArrDelay),col="red", lwd=2)
# More flights during May, June, July
qplot(as.factor(ABIA$Month), xlab='Month', ylab='freq') + theme_bw()
# No diffrence.
qplot(as.factor(ABIA$DayofMonth), xlab='Day of Month', ylab='freq') + theme_bw()
# less flights in weekends. 
plot(as.factor(ABIA$DayOfWeek), xlab='Day of Week', ylab='freq') + theme_bw()

# take out the flights from AUS
mask = ABIA$Origin == 'AUS'
fly_out = ABIA[mask, ]

# Seperate arrival delay into different month: The September and November has very low delay, and March, June, and December has high rate of delay. 
plot(aggregate(fly_out$ArrDelay,by=list(unique.values = fly_out$Month), FUN='mean', na.rm=TRUE), type="l", xlab='Month', y='Arrival Delay', main='Arrival Delay for flights from Austin')

# Seperate arrival delay into different day of month: the delay increases during the middle of the month adnd the end of month. 
plot(aggregate(fly_out$ArrDelay,by=list(unique.values = fly_out$DayofMonth), FUN='mean', na.rm=TRUE), type="l", xlab='Month', y='Arrival Delay', main='Arrival Delay for flights from Austin')

# The mean of delay during a week: there's a 8-minute delay on Friday
plot(aggregate(fly_out$ArrDelay,by=list(unique.values = fly_out$DayOfWeek), FUN='mean', na.rm=TRUE), type="l", xlab='Day of Week', y='Arrival Delay', main='Arrival Delay for flights from Austin')

# The mean of delay during a day
index = complete.cases(fly_out['DepTime'])
fly_out = fly_out[-index, ]
fly_out$Time = ifelse(fly_out$DepTime < 1159, 'Morning', ifelse(fly_out$DepTime>1759, 'Afternoon', 'Evening'))
ggplot(fly_out) + geom_bar(aes(Time, ArrDelay), position = "dodge", stat = "summary", fun.y = "mean", fill="#FFF380") + 
  theme_bw() + xlab('Time of Day')







states = map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)


map = get_map()
ggmap(map)+geom_path(data=ABIA, mapping=aes(x=Origin, y=Dest))


origin_code = geocode(as.character(unique(ABIA$Origin)))
dest_code = geocode(as.character(unique(ABIA$Dest)))



require(graphics)

bymedian <- with(InsectSprays, reorder(spray, -count, median))
boxplot(count ~ bymedian, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE,
        col = "lightgray")










