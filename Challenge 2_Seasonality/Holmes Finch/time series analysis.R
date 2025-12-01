library(TTR)
library(readxl)
library(mgcv)

kenyan_rain<-read_excel("/Users/holmesfinch/Documents/Datakind/african farmers/kenyan_rainfall.xlsx")

#LIVESTOCK#
livestock.ts<-ts(african_farmers_question.eng.livestock_by_year_month$livestock)
livestock.ts
plot(livestock.ts, col="black", ylab="Number of Questions", xlab="Month", xaxt="n")
				axis(1, at=seq(1,56, by=1),
				labels=c("11","12","1","2","3","4","5","6","7","8","9","10","11","12","1",
						"2","3","4","5","6","7","8","9","10","11","12","1","2","3","4",
						"5","6","7","8","9","10","11","12","1","2","3","4","5","6","7",
						"8","9","10","11","12","1","2","3","4","5","6"))

livestock.prop.ts<-ts(african_farmers_question.eng.livestock_by_year_month$livestock_prop)
livestock.prop.ts
plot(livestock.prop.ts, col="black", ylab="Proportion of Questions", xlab="Month", xaxt="n")
				axis(1, at=seq(1,56, by=1),
				labels=c("11","12","1","2","3","4","5","6","7","8","9","10","11","12","1",
						"2","3","4","5","6","7","8","9","10","11","12","1","2","3","4",
						"5","6","7","8","9","10","11","12","1","2","3","4","5","6","7",
						"8","9","10","11","12","1","2","3","4","5","6"))

livestock.ma3<-SMA(livestock.ts, n=3)
plot(livestock.ma3)


#CROPS#
crops.ts<-ts(african_farmers_question.eng.crops_by_year_month$crops)
crops.ts
plot(crops.ts)

crops.prop.ts<-ts(african_farmers_question.eng.crops_by_year_month$crops_prop)
crops.prop.ts
plot(crops.prop.ts)

#ANIMALS#
animals.ts<-ts(african_farmers_question.eng.animals_by_year_month$animals)
animals.ts
plot(animals.ts)

animals.prop.ts<-ts(african_farmers_question.eng.animals_by_year_month$animals_prop)
animals.prop.ts
plot(animals.prop.ts)

#TREES#
trees.ts<-ts(african_farmers_question.eng.trees_by_year_month$trees)
trees.ts
plot(trees.ts)

trees.prop.ts<-ts(african_farmers_question.eng.trees_by_year_month$trees_prop)
trees.prop.ts
plot(trees.prop.ts)

#GRASS#
grass.ts<-ts(african_farmers_question.eng.grass_by_year_month$grass)
grass.ts
plot(grass.ts)

grass.prop.ts<-ts(african_farmers_question.eng.grass_by_year_month$grass_prop)
grass.prop.ts
plot(grass.prop.ts)

#VEG/FRUIT#
veg_fruit.ts<-ts(african_farmers_question.eng.veg_fruit_by_year_month$veg_fruit)
veg_fruit.ts
plot(veg_fruit.ts)

veg_fruit.prop.ts<-ts(african_farmers_question.eng.veg_fruit_by_year_month$veg_fruit_prop)
veg_fruit.prop.ts
plot(veg_fruit.prop.ts)


#OVERLAY PLOTS#
lines(african_farmers_question.eng.grass_by_year_month$grass, col="green")
lines(african_farmers_question.eng.trees_by_year_month$trees, col="brown")
lines(african_farmers_question.eng.animals_by_year_month$animals, col="blue")
lines(african_farmers_question.eng.crops_by_year_month$crops, col="violet")
lines(african_farmers_question.eng.veg_fruit_by_year_month$veg_fruit, col="red")

legend(x="topright", fill=c("black", "green", "brown", "blue", "violet", "red"),
					legend=c("Livestock", "Grass", "Trees", "Animals", "Crops", "Vegetables/Fruit"))


lines(african_farmers_question.eng.grass_by_year_month$grass_prop, col="green")
lines(african_farmers_question.eng.trees_by_year_month$trees_prop, col="brown")
lines(african_farmers_question.eng.animals_by_year_month$animals_prop, col="blue")
lines(african_farmers_question.eng.crops_by_year_month$crops_prop, col="violet")
lines(african_farmers_question.eng.veg_fruit_by_year_month$veg_fruit_prop, col="red")

legend(x="topright", fill=c("black", "green", "brown", "blue", "violet", "red"),
					legend=c("Livestock", "Grass", "Trees", "Animals", "Crops", "Vegetables/Fruit"))


##TEST FOR SEASONALITY##
library(seastests)
isSeasonal(trees.ts, freq=12)
isSeasonal(grass.ts, freq=12)
isSeasonal(crops.ts, freq=12)
isSeasonal(animals.ts, freq=12)
isSeasonal(livestock.ts, freq=12)
isSeasonal(veg_fruit.ts, freq=12)

isSeasonal(trees.ts, freq=6)
isSeasonal(grass.ts, freq=6)
isSeasonal(crops.ts, freq=6)
isSeasonal(animals.ts, freq=6)
isSeasonal(livestock.ts, freq=6)
isSeasonal(veg_fruit.ts, freq=6)

#CORRELATION OF KENYAN RAIN WITH POSTS#
cor(livestock.ts, kenyan_rain$month_rain)
cor(livestock.ts, kenyan_rain$month_anomaly)

#CROSSCORRELATION OF KENYAN RAIN WITH POSTS#
print(ccf(kenyan_rain$month_rain, livestock.ts, main="Crosscorrelation for monthly rainfall and livestock posts"))
print(ccf(kenyan_rain$month_anomaly, livestock.ts, main="Crosscorrelation for monthly rainfall anomaly and livestock posts"))

print(ccf(kenyan_rain$month_rain, crops.ts, main="Crosscorrelation for monthly rainfall and crops posts"))
print(ccf(kenyan_rain$month_anomaly, crops.ts, main="Crosscorrelation for monthly rainfall anomaly and crops posts"))

print(ccf(kenyan_rain$month_rain, animals.ts, main="Crosscorrelation for monthly rainfall and animals posts"))
print(ccf(kenyan_rain$month_anomaly, animals.ts, main="Crosscorrelation for monthly rainfall anomaly and animals posts"))

print(ccf(kenyan_rain$month_rain, animals.ts, main="Crosscorrelation for monthly rainfall and animals posts"))
print(ccf(kenyan_rain$month_anomaly, animals.ts, main="Crosscorrelation for monthly rainfall anomaly and animals posts"))

print(ccf(kenyan_rain$month_rain, trees.ts, main="Crosscorrelation for monthly rainfall and trees posts"))
print(ccf(kenyan_rain$month_anomaly, trees.ts, main="Crosscorrelation for monthly rainfall anomaly and trees posts"))

print(ccf(kenyan_rain$month_rain, grass.ts, main="Crosscorrelation for monthly rainfall and grass posts"))
print(ccf(kenyan_rain$month_anomaly, grass.ts, main="Crosscorrelation for monthly rainfall anomaly and grass posts"))

print(ccf(kenyan_rain$month_rain, veg_fruit.ts, main="Crosscorrelation for monthly rainfall and vegetables/fruit posts"))
print(ccf(kenyan_rain$month_anomaly, veg_fruit.ts, main="Crosscorrelation for monthly rainfall anomaly and vegetables/fruit posts"))


#GAMS FOR RAIN POSTS#
livestock.rain.gam<-gam(livestock.ts~s(kenyan_rain$month_rain))
summary(livestock.rain.gam)
plot(livestock.rain.gam)
livestock.anomaly.gam<-gam(livestock.ts~s(kenyan_rain$month_anomaly))
summary(livestock.anomaly.gam)
plot(livestock.anomaly.gam)

crops.rain.gam<-gam(crops.ts~s(kenyan_rain$month_rain))
summary(crops.rain.gam)
plot(crops.rain.gam)
crops.anomaly.gam<-gam(crops.ts ~s(kenyan_rain$month_anomaly))
summary(crops.anomaly.gam)
plot(crops.anomaly.gam)

animals.rain.gam<-gam(animals.ts~s(kenyan_rain$month_rain))
summary(animals.rain.gam)
plot(animals.rain.gam)
animals.anomaly.gam<-gam(animals.ts ~s(kenyan_rain$month_anomaly))
summary(animals.anomaly.gam)
plot(animals.anomaly.gam)

trees.rain.gam<-gam(trees.ts~s(kenyan_rain$month_rain))
summary(trees.rain.gam)
plot(trees.rain.gam)
trees.anomaly.gam<-gam(trees.ts ~s(kenyan_rain$month_anomaly))
summary(trees.anomaly.gam)
plot(trees.anomaly.gam)

grass.rain.gam<-gam(grass.ts~s(kenyan_rain$month_rain))
summary(grass.rain.gam)
plot(grass.rain.gam)
grass.anomaly.gam<-gam(grass.ts ~s(kenyan_rain$month_anomaly))
summary(grass.anomaly.gam)
plot(grass.anomaly.gam)

veg_fruit.rain.gam<-gam(veg_fruit.ts~s(kenyan_rain$month_rain))
summary(veg_fruit.rain.gam)
plot(veg_fruit.rain.gam)
veg_fruit.anomaly.gam<-gam(veg_fruit.ts ~s(kenyan_rain$month_anomaly))
summary(veg_fruit.anomaly.gam)
plot(veg_fruit.anomaly.gam)

