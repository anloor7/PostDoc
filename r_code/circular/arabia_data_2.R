

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_circular/applications')
wind_data <- read.csv('arabia.csv')
wind_data <- wind_data[, c(3, 5, 10)]

# Selecting only the observations pertaining to the year 2017

wind_data_clean <- subset(wind_data, OBSERVATION_DATE >= as.Date('2010-01-01') & OBSERVATION_DATE <= as.Date('2017-12-31'))

list_series_1 <- list()
list_series_2 <- list()
list_series_3 <- list()

# Selecting the time of one particular station


wind_data_s1 <- subset(wind_data_clean, STATION_NAME == 'ABHA')

list_series_1[[1]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-01-01') & OBSERVATION_DATE <= as.Date('2010-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[2]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-02-01') & OBSERVATION_DATE <= as.Date('2010-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[3]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-03-01') & OBSERVATION_DATE <= as.Date('2010-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[4]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-04-01') & OBSERVATION_DATE <= as.Date('2010-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[5]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-05-01') & OBSERVATION_DATE <= as.Date('2010-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[6]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-06-01') & OBSERVATION_DATE <= as.Date('2010-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[7]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-07-01') & OBSERVATION_DATE <= as.Date('2010-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[8]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-08-01') & OBSERVATION_DATE <= as.Date('2010-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[9]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-09-01') & OBSERVATION_DATE <= as.Date('2010-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[10]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-10-01') & OBSERVATION_DATE <= as.Date('2010-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[11]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-11-01') & OBSERVATION_DATE <= as.Date('2010-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[12]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-12-01') & OBSERVATION_DATE <= as.Date('2010-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[13]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-01-01') & OBSERVATION_DATE <= as.Date('2011-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[14]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-02-01') & OBSERVATION_DATE <= as.Date('2011-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[15]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-03-01') & OBSERVATION_DATE <= as.Date('2011-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[16]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-04-01') & OBSERVATION_DATE <= as.Date('2011-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[17]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-05-01') & OBSERVATION_DATE <= as.Date('2011-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[18]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-06-01') & OBSERVATION_DATE <= as.Date('2011-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[19]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-07-01') & OBSERVATION_DATE <= as.Date('2011-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[20]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-08-01') & OBSERVATION_DATE <= as.Date('2011-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[21]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-09-01') & OBSERVATION_DATE <= as.Date('2011-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[22]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-10-01') & OBSERVATION_DATE <= as.Date('2011-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[23]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-11-01') & OBSERVATION_DATE <= as.Date('2011-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[24]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-12-01') & OBSERVATION_DATE <= as.Date('2011-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[25]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-01-01') & OBSERVATION_DATE <= as.Date('2012-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[26]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-02-01') & OBSERVATION_DATE <= as.Date('2012-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[27]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-03-01') & OBSERVATION_DATE <= as.Date('2012-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[28]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-04-01') & OBSERVATION_DATE <= as.Date('2012-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[29]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-05-01') & OBSERVATION_DATE <= as.Date('2012-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[30]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-06-01') & OBSERVATION_DATE <= as.Date('2012-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[31]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-07-01') & OBSERVATION_DATE <= as.Date('2012-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[32]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-08-01') & OBSERVATION_DATE <= as.Date('2012-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[33]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-09-01') & OBSERVATION_DATE <= as.Date('2012-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[34]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-10-01') & OBSERVATION_DATE <= as.Date('2012-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[35]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-11-01') & OBSERVATION_DATE <= as.Date('2012-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[36]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-12-01') & OBSERVATION_DATE <= as.Date('2012-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[37]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-01-01') & OBSERVATION_DATE <= as.Date('2013-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[38]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-02-01') & OBSERVATION_DATE <= as.Date('2013-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[39]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-03-01') & OBSERVATION_DATE <= as.Date('2013-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[40]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-04-01') & OBSERVATION_DATE <= as.Date('2013-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[41]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-05-01') & OBSERVATION_DATE <= as.Date('2013-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[42]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-06-01') & OBSERVATION_DATE <= as.Date('2013-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[43]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-07-01') & OBSERVATION_DATE <= as.Date('2013-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[44]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-08-01') & OBSERVATION_DATE <= as.Date('2013-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[45]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-09-01') & OBSERVATION_DATE <= as.Date('2013-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[46]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-10-01') & OBSERVATION_DATE <= as.Date('2013-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[47]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-11-01') & OBSERVATION_DATE <= as.Date('2013-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[48]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-12-01') & OBSERVATION_DATE <= as.Date('2013-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[49]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-01-01') & OBSERVATION_DATE <= as.Date('2014-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[50]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-02-01') & OBSERVATION_DATE <= as.Date('2014-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[51]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-03-01') & OBSERVATION_DATE <= as.Date('2014-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[52]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-04-01') & OBSERVATION_DATE <= as.Date('2014-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[53]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-05-01') & OBSERVATION_DATE <= as.Date('2014-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[54]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-06-01') & OBSERVATION_DATE <= as.Date('2014-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[55]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-07-01') & OBSERVATION_DATE <= as.Date('2014-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[56]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-08-01') & OBSERVATION_DATE <= as.Date('2014-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[57]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-09-01') & OBSERVATION_DATE <= as.Date('2014-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[58]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-10-01') & OBSERVATION_DATE <= as.Date('2014-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[59]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-11-01') & OBSERVATION_DATE <= as.Date('2014-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[60]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-12-01') & OBSERVATION_DATE <= as.Date('2014-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[61]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-01-01') & OBSERVATION_DATE <= as.Date('2015-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[62]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-02-01') & OBSERVATION_DATE <= as.Date('2015-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[63]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-03-01') & OBSERVATION_DATE <= as.Date('2015-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[64]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-04-01') & OBSERVATION_DATE <= as.Date('2015-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[65]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-05-01') & OBSERVATION_DATE <= as.Date('2015-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[66]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-06-01') & OBSERVATION_DATE <= as.Date('2015-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[67]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-07-01') & OBSERVATION_DATE <= as.Date('2015-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[68]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-08-01') & OBSERVATION_DATE <= as.Date('2015-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[69]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-09-01') & OBSERVATION_DATE <= as.Date('2015-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[70]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-10-01') & OBSERVATION_DATE <= as.Date('2015-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[71]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-11-01') & OBSERVATION_DATE <= as.Date('2015-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[72]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-12-01') & OBSERVATION_DATE <= as.Date('2015-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[73]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-01-01') & OBSERVATION_DATE <= as.Date('2016-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[74]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-02-01') & OBSERVATION_DATE <= as.Date('2016-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[75]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-03-01') & OBSERVATION_DATE <= as.Date('2016-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[76]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-04-01') & OBSERVATION_DATE <= as.Date('2016-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[77]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-05-01') & OBSERVATION_DATE <= as.Date('2016-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[78]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-06-01') & OBSERVATION_DATE <= as.Date('2016-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[79]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-07-01') & OBSERVATION_DATE <= as.Date('2016-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[80]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-08-01') & OBSERVATION_DATE <= as.Date('2016-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[81]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-09-01') & OBSERVATION_DATE <= as.Date('2016-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[82]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-10-01') & OBSERVATION_DATE <= as.Date('2016-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[83]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-11-01') & OBSERVATION_DATE <= as.Date('2016-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[84]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-12-01') & OBSERVATION_DATE <= as.Date('2016-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[85]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-01-01') & OBSERVATION_DATE <= as.Date('2017-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[86]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-02-01') & OBSERVATION_DATE <= as.Date('2017-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[87]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-03-01') & OBSERVATION_DATE <= as.Date('2017-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[88]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-04-01') & OBSERVATION_DATE <= as.Date('2017-04-30'))$WIND_DIRECTION_ANGLE
list_series_1[[89]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-05-01') & OBSERVATION_DATE <= as.Date('2017-05-31'))$WIND_DIRECTION_ANGLE
list_series_1[[90]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-06-01') & OBSERVATION_DATE <= as.Date('2017-06-30'))$WIND_DIRECTION_ANGLE
list_series_1[[91]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-07-01') & OBSERVATION_DATE <= as.Date('2017-07-31'))$WIND_DIRECTION_ANGLE
list_series_1[[92]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-08-01') & OBSERVATION_DATE <= as.Date('2017-08-31'))$WIND_DIRECTION_ANGLE
list_series_1[[93]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-09-01') & OBSERVATION_DATE <= as.Date('2017-09-30'))$WIND_DIRECTION_ANGLE
list_series_1[[94]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-10-01') & OBSERVATION_DATE <= as.Date('2017-10-31'))$WIND_DIRECTION_ANGLE
list_series_1[[95]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-11-01') & OBSERVATION_DATE <= as.Date('2017-11-30'))$WIND_DIRECTION_ANGLE
list_series_1[[96]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-12-01') & OBSERVATION_DATE <= as.Date('2017-12-31'))$WIND_DIRECTION_ANGLE


# Selecting the time of other particular station

wind_data_s2 <- subset(wind_data_clean, STATION_NAME == 'MAKKAH')

list_series_2[[1]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-01-01') & OBSERVATION_DATE <= as.Date('2010-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[2]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-02-01') & OBSERVATION_DATE <= as.Date('2010-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[3]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-03-01') & OBSERVATION_DATE <= as.Date('2010-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[4]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-04-01') & OBSERVATION_DATE <= as.Date('2010-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[5]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-05-01') & OBSERVATION_DATE <= as.Date('2010-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[6]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-06-01') & OBSERVATION_DATE <= as.Date('2010-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[7]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-07-01') & OBSERVATION_DATE <= as.Date('2010-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[8]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-08-01') & OBSERVATION_DATE <= as.Date('2010-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[9]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-09-01') & OBSERVATION_DATE <= as.Date('2010-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[10]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-10-01') & OBSERVATION_DATE <= as.Date('2010-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[11]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-11-01') & OBSERVATION_DATE <= as.Date('2010-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[12]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-12-01') & OBSERVATION_DATE <= as.Date('2010-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[13]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-01-01') & OBSERVATION_DATE <= as.Date('2011-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[14]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-02-01') & OBSERVATION_DATE <= as.Date('2011-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[15]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-03-01') & OBSERVATION_DATE <= as.Date('2011-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[16]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-04-01') & OBSERVATION_DATE <= as.Date('2011-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[17]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-05-01') & OBSERVATION_DATE <= as.Date('2011-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[18]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-06-01') & OBSERVATION_DATE <= as.Date('2011-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[19]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-07-01') & OBSERVATION_DATE <= as.Date('2011-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[20]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-08-01') & OBSERVATION_DATE <= as.Date('2011-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[21]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-09-01') & OBSERVATION_DATE <= as.Date('2011-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[22]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-10-01') & OBSERVATION_DATE <= as.Date('2011-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[23]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-11-01') & OBSERVATION_DATE <= as.Date('2011-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[24]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-12-01') & OBSERVATION_DATE <= as.Date('2011-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[25]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-01-01') & OBSERVATION_DATE <= as.Date('2012-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[26]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-02-01') & OBSERVATION_DATE <= as.Date('2012-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[27]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-03-01') & OBSERVATION_DATE <= as.Date('2012-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[28]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-04-01') & OBSERVATION_DATE <= as.Date('2012-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[29]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-05-01') & OBSERVATION_DATE <= as.Date('2012-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[30]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-06-01') & OBSERVATION_DATE <= as.Date('2012-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[31]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-07-01') & OBSERVATION_DATE <= as.Date('2012-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[32]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-08-01') & OBSERVATION_DATE <= as.Date('2012-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[33]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-09-01') & OBSERVATION_DATE <= as.Date('2012-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[34]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-10-01') & OBSERVATION_DATE <= as.Date('2012-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[35]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-11-01') & OBSERVATION_DATE <= as.Date('2012-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[36]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-12-01') & OBSERVATION_DATE <= as.Date('2012-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[37]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-01-01') & OBSERVATION_DATE <= as.Date('2013-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[38]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-02-01') & OBSERVATION_DATE <= as.Date('2013-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[39]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-03-01') & OBSERVATION_DATE <= as.Date('2013-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[40]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-04-01') & OBSERVATION_DATE <= as.Date('2013-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[41]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-05-01') & OBSERVATION_DATE <= as.Date('2013-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[42]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-06-01') & OBSERVATION_DATE <= as.Date('2013-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[43]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-07-01') & OBSERVATION_DATE <= as.Date('2013-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[44]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-08-01') & OBSERVATION_DATE <= as.Date('2013-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[45]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-09-01') & OBSERVATION_DATE <= as.Date('2013-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[46]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-10-01') & OBSERVATION_DATE <= as.Date('2013-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[47]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-11-01') & OBSERVATION_DATE <= as.Date('2013-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[48]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-12-01') & OBSERVATION_DATE <= as.Date('2013-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[49]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-01-01') & OBSERVATION_DATE <= as.Date('2014-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[50]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-02-01') & OBSERVATION_DATE <= as.Date('2014-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[51]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-03-01') & OBSERVATION_DATE <= as.Date('2014-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[52]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-04-01') & OBSERVATION_DATE <= as.Date('2014-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[53]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-05-01') & OBSERVATION_DATE <= as.Date('2014-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[54]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-06-01') & OBSERVATION_DATE <= as.Date('2014-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[55]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-07-01') & OBSERVATION_DATE <= as.Date('2014-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[56]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-08-01') & OBSERVATION_DATE <= as.Date('2014-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[57]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-09-01') & OBSERVATION_DATE <= as.Date('2014-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[58]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-10-01') & OBSERVATION_DATE <= as.Date('2014-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[59]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-11-01') & OBSERVATION_DATE <= as.Date('2014-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[60]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-12-01') & OBSERVATION_DATE <= as.Date('2014-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[61]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-01-01') & OBSERVATION_DATE <= as.Date('2015-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[62]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-02-01') & OBSERVATION_DATE <= as.Date('2015-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[63]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-03-01') & OBSERVATION_DATE <= as.Date('2015-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[64]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-04-01') & OBSERVATION_DATE <= as.Date('2015-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[65]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-05-01') & OBSERVATION_DATE <= as.Date('2015-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[66]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-06-01') & OBSERVATION_DATE <= as.Date('2015-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[67]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-07-01') & OBSERVATION_DATE <= as.Date('2015-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[68]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-08-01') & OBSERVATION_DATE <= as.Date('2015-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[69]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-09-01') & OBSERVATION_DATE <= as.Date('2015-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[70]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-10-01') & OBSERVATION_DATE <= as.Date('2015-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[71]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-11-01') & OBSERVATION_DATE <= as.Date('2015-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[72]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-12-01') & OBSERVATION_DATE <= as.Date('2015-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[73]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-01-01') & OBSERVATION_DATE <= as.Date('2016-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[74]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-02-01') & OBSERVATION_DATE <= as.Date('2016-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[75]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-03-01') & OBSERVATION_DATE <= as.Date('2016-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[76]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-04-01') & OBSERVATION_DATE <= as.Date('2016-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[77]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-05-01') & OBSERVATION_DATE <= as.Date('2016-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[78]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-06-01') & OBSERVATION_DATE <= as.Date('2016-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[79]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-07-01') & OBSERVATION_DATE <= as.Date('2016-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[80]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-08-01') & OBSERVATION_DATE <= as.Date('2016-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[81]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-09-01') & OBSERVATION_DATE <= as.Date('2016-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[82]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-10-01') & OBSERVATION_DATE <= as.Date('2016-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[83]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-11-01') & OBSERVATION_DATE <= as.Date('2016-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[84]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-12-01') & OBSERVATION_DATE <= as.Date('2016-12-31'))$WIND_DIRECTION_ANGLE
list_series_2[[85]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-01-01') & OBSERVATION_DATE <= as.Date('2017-01-31'))$WIND_DIRECTION_ANGLE
list_series_2[[86]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-02-01') & OBSERVATION_DATE <= as.Date('2017-02-28'))$WIND_DIRECTION_ANGLE
list_series_2[[87]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-03-01') & OBSERVATION_DATE <= as.Date('2017-03-31'))$WIND_DIRECTION_ANGLE
list_series_2[[88]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-04-01') & OBSERVATION_DATE <= as.Date('2017-04-30'))$WIND_DIRECTION_ANGLE
list_series_2[[89]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-05-01') & OBSERVATION_DATE <= as.Date('2017-05-31'))$WIND_DIRECTION_ANGLE
list_series_2[[90]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-06-01') & OBSERVATION_DATE <= as.Date('2017-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[91]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-07-01') & OBSERVATION_DATE <= as.Date('2017-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[92]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-08-01') & OBSERVATION_DATE <= as.Date('2017-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[93]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-09-01') & OBSERVATION_DATE <= as.Date('2017-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[94]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-10-01') & OBSERVATION_DATE <= as.Date('2017-10-31'))$WIND_DIRECTION_ANGLE
list_series_2[[95]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-11-01') & OBSERVATION_DATE <= as.Date('2017-11-30'))$WIND_DIRECTION_ANGLE
list_series_2[[96]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-12-01') & OBSERVATION_DATE <= as.Date('2017-12-31'))$WIND_DIRECTION_ANGLE


# Selecting the time of other particular station

wind_data_s3 <- subset(wind_data_clean, STATION_NAME == 'DAMMAM (KING FAHD INT. AIRPORT)')

list_series_3[[1]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-01-01') & OBSERVATION_DATE <= as.Date('2010-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[2]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-02-01') & OBSERVATION_DATE <= as.Date('2010-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[3]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-03-01') & OBSERVATION_DATE <= as.Date('2010-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[4]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-04-01') & OBSERVATION_DATE <= as.Date('2010-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[5]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-05-01') & OBSERVATION_DATE <= as.Date('2010-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[6]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-06-01') & OBSERVATION_DATE <= as.Date('2010-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[7]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-07-01') & OBSERVATION_DATE <= as.Date('2010-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[8]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-08-01') & OBSERVATION_DATE <= as.Date('2010-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[9]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-09-01') & OBSERVATION_DATE <= as.Date('2010-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[10]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-10-01') & OBSERVATION_DATE <= as.Date('2010-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[11]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-11-01') & OBSERVATION_DATE <= as.Date('2010-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[12]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2010-12-01') & OBSERVATION_DATE <= as.Date('2010-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[13]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-01-01') & OBSERVATION_DATE <= as.Date('2011-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[14]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-02-01') & OBSERVATION_DATE <= as.Date('2011-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[15]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-03-01') & OBSERVATION_DATE <= as.Date('2011-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[16]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-04-01') & OBSERVATION_DATE <= as.Date('2011-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[17]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-05-01') & OBSERVATION_DATE <= as.Date('2011-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[18]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-06-01') & OBSERVATION_DATE <= as.Date('2011-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[19]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-07-01') & OBSERVATION_DATE <= as.Date('2011-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[20]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-08-01') & OBSERVATION_DATE <= as.Date('2011-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[21]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-09-01') & OBSERVATION_DATE <= as.Date('2011-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[22]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-10-01') & OBSERVATION_DATE <= as.Date('2011-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[23]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-11-01') & OBSERVATION_DATE <= as.Date('2011-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[24]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2011-12-01') & OBSERVATION_DATE <= as.Date('2011-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[25]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-01-01') & OBSERVATION_DATE <= as.Date('2012-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[26]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-02-01') & OBSERVATION_DATE <= as.Date('2012-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[27]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-03-01') & OBSERVATION_DATE <= as.Date('2012-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[28]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-04-01') & OBSERVATION_DATE <= as.Date('2012-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[29]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-05-01') & OBSERVATION_DATE <= as.Date('2012-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[30]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-06-01') & OBSERVATION_DATE <= as.Date('2012-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[31]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-07-01') & OBSERVATION_DATE <= as.Date('2012-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[32]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-08-01') & OBSERVATION_DATE <= as.Date('2012-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[33]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-09-01') & OBSERVATION_DATE <= as.Date('2012-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[34]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-10-01') & OBSERVATION_DATE <= as.Date('2012-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[35]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-11-01') & OBSERVATION_DATE <= as.Date('2012-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[36]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2012-12-01') & OBSERVATION_DATE <= as.Date('2012-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[37]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-01-01') & OBSERVATION_DATE <= as.Date('2013-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[38]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-02-01') & OBSERVATION_DATE <= as.Date('2013-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[39]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-03-01') & OBSERVATION_DATE <= as.Date('2013-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[40]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-04-01') & OBSERVATION_DATE <= as.Date('2013-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[41]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-05-01') & OBSERVATION_DATE <= as.Date('2013-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[42]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-06-01') & OBSERVATION_DATE <= as.Date('2013-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[43]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-07-01') & OBSERVATION_DATE <= as.Date('2013-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[44]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-08-01') & OBSERVATION_DATE <= as.Date('2013-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[45]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-09-01') & OBSERVATION_DATE <= as.Date('2013-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[46]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-10-01') & OBSERVATION_DATE <= as.Date('2013-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[47]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-11-01') & OBSERVATION_DATE <= as.Date('2013-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[48]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2013-12-01') & OBSERVATION_DATE <= as.Date('2013-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[49]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-01-01') & OBSERVATION_DATE <= as.Date('2014-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[50]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-02-01') & OBSERVATION_DATE <= as.Date('2014-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[51]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-03-01') & OBSERVATION_DATE <= as.Date('2014-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[52]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-04-01') & OBSERVATION_DATE <= as.Date('2014-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[53]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-05-01') & OBSERVATION_DATE <= as.Date('2014-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[54]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-06-01') & OBSERVATION_DATE <= as.Date('2014-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[55]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-07-01') & OBSERVATION_DATE <= as.Date('2014-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[56]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-08-01') & OBSERVATION_DATE <= as.Date('2014-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[57]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-09-01') & OBSERVATION_DATE <= as.Date('2014-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[58]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-10-01') & OBSERVATION_DATE <= as.Date('2014-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[59]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-11-01') & OBSERVATION_DATE <= as.Date('2014-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[60]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2014-12-01') & OBSERVATION_DATE <= as.Date('2014-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[61]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-01-01') & OBSERVATION_DATE <= as.Date('2015-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[62]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-02-01') & OBSERVATION_DATE <= as.Date('2015-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[63]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-03-01') & OBSERVATION_DATE <= as.Date('2015-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[64]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-04-01') & OBSERVATION_DATE <= as.Date('2015-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[65]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-05-01') & OBSERVATION_DATE <= as.Date('2015-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[66]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-06-01') & OBSERVATION_DATE <= as.Date('2015-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[67]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-07-01') & OBSERVATION_DATE <= as.Date('2015-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[68]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-08-01') & OBSERVATION_DATE <= as.Date('2015-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[69]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-09-01') & OBSERVATION_DATE <= as.Date('2015-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[70]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-10-01') & OBSERVATION_DATE <= as.Date('2015-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[71]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-11-01') & OBSERVATION_DATE <= as.Date('2015-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[72]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2015-12-01') & OBSERVATION_DATE <= as.Date('2015-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[73]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-01-01') & OBSERVATION_DATE <= as.Date('2016-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[74]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-02-01') & OBSERVATION_DATE <= as.Date('2016-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[75]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-03-01') & OBSERVATION_DATE <= as.Date('2016-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[76]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-04-01') & OBSERVATION_DATE <= as.Date('2016-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[77]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-05-01') & OBSERVATION_DATE <= as.Date('2016-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[78]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-06-01') & OBSERVATION_DATE <= as.Date('2016-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[79]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-07-01') & OBSERVATION_DATE <= as.Date('2016-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[80]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-08-01') & OBSERVATION_DATE <= as.Date('2016-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[81]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-09-01') & OBSERVATION_DATE <= as.Date('2016-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[82]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-10-01') & OBSERVATION_DATE <= as.Date('2016-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[83]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-11-01') & OBSERVATION_DATE <= as.Date('2016-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[84]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2016-12-01') & OBSERVATION_DATE <= as.Date('2016-12-31'))$WIND_DIRECTION_ANGLE
list_series_3[[85]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-01-01') & OBSERVATION_DATE <= as.Date('2017-01-31'))$WIND_DIRECTION_ANGLE
list_series_3[[86]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-02-01') & OBSERVATION_DATE <= as.Date('2017-02-28'))$WIND_DIRECTION_ANGLE
list_series_3[[87]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-03-01') & OBSERVATION_DATE <= as.Date('2017-03-31'))$WIND_DIRECTION_ANGLE
list_series_3[[88]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-04-01') & OBSERVATION_DATE <= as.Date('2017-04-30'))$WIND_DIRECTION_ANGLE
list_series_3[[89]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-05-01') & OBSERVATION_DATE <= as.Date('2017-05-31'))$WIND_DIRECTION_ANGLE
list_series_3[[90]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-06-01') & OBSERVATION_DATE <= as.Date('2017-06-30'))$WIND_DIRECTION_ANGLE
list_series_3[[91]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-07-01') & OBSERVATION_DATE <= as.Date('2017-07-31'))$WIND_DIRECTION_ANGLE
list_series_3[[92]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-08-01') & OBSERVATION_DATE <= as.Date('2017-08-31'))$WIND_DIRECTION_ANGLE
list_series_3[[93]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-09-01') & OBSERVATION_DATE <= as.Date('2017-09-30'))$WIND_DIRECTION_ANGLE
list_series_3[[94]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-10-01') & OBSERVATION_DATE <= as.Date('2017-10-31'))$WIND_DIRECTION_ANGLE
list_series_3[[95]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-11-01') & OBSERVATION_DATE <= as.Date('2017-11-30'))$WIND_DIRECTION_ANGLE
list_series_3[[96]] <- (pi/180) * subset(wind_data_s3, OBSERVATION_DATE >= as.Date('2017-12-01') & OBSERVATION_DATE <= as.Date('2017-12-31'))$WIND_DIRECTION_ANGLE


list_series <- c(list_series_1, list_series_2)

vector_lengths <- numeric()

for (j in 1 : 192) {
  
  vector_lengths[j] <- length(list_series[[j]])
  
}

for (i in 1 : 192) {
  
  list_series[[i]][list_series[[i]] > 10] <- 0
  
}

max_lag <- 20
alpha <- 0.05
corrected_alpha <- alpha

# Selection of the optimal set of lags

lags <- numeric()

for (j in 1 : 192) {
  
  auxiliary <- test_lags_circular(series = list_series[[j]], max_lag = max_lag,
                                  alpha = corrected_alpha)
  index <- ifelse(length(which.max(auxiliary$correlation)) == 0, 0, 
                  which.max(abs(auxiliary$correlation)))
  lags[[j]] <- ifelse(length(auxiliary$lags) >= 1, auxiliary$lags[index],
                      0)
  
  print(j)
  
}

# Selection of r and m

features_q <-list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
vector_r <- seq(0.5, 1.5, 0.1)
l_r <- length(vector_r)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  for (j2 in vector_r) {
    
    for (i in 1 : 192) {
      
      features_q[[i]] <- autocorrelations_q(list_series[[i]], levels = levels, lags = lags, radius = j2, quantile_function = quantile_function)
      l_l <- length(which(is.na(features_q[[i]])))
      features_q[[i]][is.na(features_q[[i]])] <- runif(l_l, 0, 2 * pi)
      
    }
    
    matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q)
    dis_matrix_q <- as.matrix(proxy::dist(matrix_features_q)^2)
    
    clustering_q <- FKM((matrix_features_q), k = 2, m = j1)
    xie_beni[k] <- XB(matrix_features_q, 
                      clustering_q$U, clustering_q$H, m = 2)
    
    k <- k + 1
    print(j1)
    
  }
  
}

which.min(xie_beni)
xie_beni_matrix <- matrix(xie_beni, nrow = l_m, ncol = l_r, byrow = T)

plot_2d_scaling <- function (distance_matrix, cluster_labels = NULL, title = "") 
{
  mds <- stats::cmdscale(distance_matrix, 2, list. = TRUE)
  gof <- mds$GOF[1]
  if (is.null(cluster_labels)) {
    df <- data.frame(cbind(mds$points))
    X1 <- df$X1
    X2 <- df$X2
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2)) + 
      ggplot2::geom_point(size = 2.5, col = "blue") + ggplot2::xlab("Coordinate 1") + 
      ggplot2::ylab("Coordinate 2") + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), 
                                                     axis.title = ggplot2::element_text(size = 17), plot.title = ggplot2::element_text(hjust = 0.5, 
                                                                                                                                       size = 18), legend.position = "bottom", legend.title = ggplot2::element_blank(), 
                                                     legend.text = ggplot2::element_text(size = 12)) + 
      ggplot2::ggtitle(title)
    return_list <- list(plot = plot, coordinates_2d = mds$points, 
                        gof = gof)
    return(return_list)
  }
  else {
    n_labels <- length(unique(cluster_labels))
    vector_labels <- numeric(n_labels)
    
    vector_labels[1] <- 'Abha'
    vector_labels[2] <- 'Makkah'
    vector_labels[3] <- 'Dammam'
    
    df <- data.frame(cbind(mds$points), factor(cluster_labels))
    colnames(df)[3] <- "series"
    series <- df$series
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2, 
                                             col = series)) + ggplot2::geom_point(size = 2.5) + 
      ggplot2::scale_color_manual(labels = vector_labels, values = c('blue', 'forestgreen', 'orangered')) + 
      ggplot2::xlab("Coordinate 1") + ggplot2::ylab("Coordinate 2") + 
      ggplot2::theme(axis.text = ggplot2::element_text(size = 15), 
                     axis.title = ggplot2::element_text(size = 17), 
                     plot.title = ggplot2::element_text(hjust = 0.5, 
                                                        size = 18), legend.position = "bottom", legend.title = ggplot2::element_blank(), 
                     legend.text = ggplot2::element_text(size = 12)) + 
      ggplot2::ggtitle(title)
    return_list <- list(plot = plot, coordinates_2d = mds$points, 
                        gof = gof)
    return(return_list)
  }
}



# ground_truth <- c(rep(c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1), 8), rep(c(3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 3, 3), 8), rep(c(5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5), 8))
ground_truth <- c(rep(1, 96), rep(2, 96))
lags <- c(1)
levels <- c(0.1, 0.5, 0.9)
radius <- 1
features_q <- list()

for (i in 1 : 192) {
  
  features_q[[i]] <- autocorrelations_q(list_series[[i]], levels = levels, lags = lags, radius = radius, quantile_function = quantile_function)
  features_q[[i]][is.na(features_q[[i]])] <- 0
  
}

matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q)
dis_matrix_q <- as.matrix(proxy::dist(matrix_features_q)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_q, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(123)
clustering_q <- fuzzy_c_medoids(matrix_features_q, C = 2, m = 2, dis = sqe)
# clustering_q$U
ARI.F(ground_truth, clustering_q$U)
JACCARD.F(ground_truth, clustering_q$U)


# d_{FL}


# Selection of r and m

features_fl <-list()
set.seed(12345)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  
  for (i in 1 : 192) {
    
    features_fl[[i]] <- autocorrelations_fl(list_series[[i]], lags = lags)
    l_l <- length(which(is.na(features_fl[[i]])))
    features_fl[[i]][is.na(features_fl[[i]])] <- runif(l_l, 0, 2 * pi)
    
  }
  
  matrix_features_fl <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_fl)
  matrix_features_fl <- cbind(matrix_features_fl, rep(0, 192))
  dis_matrix_fl <- as.matrix(proxy::dist(matrix_features_fl)^2)
  
  clustering_fl <- FKM(matrix_features_fl, k = 2, m = j1)
  xie_beni[k] <- XB(matrix_features_fl, 
                    clustering_fl$U, clustering_fl$H, m = 2)
  
  k <- k + 1
  print(j1)
  
  
  
}

which.min(xie_beni)


lags <- c(1)
features_fl <- list()

for (i in 1 : 192) {
  
  features_fl[[i]] <- autocorrelations_fl(list_series[[i]], lags = lags)
  l_l <- length(which(is.na(features_fl[[i]])))
  features_fl[[i]][is.na(features_fl[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_fl <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_fl)
matrix_features_fl <- cbind(matrix_features_fl, rep(0, 192))
dis_matrix_fl <- as.matrix(proxy::dist(matrix_features_fl)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_fl, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(123)
clustering_fl <- fuzzy_c_medoids(matrix_features_fl, C = 2, m = 1.9, dis = sqe)
clustering_fl$U
ARI.F(ground_truth, clustering_fl$U)
JACCARD.F(ground_truth, clustering_fl$U)



# d_{J}


# Selection of r and m

features_j <-list()
set.seed(123456)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  
  for (i in 1 : 192) {
    
    features_j[[i]] <- autocorrelations_j(list_series[[i]], lags = lags)
    l_l <- length(which(is.na(features_j[[i]])))
    features_j[[i]][is.na(features_j[[i]])] <- runif(l_l, 0, 2 * pi)
    
  }
  
  matrix_features_j <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_j)
  matrix_features_j <- cbind(matrix_features_j, rep(0, 192))
  dis_matrix_j <- as.matrix(proxy::dist(matrix_features_j)^2)
  
  clustering_j <- FKM(matrix_features_j, k = 2, m = j1)
  xie_beni[k] <- XB(matrix_features_j, 
                    clustering_j$U, clustering_j$H, m = 2)
  
  k <- k + 1
  print(j1)
  
  
  
}

which.min(xie_beni)


lags <- c(1)
features_j <- list()

for (i in 1 : 192) {
  
  features_j[[i]] <- autocorrelations_j(list_series[[i]], lags = lags)
  l_l <- length(which(is.na(features_j[[i]])))
  features_j[[i]][is.na(features_j[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_j <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_j)
matrix_features_j <- cbind(matrix_features_j, rep(0, 192))
dis_matrix_j <- as.matrix(proxy::dist(matrix_features_j)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_j, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(123)
clustering_j <- fuzzy_c_medoids(matrix_features_j, C = 2, m = 2, dis = sqe)
clustering_j$U
ARI.F(ground_truth, clustering_j$U)
JACCARD.F(ground_truth, clustering_j$U)


# d_{QA}


# Selection of m

features_noncircular <- list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  for (i in 1 : 192) {
    
    features_noncircular[[i]] <- autocorrelations_noncircular(list_series[[i]], levels = levels, lags = lags)
    l_l <- length(which(is.na(features_noncircular[[i]])))
    features_noncircular[[i]][is.na(features_noncircular[[i]])] <- runif(l_l, 0, 2 * pi)
    
  }
  
  matrix_features_noncircular <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_noncircular)
  dis_matrix_noncircular <- as.matrix(proxy::dist(matrix_features_noncircular)^2)
  
  clustering_noncircular <- FKM((matrix_features_noncircular), k = 2, m = j1)
  xie_beni[k] <- XB(matrix_features_noncircular, 
                    clustering_noncircular$U, clustering_noncircular$H, m = 2)
  
  k <- k + 1
  print(j1)
  
}

which.min(xie_beni)


lags <- c(1)
features_noncircular <- list()

for (i in 1 : 192) {
  
  features_noncircular[[i]] <- autocorrelations_noncircular(list_series[[i]], lags = lags, levels = levels)
  l_l <- length(which(is.na(features_noncircular[[i]])))
  features_noncircular[[i]][is.na(features_noncircular[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_noncircular <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_noncircular)
dis_matrix_noncircular <- as.matrix(proxy::dist(matrix_features_noncircular)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_noncircular, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(123)
clustering_noncircular <- fuzzy_c_medoids(matrix_features_noncircular, C = 2, m = 1.9, dis = sqe)
clustering_noncircular$U
ARI.F(ground_truth, clustering_noncircular$U)
JACCARD.F(ground_truth, clustering_noncircular$U)

