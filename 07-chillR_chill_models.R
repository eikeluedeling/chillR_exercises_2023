require(tidyr)
require(reshape2)
require(dplyr)
require(kableExtra)

library(chillR)
Chilling_Hours


Chilling_Hours(Winters_hours_gaps$Temp)[1:100]

Utah_Model

data.frame(lower = c(-1000, 
                1.4, 2.4, 9.1, 12.4, 15.9, 18), upper = c(1.4, 2.4, 9.1, 
                12.4, 15.9, 18, 1000), weight = c(0, 0.5, 1, 0.5, 0, -0.5, 
                -1))

step_model

Utah_Model(Winters_hours_gaps$Temp)[1:100]

df<-data.frame(
  lower= c(-1000, 1, 2, 3, 4, 5,    6),
  upper= c(    1, 2, 3, 4, 5, 6, 1000),
  weight=c(    0, 1, 2, 3, 2, 1,    0))

kable(df) %>%
  kable_styling("striped", position = "left", font_size = 10)

custom<-function(x) step_model(HourTemp=x,df)

custom(Winters_hours_gaps$Temp)[1:100]

Dynamic_Model

Dynamic_Model(Winters_hours_gaps$Temp)[1:300]

?chilling

Winters_JDay<-make_JDay(Winters_hours_gaps)
output<-chilling(Winters_JDay,Start_JDay = 90, End_JDay = 100)

output<-chilling(make_JDay(Winters_hours_gaps),Start_JDay = 90, End_JDay = 100)

chilling(stack_hourly_temps(fix_weather(KA_weather[which(KA_weather$Year > 2006), ]),
                            latitude = 50.4))

kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)


output<-tempResponse(make_JDay(Winters_hours_gaps),
                     Start_JDay = 90, End_JDay = 100)

output<-tempResponse(make_JDay(Winters_hours_gaps),
                     Start_JDay = 90, End_JDay = 100,
                     models=list(Chill_Portions = Dynamic_Model,
                                 GDH = GDH))

output<-tempResponse(make_JDay(Winters_hours_gaps),
                     Start_JDay = 90, End_JDay = 100,
                     models=list(Chill_Portions = Dynamic_Model,
                                 Our_model = custom,
                                 GDH = GDH))

kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)

