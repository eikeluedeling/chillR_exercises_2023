library(chillR)

?"chillR-package"

?chilling

Winters_hours_gaps[1:5, 6]

Winters_hours_gaps[, "Temp"]

c(1, 2, 3, 4, 5)

c("A", "Hi", "rabbit")

Winters_hours_gaps[,
                   c("Year",
                     "Month",
                     "Day",
                     "Hour",
                     "Temp")]

hourtemps <- Winters_hours_gaps[,c("Year",
                                   "Month",
                                   "Day",
                                   "Hour",
                                   "Temp")]

hourtemps

head(hourtemps)
tail(hourtemps)

hourtemps[, "Temp"]
hourtemps$Temp

hourtemps[, "MyColumn"]<-NA

hourtemps <- hourtemps[, c("Year",
                           "Month",
                           "Day",
                           "Hour",
                           "Temp")]

0 < 1
0 > 1
0 == 1
0 <= 1

a <- 5

a > 3

a <- c(-1, -2, 0, 1, 2, 3, 4, 5, 8, 1, 4, 7)

a > 0

a <= 7.2

a > 0 & a <= 7.2

hourtemps$Temp > 0 & hourtemps$Temp <= 7.2

hourtemps[,"CH"] <- hourtemps$Temp > 0 &
  hourtemps$Temp <= 7.2

head(hourtemps)

sum(hourtemps$CH)

sum(hourtemps$CH[100:200])

hourtemps[hourtemps$Year==2008 & hourtemps$Month==3 &
            hourtemps$Day==10 & hourtemps$Hour==0, ] 
hourtemps[hourtemps$Year==2008 & hourtemps$Month==4 &
            hourtemps$Day==10 & hourtemps$Hour==0, ]

Start_row <- which(hourtemps$Year == 2008 & hourtemps$Month == 3 &
                     hourtemps$Day == 10 & hourtemps$Hour == 0)
End_row <- which(hourtemps$Year == 2008 & hourtemps$Month == 4 &
                   hourtemps$Day == 10 & hourtemps$Hour == 0)

sum(hourtemps$CH[Start_row:End_row])

CH <- function(value) plot(value)

CH(c(7, 6, 4))

CH <- function()
{
  Start_row <- which(hourtemps$Year == 2008 & hourtemps$Month == 3 &
                       hourtemps$Day == 10 & hourtemps$Hour == 0)
  End_row <- which(hourtemps$Year == 2008 & hourtemps$Month == 4 &
                     hourtemps$Day == 10 & hourtemps$Hour == 0)
  
  sum(hourtemps$CH[Start_row:End_row])
}

CH()


CH <- function(hourtemps,
               Start_Year,
               Start_Month,
               Start_Day,
               Start_Hour,
               End_Year,
               End_Month,
               End_Day,
               End_Hour)
{
  hourtemps[,"CH"] <- hourtemps$Temp > 0 &  hourtemps$Temp <= 7.2
  Start_row<-which(hourtemps$Year == Start_Year &
                     hourtemps$Month == Start_Month &
                     hourtemps$Day == Start_Day &
                     hourtemps$Hour == Start_Hour)
  End_row<-which(hourtemps$Year == End_Year &
                   hourtemps$Month == End_Month &
                   hourtemps$Day == End_Day &
                   hourtemps$Hour == End_Hour)
  
  sum(hourtemps$CH[Start_row:End_row])
}

CH(hourtemps,
   Start_Year = 2008,
   Start_Month = 3,
   Start_Day = 20,
   Start_Hour = 0,
   End_Year = 2008,
   End_Month = 5,
   End_Day = 1,
   End_Hour = 12)

CH(hourtemps = Winters_hours_gaps,
   Start_Year = 2008,
   Start_Month = 3,
   Start_Day = 20,
   Start_Hour = 0,
   End_Year = 2008,
   End_Month = 5,
   End_Day = 1,
   End_Hour = 12)


#### we only got until here
# next time we'll still add the temperature check to the function and make the
# date part a bit more convenient

CH_flex <- function(hourtemps,
               Start_Year,
               Start_Month,
               Start_Day,
               Start_Hour,
               End_Year,
               End_Month,
               End_Day,
               End_Hour,
               lower_threshold = 0,
               upper_threshold = 7.2)
{
  hourtemps[,"CH"] <- hourtemps$Temp > lower_threshold &
    hourtemps$Temp <= upper_threshold
  Start_row <- which(hourtemps$Year == Start_Year &
                       hourtemps$Month == Start_Month & 
                       hourtemps$Day == Start_Day &
                       hourtemps$Hour == Start_Hour)
  End_row <- which(hourtemps$Year == End_Year & 
                     hourtemps$Month == End_Month &
                     hourtemps$Day == End_Day &
                     hourtemps$Hour == End_Hour)
  
  sum(hourtemps$CH[Start_row:End_row])
}

CH_flex(hourtemps = Winters_hours_gaps,
        Start_Year = 2008,
        Start_Month = 3,
        Start_Day = 20,
        Start_Hour = 0,
        End_Year = 2008,
        End_Month = 5,
        End_Day = 1,
        End_Hour = 12)

CH_flex(hourtemps = Winters_hours_gaps,
        Start_Year = 2008,
        Start_Month = 3,
        Start_Day = 20,
        Start_Hour = 0,
        End_Year = 2008,
        End_Month = 5,
        End_Day = 1,
        End_Hour = 12,
        lower_threshold = 25,
        upper_threshold = 99)


2008032000

head(Winters_hours_gaps)

CH_flex_date <- function(hourtemps,
                         Start_YEARMODAHO,
                         End_YEARMODAHO,
                         lower_threshold = 0,
                         upper_threshold = 7.2)
{
  hourtemps[,"CH"] <- hourtemps$Temp > lower_threshold &
                      hourtemps$Temp <= upper_threshold
  hourtemps[,"YEARMODAHO"] <- hourtemps$Year  * 1000000 +
                              hourtemps$Month *   10000 +
                              hourtemps$Day   *     100 +
                              hourtemps$Hour
  
  if(!Start_YEARMODAHO %in% hourtemps[,
                                      "YEARMODAHO"]) stop("Start date not in dataset")
  if(!End_YEARMODAHO %in% hourtemps[,
                                    "YEARMODAHO"]) stop("End date not in dataset")
  
  Start_row <- which(hourtemps$YEARMODAHO == Start_YEARMODAHO)
  End_row <- which(hourtemps$YEARMODAHO == End_YEARMODAHO)
  
  sum(hourtemps$CH[Start_row:End_row])
}

CH_flex_date(Winters_hours_gaps,
             2008031410,
             2008041006)
CH_flex_date(Winters_hours_gaps,
             Start_YEARMODAHO = 2008031401,
             End_YEARMODAHO = 20080121006,
             lower_threshold = 25,
             upper_threshold = 99)

