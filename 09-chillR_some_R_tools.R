require(chillR)
require(tidyverse)
#require(kableExtra)

# demonstration of tidyverse functions
library(tidyverse)

#tibbles
dat <- data.frame(a=c(1,2,3),b=c(4,5,6))
d <- as_tibble(dat)
d

d %>% sum()


library(chillR)

KAw<-as_tibble(KA_weather[1:10,])
KAw

# pivot_longer
KAwlong <- KAw %>% pivot_longer(cols=Tmax:Tmin)
KAwlong

# pivot_wider
KAwwide <- KAwlong %>% pivot_wider(names_from=name,values_from=value) 
KAwwide <- KAwlong %>% pivot_wider() 
KAwwide

KAw[,c("Year","Month","Day")]

# select
KAw %>% select(c(Month, Day, Tmax))
KAw %>% select(c(Month:Tmax))
KAw %>% select(c(Month:Tmax)) %>% sum()

# filter
KAw[which(KAw$Tmax>10),]

KAw %>% filter(Tmax>10)

# mutate
KAw_K <- KAw %>% mutate(Tmax_K = Tmax + 273.15, Tmin_K = Tmin + 273.15)
KAw_K

KAw_K <- KAw_K %>% mutate(Tmin_K = NULL, Tmax_K = NULL)

KAw %>% mutate(Tmin = Tmin + 273.15, Tmax = Tmax + 273.15)

# arrange
KAw %>% arrange(Tmax, Tmin)

KAw %>% arrange(desc(Tmax), Tmin)

KA_weather %>% arrange(desc(Tmax), Tmin)

# LOOPS
# for loops

# repeat the same code three times
for (i in 1:3) print("Hello")

# change variable within loop
#addition <- 1


for (i in 1:3)
{
  if(i==1) addition<-1
  addition <- addition + 1
  print(addition)
}

# use the counter to modify a variable
addition <- 1

for (i in 1:3)
{
  if(i==1) addition<-1
  addition <- addition + i
  print(addition)
}

# use the counter in a more creative way
names <- c("Paul", "Mary", "John")

for (i in 1:3)
{
  print(paste("Hello", names[i]))
}

# use a vector of character strings to define iterations
for (i in c("Paul", "Mary", "John"))
{
  print(paste("Hello", i))
}



# while loops
cond <- 5

while (cond<0)
{
  print(cond)
  cond <- cond - 1
}

# the apply function family

func <- function(x)  x + 1

1:5

sa<-sapply(1:5, func)

for (i in 1:1000000) print(i + 1)

sapply(1:1000000, func)

sa<-sapply(list(1:5), func)



lapply(1:5, func)



la<-lapply(list(1:5), func)



mat <- matrix(c(1,1,1,2,2,2,3,3,3),c(3,3))

mat

apply(mat, MARGIN=1, sum) # adding up all the data in each row

apply(mat, MARGIN=2, sum) # adding up all the data in each column

apply(mat, MARGIN=2, mean)


