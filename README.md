# wholesaleprices
These are some basic R queries meant to gain insights into the wholesale electricity market across the country
#Install the necssary R packages - dplyr, magrittr, lubridate, readr, readxl
install.packages("ggplot2")
#Import Energy Information Administration's 2017 spreadsheet of wholesale electricity prices
wholesale_prices_2017 <- read.csv("/Users/stevenshoemaker/Desktop/ice_electric-2017final.csv")
#Change the "Trade" Column into a usable date class using lubridate, create new "Trade.Month" column
wholesale_prices_2017_lubridated <- wholesale_prices_2017 %>% mutate(Trade.date = mdy(Trade.date))%>% mutate(Trade.month = months(Trade.date))
#Determine which month has the highest volume of trades nationwide
wholesale_prices_2017_lubridated %>% group_by(Trade.month) %>% summarize(monthly.total.trades = sum(Number.of.trades)) %>% arrange(desc(monthly.total.trades)) 
#Turns out to be June, with 4,634 total trades. This is interesting because it is a summer month with increasing more solar on the grid
#The next question is: What is the region that has the highest average difference between the High price per MWh and the low price (spread)?
wholesale_prices_2017_lubridated %>% mutate(Spread = High.price...MWh - Low.price...MWh) %>% group_by(Price.hub) %>% summarize(average_spread = mean(Spread)) %>% arrange(desc(average_spread)) 
#Mid C Peak, which is the Price hub that represents the Northwest, had the largest price spread
#The next question: Can we visual the relationshop between the month and the number of counterparties?
wholesale_prices_2017_lubridated %>% mutate(Trade.month = as.factor(Trade.month)) %>% group_by(Trade.month) %>% summarize(average_parties = mean(Number.of.counterparties)) %>% ggplot(aes(x =Trade.month, y = average_parties )) + geom_line() + geom_point()
#July has the largest average number of counterparties and June has the highest number of trades. Clearly there is more action in the summer
#The next line visuals the average weighted price by region
wholesale_prices_2017_lubridated %>% group_by(Price.hub) %>% summarize (Average_price = mean(Wtd.avg.price...MWh)) %>% ggplot(aes(x = Price.hub, y = Average_price)) + geom_col()
#There were more than 5,000 total trades in the Northwest in the summer months, and that trade volume differential holds throughout the year
#The Northwest also has the highest "spread" and lowest weighted average price (around $25). Preliminary observations indicated that regions with
#more trades and more volume also have lower prices. However, there are many other factors at play, including cheap hydropower available 
