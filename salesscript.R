baskets <- sqldf("SELECT * FROM business WHERE Product_Type = 'Basket'")
View(baskets)

sum(baskets$Gross_Sales)

sum(baskets$Returns)

baskets_rate_of_return <- 4439.69/143815.5
baskets_rate_of_return

sum(baskets$Total_Net_Sales)
[1] 134791.4
sum(baskets$Net_Quantity)
[1] 1461
baskets_revenue <- 134791.4 * 1461
baskets_revenue
[1] 196930235

total_sales <- business_retailsales2$Total_Sales

#plotting business's sales
myts <- ggplot(business_retailsales2, aes(x=Month, y=Total_Sales)) + geom_line()
myts <- myts + ylab("Sales") + labs(title="Total Sales by Year") + theme_classic()

as.Date(business_retailsales2$Month, format="%m/%d")

sales_ts <- ts(total_sales, start=2017,end=2019,frequency=4)

#consolidating the data so that each row is a unique product type
df <- sqldf(
            "SELECT 
            Product_Type,
            SUM(Net_Quantity) AS Net_Quantity,
            SUM(Gross_Sales) AS Gross_Sales,
            SUM(Returns),
            SUM(Total_Net_Sales) AS Total_Net_Sales 
            FROM business GROUP BY Product_Type"
            )

#finding the top 6 selling product types
df_6 <- sqldf(
            "SELECT Product_Type,
            SUM(Net_Quantity) AS Net_Quantity,
            SUM(Gross_Sales) AS Gross_Sales,
            SUM(Returns) AS Returns,
            SUM(Total_Net_Sales) AS Total_Net_Sales 
            FROM business 
            GROUP BY Product_Type 
            ORDER BY Total_Net_Sales 
            DESC LIMIT 6"
            )

#using a bar plot to visuaize the top 6 product types by sales volume
b <- ggplot(df_5, aes(Product_Type,Total_Net_Sales)) + geom_bar(stat="identity",color="#FF6666") + xlab("Product Type") + ylab("Total Sales") + theme_minimal()

#creating table of the rates that each product types were returned
rates <- sqldf("CREATE TABLE rates 
               (Product_Type VARCHAR(50),
                Rate_of_Return FLOAT
                )
               ")

#inserting rates of return into new ly created table
rates <- sqldf("INSERT INTO rates (Product_Type,Rate_of_Return)
                VALUES ('Baskets',0.31),
                       ('Christmas',0.43),
                       ('Home Decor',0.016),
                       ('Jewelry',0.0164),
                       ('Arts & Sculpture',0.032),
                       ('Kitchen',0.02)
               ")

#selecting all months where total sales were greater than 11,000
holiday <- sqldf("SELECT Date,Total_Orders,Total_Sales 
                 FROM business_retailsales2 
                 WHERE Month IN ('November','December') 
                 ORDER BY Total_Sales DESC
                 ")

non_holiday <- sqldf("SELECT Date,Total_Orders,Total_Sales 
                     FROM business_retailsales2 
                     WHERE Month NOT IN ('November','December') 
                     ORDER BY Total_Sales DESC
                     ")

sum(holiday$Total_Sales) / 6
[1] 19142.1
sum(non_holiday$Total_Sales) / 30
[1] 8937.109

#creating a time series of the business's sales
y <- ts(business_retailsales2[,8],start=c(2017,1),end=c(2019,10), frequency=12) 

#forecast library
library(fpp2)
fit <- snaive(DY)
summary(fit)
checkresiduals(fit)

fit_ets <- ets(y)
print(summary(fit_ets))

fit_arima <- auto.arima(y,d=1,D=1,stepwise=FALSE,approximation=FALSE,trace=TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
  
#forecasting future sales
fcst <- forecast(fit_arima,h=38)
autoplot(fcst)
fcst 

f <- ts(business_retailsales2[,10],start=c(2017,1),end=c(2019,10),frequency=12)

aov <- sum(business_retailsales2$Total_Sales) / sum(business_retailsales2$Total_Orders)




  
  
  
