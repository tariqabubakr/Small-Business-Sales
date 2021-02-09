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


myts <- ggplot(business_retailsales2, aes(x=Month, y=Total_Sales)) + geom_line()
myts <- myts + ylab("Sales") + labs(title="Total Sales by Year") + theme_classic()

as.Date(business_retailsales2$Month, format="%m/%d")

sales_ts <- ts(total_sales, start=2017,end=2019,frequency=4)

df <- sqldf(
            "SELECT 
            Product_Type,
            SUM(Net_Quantity) AS Net_Quantity,
            SUM(Gross_Sales) AS Gross_Sales,
            SUM(Returns),
            SUM(Total_Net_Sales) AS Total_Net_Sales 
            FROM business GROUP BY Product_Type"
            )

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

b <- ggplot(df_5, aes(Product_Type,Total_Net_Sales)) + geom_bar(stat="identity",color="#FF6666") + xlab("Product Type") + ylab("Total Sales") + theme_minimal()

rates <- sqldf("CREATE TABLE rates 
               (Product_Type VARCHAR(50),
                Rate_of_Return FLOAT
                )
               ")

rates <- sqldf("INSERT INTO rates (Product_Type,Rate_of_Return)
                VALUES ('Baskets',0.31),
                       ('Christmas',0.43),
                       ('Home Decor',0.016),
                       ('Jewelry',0.0164),
                       ('Arts & Sculpture',0.032),
                       ('Kitchen',0.02)
               ")

holiday <- sqldf("SELECT Date,Total_Orders,Total_Sales 
                 FROM business_retailsales2 
                 WHERE Total_Sales > 11000 
                 ORDER BY Total_Sales DESC
                 ")




  
  
  
