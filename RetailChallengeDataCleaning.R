
library(dplyr)
library(maps)
library(ggplot2)

proddf<- read.csv("BigSupplyCo_Products.csv")
orddf <- read.csv("BigSupplyCo_Orders.csv", encoding =  "UTF8")
deptdf <- read.csv("BigSupplyCo_Departments.csv")
custdf <- read.csv("BigSupplyCo_Customers.csv", encoding =  "UTF8")
categdf <- read.csv("BigSupplyCo_Categories.csv")

# products:
  # how many?
length(unique(proddf$Product.Name)) #118 products
length(unique(proddf$Product.Category.Id)) # only 51 categories, will need to join with categdf on category ID
# no unique values in Product.Status. no non-NA values in Product.Description. 
sum(!is.na(proddf$Product.Description))
sum(is.na(proddf$Product.Name))

#Product.Price might be important to customers, however, so we'll include this in the join
hist(proddf$Product.Price)
proddf %>% ggplot(aes(x = Product.Price)) +
  geom_histogram() # wanted to set by product category, but too many! need to link to dept ID


#categories
  #only 2 columns!
product <- left_join(proddf[,c(1,2,5,6)],categdf, by = c("Product.Category.Id" = "Category.Id")) # keep card Id to crossref with orders

write.csv(product, "ProductData.csv", row.names = F)

# departments
  # don't see how to join this to products + categories? maybe with orders...
plot(deptdf$Longitude, deptdf$Latitude, col = as.factor(deptdf$Department.Name)); map(add = T)
  # pretty far reaching geographical extent, but primarily US-based

  
# customers
length(unique(custdf$Customer.Id)) #20652!
length(unique(custdf$Customer.Zipcode)) #996 different zipcodes
unique(custdf$Customer.Country) # looks like US customers don't have a specified country? not a particularly helpful column

# let's change this to English:
#custdf$Customer.Country[custdf$Customer.Country == "EE. UU."] <- "U.S."


# orders
# order status == interesting?
# discount == interesting

ordcust <- right_join(custdf[c(1,8,9,11)], orddf[c(1,3,4,5:9,10,11,12,13:20)], by=c("Customer.Id" = "Order.Customer.Id"))

# cleaning
unique(ordcust$Order.Country)
unique(ordcust$Order.City)


# are there customers who repeatedly order stuff?

ordernum <- ordcust %>% group_by(Customer.Segment) %>% count(Customer.Id)

hist(ordernum$n)
library(lattice) 
ordernum %>% ggplot(aes(x = n)) +
  geom_histogram() +
  facet_wrap(~Customer.Segment) + labs(x = "Unique Orders")

# the consumer segment has a lot more customers that place only 1 order, compared to repeat orders


# what about the value of those orders?
ordcust %>% group_by(Customer.Segment, Customer.Id) %>% 
  mutate(
    #ordernum = count(Customer.Id),
    totalsales = sum(Sales),
    avgOP = mean(Order.Profit),
    avgTotal = mean(Order.Item.Total)
  ) %>%
  ggplot(aes(x = totalsales)) +
  geom_histogram() +
  facet_wrap(~Customer.Segment) + labs(x = "Total Sales")

ordcust %>% group_by(Customer.Segment, Customer.Id) %>% 
  #filter(Order.Profit > -1000) %>%
  mutate(
    #ordernum = count(Customer.Id),
    totalsales = sum(Sales),
    avgOP = mean(Order.Profit),
    avgTotal = mean(Order.Item.Total)
  ) %>%
  ggplot(aes(x = Order.Profit)) +
  geom_histogram() +
  facet_wrap(~Customer.Segment) + labs(x = "Order Profit")
# an outlier here: one sale has a profit of -$4274??
sum(ordcust$Order.Profit < -3000) # there are a lot like this!
sum(ordcust$Order.Profit) # but I guess they're still profitable??
ordcust %>% group_by(Customer.Segment) %>%
  summarize(profit = sum(Order.Profit),
            uniqCustomer = length(unique(Customer.Id)),
            profitperCustomer = profit/uniqCustomer,
            negProfit = length(Order.Profit < 0),
            negperTotal = negProfit/uniqCustomer)
# profit per customer is pretty much the same
  # also consumer segment not more likely to have a negative profit




ordcust %>% group_by(Customer.Segment, Customer.Id, Order.Region) %>% 
  #filter(Order.Profit > -1000) %>%
  mutate(
    #ordernum = count(Customer.Id),
    totalsales = sum(Sales),
    avgOP = mean(Order.Profit),
    avgTotal = mean(Order.Item.Total)
  ) %>%
  ggplot(aes(x = Order.Profit)) +
  geom_histogram() +
  facet_wrap(~Customer.Segment) + labs(x = "Order Profit")

ordcust %>%
  ggplot(aes(x = Sales, y = Order.Profit, col = as.factor(Order.Region))) +
  geom_point() + geom_smooth()



ordcust %>% group_by(Customer.Segment, Customer.Id, Order.Region) %>% 
  #filter(Order.Profit > -1000) %>%
  mutate(
    #ordernum = count(Customer.Id),
    totalsales = sum(Sales),
    avgOP = mean(Order.Profit),
    avgTotal = mean(Order.Item.Total)
  ) %>%
  ggplot(aes(x = Order.Region, y = Order.Profit)) +
  geom_boxplot() +
  facet_wrap(~Customer.Segment) + labs(x = "Order Profit")


library(lubridate)
ordcust$order.date..DateOrders. <- mdy_hm(ordcust$order.date..DateOrders.)


# get which number of order each order is for a unique customer?
ordcust <- ordcust %>% group_by(Customer.Id) %>%
  arrange(`order.date..DateOrders.`) %>%
  mutate(OrderNum = row_number())
ordcust %>%
  ggplot(aes(x = OrderNum, y = Order.Item.Discount.Rate)) +
  geom_point()
  
# anova: what factors are more common in repeat orders?
orditem <- left_join(ordcust, proddf, by = c("Order.Item.Id" = "Product.Card.Id") )
statelm <- lm(OrderNum ~ Customer.State, data = ordcust)
summary(statelm) # significant, but not strongly predictive (R-squared of < 0.1)
  # no individual states are significant on their own

pricing <- lm(OrderNum ~ Order.Item.Discount + Order.Item.Discount.Rate + Product.Price, data = orditem)
summary(pricing) # not significant

segmentlm <- lm(OrderNum ~ Customer.Segment, data = ordcust)
summary(segmentlm)
  # corporate segment has significantly more repeat orders than consumer, although home office does not differ significantly

departmentlm <- lm(OrderNum ~ as.factor(Order.Department.Id), data = ordcust)
summary(departmentlm)

typelm <- lm(OrderNum ~ as.factor(Type), data = ordcust)

anova(statelm)



write.csv(ordcust, "OrderData.csv", row.names = F)
  # need to check with data dictionary if I have to convert to timezone

ordcust %>% 
  filter(order.date..DateOrders. < as_date("1-1-2016", format = "%m-%d-%Y")) %>%
  ggplot(aes(x = order.date..DateOrders., y = Sales)) +
  geom_line()

ordcust %>% 
  filter(order.date..DateOrders. < as_date("1-1-2016", format = "%m-%d-%Y")) %>%
  ggplot(aes(x = order.date..DateOrders., y = Sales)) +
  geom_line()



# what I'm tasked with: calculating retention
# did most of this in tableau

# but cannot figure out how to calculate % customers retained in 2017 in tableau
length(unique(ordcust$Customer.Id[ordcust$order.date..DateOrders. > as_date("1/1/2017", format = "%m/%d/%Y")]))
  # there are 15156 unique customers in 2017 (there are 20652 unique customers in this set total)
cust2017 <- unique(ordcust$Customer.Id[ordcust$order.date..DateOrders. > as_date("1/1/2017", format = "%m/%d/%Y") & ordcust$order.date..DateOrders. < as_date("1/1/2018", format = "%m/%d/%Y")])
custpre2017 <- unique(ordcust$Customer.Id[ordcust$order.date..DateOrders. < as_date("1/1/2017", format = "%m/%d/%Y")])

sum(cust2017 %in% custpre2017)/length(cust2017) * 100 # 57% of 2017 customers are returning!
sum(!(cust2017 %in% custpre2017))


ordcust %>% group_by(Customer.State, Customer.Segment) %>%
  mutate(retentionrate = length(unique(Customer.Id) in 2018) - length(unique(Customer.Id))/length(unique(Customer.Id) in 2015) #there's no way that's right. would just be 1.
         #Customer lifetime value = (total number of transactions per year x average order value x average gross margin x average customer lifespan) / total number of customers for the period,
         #productchurn = [(revenue in the previous period - revenue in this period) / (revenue in the previous period)] x 100,
         #Repeat purchase rate = (number of customers who purchase after initial profit / total number of customers) x 100,
         #Average order value = total revenue earned in the period / total number of orders ???????????//]placed in the period,
         #Customer growth rate = [(new customers acquired x retention rate) / total number of customers at the start of the period] x 100
           )


# then use ^ to calculate discount, price, geographic region, days shipping affected retention