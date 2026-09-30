sales<-read.csv("Sales_Analysis.csv")
library(tidyverse)
# understand the dataset
head(sales)
str(sales)
summary(sales)
dim(sales)

#check missing values
colSums(is.na(sales))

# remove duplicates
sales <- sales %>%
  distinct()


#create profit column
sales$Profit <- sales$Sales - sales$Cost
sales$Profit



#total sales
sales %>%
summarise(total_sales=sum(Sales))
#total profit
sales %>%
summarise(total_profit=sum(Profit))

#sales by region
sales %>%
group_by(Region)%>%
summarise(total_sales=sum(Sales))%>%
arrange(desc(total_sales))

#sales by top products
sales %>%
group_by(Product_Category)%>%
summarise(total_sales=sum(Sales))%>%
arrange(desc(total_sales))

#sales by top products in each region
sales %>%
group_by(Region,Product_Category)%>%
summarise(total_sales=sum(Sales))%>%
arrange(Region,desc(total_sales))

#visualise sales
library(ggplot2)
ggplot(sales,aes(x=Product_Category,y=Sales,))+
  geom_col(fill="steelblue")+
  scale_y_continuous(labels = scales::label_comma())+
  labs(title="Sales by Product Category",x="Product Category",y="Sales")

# export cleaned data to CSV
write.csv(sales, "Sales_Analysis_cleaned.csv", row.names = FALSE)
