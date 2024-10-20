library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
setwd("C:/Users/gabi_/OneDrive/Documentos/R/EDA-Customers_Segmentations")
getwd()

#Create df with the data

customers_df <- read.table("Mall_Customers.csv", header=T, sep=",",fill=T)

head(customers_df)
summary(customers_df)
sum(is.na(customers_df))
distinct(customers_df["Genre"])


#Change the varibles names for the manipulations
colnames(customers_df)[4]<-"Income"
colnames(customers_df)[5]<-"Spending"
colnames(customers_df)
customers_df[1-5]

#Change Genre values to a numerics values
customers_df <- customers_df %>% mutate(genero = ifelse(Genre == "Male", 1, 0))
head(customers_df)

#Exploratory Questions

# 1.What is the relationship between annual income and spending score?
# 2.Does gender or age influence spending behavior?
# 3.Which customers have high spending scores but low incomes, or vice versa?

df_1 <- customers_df[,c("Age","Spending","Income","genero")]
round(cor(df_1),2)  
rcorr(as.matrix(df_1))
# 1. Here I can see that the relationship between income and spending is almost zero = 0.01
# 2. We can observe a negative relationship between age and spending

# 3.

mean_income <- mean(customers_df$Income, na.rm = T)
mean_spending <- mean(customers_df$Spending, na.rm = T)

high_income_x_low_spending <- subset(customers_df, Income > mean_income & Spending < mean_spending) %>% count()
 
high_spending_x_low_income <- subset(customers_df, Income < mean_income & Spending > mean_spending) %>% count()

high_income_x_low_spending_male <- subset(customers_df, Income > mean_income & Spending < mean_spending & genero == 1) %>% count()
high_income_x_low_spending_female <- subset(customers_df, Income > mean_income & Spending < mean_spending & genero == 0) %>% count()

high_spending_x_low_income_male <- subset(customers_df, Income < mean_income & Spending > mean_spending & genero == 1) %>% count()
high_spending_x_low_income_female <- subset(customers_df, Income < mean_income & Spending > mean_spending & genero == 0) %>% count()

#Male with high income and low spending
high_income_x_low_spending_male
#Female with high income and low spending
high_income_x_low_spending_female
#Male with high spending income and low income
high_spending_x_low_income_male
#Female with high income and low spending
high_spending_x_low_income_female
