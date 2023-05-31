library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)
library(tidytext)
library(dplyr)
library(ggplot2)
library(arules)
# get working directory
getwd()

filepath <- "C:/Users/PRAISE/Documents/R_Forcast/Personal_Project"


transactionData <- read_excel("QVI_transaction_data.xlsx")
customerData <- read.csv("QVI_purchase_behaviour.csv")
View(transactionData)



## Exploratory Analysis

str(transactionData)
str(customerData)

# converting the date column to date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
str(transactionData)



# Convert the PROD_NAME column to a tidy format
tidy_data <- transactionData %>%
  select(PROD_NAME) %>%
  unnest_tokens(word, PROD_NAME)

# Extract the unique words
unique_words <- unique(tidy_data$word)
unique_words

names(transactionData)

transactionData <- transactionData[!grepl("(?i)salsa", transactionData$PROD_NAME),]

#checking for nulls and outliers


is.na(transactionData)
sum(is.na(transactionData))

summary(transactionData$PROD_QTY)

# Filter out rows with outliers
filtered_data <- transactionData %>%
  filter(PROD_QTY < 200)
print(filtered_data)


# Save the filtered rows into a separate data frame
outliers <- transactionData[!(transactionData$PROD_QTY < 200), ]

transactionData <- transactionData %>%
  filter(PROD_QTY != 200)%>%
  group_by(LYLTY_CARD_NBR)

summary(transactionData)

#checking for missing data
colSums(is.na(transactionData))

#summary of transactions by date

summary(transactionData$DATE)


# Group transactions by date and count the number of transactions in each group
transact_summary <- transactionData %>%
  group_by(DATE) %>%
  summarize(TransactionCount = n())
# Print the summary
print(transact_summary)

# Create a sequence of dates and join this the count of transactions by date

# Create a vector of dates from 1 Jul 2018 to 30 Jun 2019
date_vec <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day")

# Convert the vector to a data frame
date_df <- data.frame(DATE = date_vec)

# Join the data frame with the original data to fill in the missing days
transaction_by_day <- merge(date_df, transact_summary, by = "DATE", all = TRUE)
transaction_by_day

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#### Plot transactions over time
ggplot(transaction_by_day, aes(x = DATE, y = TransactionCount)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#looking at individual days in December

december_data <- transaction_by_day %>% filter(month(DATE) == 12, year(DATE) == 2018)
december_data

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#### Plot transactions over time
ggplot(december_data, aes(x = DATE, y = TransactionCount)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December 2018") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


transactionData$PACK_SIZE <- as.numeric(gsub("[^[:digit:]]", "", transactionData$PROD_NAME))
transactionData
summary(transactionData$PACK_SIZE)


# plot a histogram of PACK_SIZE using ggplot
ggplot(data = transactionData, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "white") +
  labs(title = "Histogram of Pack Size", x = "Pack Size (grams)", y = "Frequency")



# create a new column for the brand
transactionData$BRAND <- gsub("^(\\w+).*", "\\1", transactionData$PROD_NAME)

# show the first 10 rows of the data frame
head(transactionData, 10)

#cleaning brand names
unique(transactionData$BRAND)

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "Red", "RRD", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "Smith", "Smiths", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "Snbts", "Sunbites", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "Infzns", "Infuzions", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "WW", "Woolworths", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "NCC", "Natural", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "Dorito", "Doritos", BRAND))

transactionData <- transactionData %>%
  mutate(BRAND = if_else(BRAND == "Grain", "GrnWves", BRAND))



#checking the brand names
unique(transactionData$BRAND)


data <- merge(transactionData, customerData, all.x = TRUE)
str(data)

#checking for missing data
colSums(is.na(data))



sales_summary <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(total_sales = sum(TOT_SALES))
sales_summary


ggplot(sales_summary, aes(x = LIFESTAGE, y = total_sales, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "LIFESTAGE", y = "Total Sales", fill = "PREMIUM_CUSTOMER") +
  ggtitle("Total Sales by LIFESTAGE and PREMIUM_CUSTOMER")



customer_summary <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(num_customers = n())




ggplot(customer_summary, aes(x = LIFESTAGE, y = num_customers, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "LIFESTAGE", y = "Number of Customers", fill = "PREMIUM_CUSTOMER") +
  ggtitle("Number of Customers by LIFESTAGE and PREMIUM_CUSTOMER")



# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
unit_summary <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(avg_units_per_customer = mean(PROD_QTY))


ggplot(unit_summary, aes(x = LIFESTAGE, y = avg_units_per_customer, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(x = "LIFESTAGE", y = "Average Units per Customer", fill = "PREMIUM_CUSTOMER") +
  ggtitle("Average Units per Customer by LIFESTAGE and PREMIUM_CUSTOMER")

# Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(avg_price_ = mean(TOT_SALES))


ggplot(avg_price, aes(x=LIFESTAGE,y= avg_price_, fill = PREMIUM_CUSTOMER))+
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(x="LIFESTAGE", y= "Average Price", fill="PREMIUM_CUSTOMER")+
  ggtitle("Average Price by LIFESTAGE AND PREMIUM_CUSTOMER")

#perform an Welch Two-sample
unique(data$PREMIUM_CUSTOMER)
unique(data$LIFESTAGE)


data<-data.table(data)


pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER == "Mainstream", price],
       data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER != "Mainstream", price], alternative = "greater")



# Create a contingency table of the two columns
contingency_table <- table(data$LIFESTAGE, data$PREMIUM_CUSTOMER)

# Perform the chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Print the result
print(chi_squared_result)



# Create a transaction matrix of LIFESTAGE and PREMIUM_CUSTOMER
# Deep dive into Mainstream, young singles/couples
segment_ <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER =="Mainstream",]
others <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"),]
#### Brand affinity compared to the rest of the population
quantity_segment <- segment_[, sum(PROD_QTY)]


other_quantity <- others[, sum(PROD_QTY)]
quantity_segment_by_brand <- segment_[, .(targetSegment =sum(PROD_QTY)/quantity_segment), by = BRAND]
quantity_other_by_brand <- others[, .(others = sum(PROD_QTY)/other_quantity), by= BRAND]
brand_proportions <- merge(quantity_segment_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/others]
brand_proportions[order(-affinityToBrand)]


# Preferred pack size compared to the rest of the population
quantity_segment_by_pack <- segment_[, .(targetSegment =sum(PROD_QTY)/quantity_segment), by = PACK_SIZE]
quantity_other_by_pack <- others[, .(other = sum(PROD_QTY)/other_quantity), by = PACK_SIZE]
pack_proportions <- merge(quantity_segment_by_pack, quantity_other_by_pack)[,affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]

data[PACK_SIZE == 270, unique(PROD_NAME)]
