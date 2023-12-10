#loading the library I'll be working with
library(tidyverse)

#importing the dataset
summer_products <-read.csv("summer-products-with-rating-and-performance_2020-08.csv")

head(summer_products)


#selecting the columns I'll be working with
rating_products <- summer_products[ , c("title_orig", "price", "retail_price", "currency_buyer", 
                                        "units_sold", "uses_ad_boosts", "rating", "rating_count",
                                        "product_color", "merchant_title", "merchant_rating", 
                                        "merchant_rating_count","merchant_has_profile_picture")]

#removing duplicates and null values
rating_products <- rating_products %>%
  distinct() %>%
  drop_na()

#cleaning the columns title
rating_products<- rename_with(rating_products, tolower)

View(rating_products)

#transposing the certain columns in the dataset
rating_products <- rating_products%>%
  mutate(discount = retail_price - price, ad_boost = case_when(
    uses_ad_boosts <=0 ~ "not_advertised",
    uses_ad_boosts >=1 ~ "advertised"), merchant_picture = case_when(
      merchant_has_profile_picture <=0 ~ "no_picture", 
      merchant_has_profile_picture >=1 ~ "has_picture"
    ))

#also transposing a column
rating_products <- rating_products%>%
  mutate(ratings = case_when(rating >= 1 & rating <= 1.99 ~ "1",
                             rating >= 2 & rating <= 2.99 ~ "2",
                             rating >= 3 & rating <= 3.99 ~ "3",
                             rating >= 4 & rating <= 4.99 ~ "4",
                             rating >= 5 ~ "5"))


#creating a dataset to know the average price of units sold
rating_pricing <- rating_products%>%
  group_by(ratings)%>%
  summarise(avg_price = mean(price),
            units_sold = sum(units_sold))

View(rating_pricing)   

#exporting the dataset for visualisation
write.csv(rating_products, "rating_products.csv")
