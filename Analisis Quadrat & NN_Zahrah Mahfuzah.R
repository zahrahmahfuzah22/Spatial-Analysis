#load library
library(dplyr)
library(sf)
library(spatstat)
library(readxl)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

#load data
data <- NY.House.Dataset

# Understand the data set ----
colnames(data) # column names
dim(data) # table shape (rows, columns)
glimpse(data) # sample data and types (requires dplyr or tibble package)
head(data) # sample data (first few rows)

# Remove duplicate
data <- distinct(data)

# Remove prefix "Brokered by " from the "BROKERTITLE" column
data$BROKERTITLE <- gsub("^Brokered by ", "", data$BROKERTITLE)
# Remove suffix " for sale" from the "TYPE" column
data$TYPE <- gsub(" for sale$", "", data$TYPE)

# Convert the "baths" column to integer
data$BATH <- as.integer(data$BATH)

# Rename columns and select specific columns
data <- data %>%
  rename(
    SQFT = PROPERTYSQFT) 

# Get summary statistics for selected columns
summary(data[, c("PRICE", "BEDS", "BATH", "SQFT")])

# Group by "sub_locality" and compute summary statistics for "price"
summary_price <- data %>%
  group_by(SUBLOCALITY) %>%
  summarise(
    min = min(PRICE, na.rm = TRUE),
    max = max(PRICE, na.rm = TRUE),
    std = sd(PRICE, na.rm = TRUE),
    mean = mean(PRICE, na.rm = TRUE),
    median = median(PRICE, na.rm = TRUE)) 

# Replace "The Bronx" with "Bronx  County"
data$SUBLOCALITY <- gsub("The Bronx", "Bronx County", data$SUBLOCALITY)
# Replace "East Bronx" with "Bronx County"
data$SUBLOCALITY <- gsub("East Bronx", "Bronx County", data$SUBLOCALITY)
# Replace "Brooklyn Heights" with "Brooklyn"
data$SUBLOCALITY <- gsub("Brooklyn Heights", "Brooklyn", data$SUBLOCALITY)
# Replace "Dumbo" with "Brooklyn"
data$SUBLOCALITY <- gsub("Dumbo", "Brooklyn", data$SUBLOCALITY)
# Replace "Fort Hamilton" with "Brooklyn"
data$SUBLOCALITY <- gsub("Fort Hamilton", "Brooklyn", data$SUBLOCALITY)
# Replace "Jackson Heights" with "Queens"
data$SUBLOCALITY <- gsub("Jackson Heights", "Queens", data$SUBLOCALITY)
# Replace "Rego Park" with "Queens"
data$SUBLOCALITY <- gsub("Rego Park", "Queens", data$SUBLOCALITY)
# Replace "Snyder Avenue" with "Brooklyn"
data$SUBLOCALITY <- gsub("Snyder Avenue", "Brooklyn", data$SUBLOCALITY)

# Filter, sort, and select top 10 rows
top_10_prices_richmond <- data %>%
  filter(SUBLOCALITY == "Richmond County") %>%  # Filter rows where sub_locality is "Richmond County"
  arrange(desc(PRICE)) %>%          # Sort by price in descending order
  slice_head(n = 10)                            # Select the top 10 rows

# Correct the outlier
data$PRICE[data$PRICE == 2147483647] <- 2595000

# Calculate summary statistics for "price" grouped by "sub_locality"
summary_price <- data %>%
  group_by(SUBLOCALITY) %>%
  summarise(
    min = min(PRICE, na.rm = TRUE),
    max = max(PRICE, na.rm = TRUE),
    std = sd(PRICE, na.rm = TRUE),
    mean = mean(PRICE, na.rm = TRUE),
    median = median(PRICE, na.rm = TRUE))

data_cleaned <- data %>%
  group_by(LONGITUDE, LATITUDE) %>%
  filter(PRICE == max(PRICE)) %>%  # Memilih baris dengan harga tertinggi
  ungroup()

# Round longitude and latitude to avoid near-duplicate points
data_cleaned <- data_cleaned %>%
  mutate(
    LATITUDE = round(LATITUDE, 5),    # Round to 5 decimal places
    LONGITUDE = round(LONGITUDE, 5)
  ) %>%
  distinct(LATITUDE, LONGITUDE, .keep_all = TRUE)

##QUADRAT METHOD
#Data scaling
x_max<-max(data_cleaned$LONGITUDE)
x_min<-min(data_cleaned$LONGITUDE)
y_max<-max(data_cleaned$LATITUDE)
y_min<-min(data_cleaned$LATITUDE)

#PPP object berdasarkan long and lat
ppp_house<-ppp(data_cleaned$LONGITUDE,data_cleaned$LATITUDE, window =
                 owin(c(x_min,x_max),c(y_min,y_max)))

#Quadrat count
Q <- quadratcount(ppp_house, nx=10, ny=10)
Q

#Plotting PPP, Quadrat, intensity
par(mar = c(2, 2, 2, 2))
plot(ppp_house, axes=T, main="New York Housing Market")
plot(ppp_house, pch=20, cols="blue", main="New York Housing Market", add=TRUE, axes =F)
plot(Q, add=TRUE, col="red")

#Plotting PPP and Quadrat analysis par ("mar")
par(mar = c(2, 2, 2, 2))
plot(ppp_house, axes = T, main="New York Housing - Quadrat Point")
plot(ppp_house, pch = 20, cols = "red", main = "New York Housing - Quadrat Point", add =
       TRUE, axes = T)
plot (Q, add = TRUE, col = "green")

qt <- quadrat.test(ppp_house, nx = 10, ny = 10)
qt

par(mar = c(2, 2, 2, 2))
plot(ppp_house, main = "New York Housing - Quadrat Test")
plot(qt, add = TRUE, cex = 0.5, col = "red")

#Uji Lanjutan
var_sample <- var(qt$observed)
var_sample
mean_sample <- mean(qt$observed)
mean_sample
relative_var <- var_sample/mean_sample
relative_var
ics <- relative_var-1
ics

##analisis harga murah-mahal
#Buat estimasi kepadatan (density estimation) untuk heatmap berdasarkan titik properti
density_map <- density(ppp_house, weights = data_cleaned$PRICE, sigma = 0.02)

#Plot heatmap
plot(density_map, main = "Heatmap New York Housing Market (based on Price)")
plot(ppp_house, pch=20, cols="blue", add=TRUE)
plot(Q, add=TRUE, col="green")

#Menghitung r_obs dan r_exp untuk Nearest Neighbor Analysis
distance <- dist(c(data_cleaned$LONGITUDE, data_cleaned$LATITUDE))
mean_dist <- mean(distance)
r_obs <- mean_dist/(0.5*sqrt(length(data_cleaned)))
r_exp <- 1/(2*(sqrt(length(data_cleaned)/54556)))
print(r_obs)
print(r_exp) 

#Metode Nearest Neighbor Analysis 
# G-function (Nearest Neighbor Function) with envelope
G_env <- envelope(ppp_house, Gest)
plot(G_env, main = "G Function (Nearest Neighbor)", legendargs=list(cex=1, bty="n", x.intersp=0.5, y.intersp=0.8))

# F-function (Empty Space Function) with envelope
F_env <- envelope(ppp_house, Fest)
plot(F_env, main = "F Function (Empty Space)", legendargs=list(cex=1, bty="n", x.intersp=0.5, y.intersp=0.8))

# K-function (Ripley's K-function) with envelope
K_env <- envelope(ppp_house, Kest)
plot(K_env, main = "K Function (Ripley's K)", legendargs=list(cex=1, bty="n", x.intersp=0.5, y.intersp=0.8))

# L-function (Modified Ripley's K-function) with envelope
L_env <- envelope(ppp_house, Lest)
plot(L_env, main = "L Function (Modified Ripley's K)", legendargs=list(cex=1, bty="n", x.intersp=0.5, y.intersp=0.8))
