library(tidyverse)
library(lubridate)
library(scales)
library(dplyr)
library(stringr)
library(ggalt)
library(patchwork)
library(reshape2)
library(ggpubr)
library(hrbrthemes)
library(viridis)
library(ggforce)
library(treemapify)

# Loading the data
load('sales_data.Rdata')

sales$Quantity <- as.integer(sales$Quantity)
sales$PriceEach <- as.numeric(sales$PriceEach)
sales$Product <- as.factor(sales$Product)

sales <- sales %>%
  filter(Date != '2020-01-01') %>%
  mutate(sales_value = PriceEach * Quantity,
         month = month(Date, label = TRUE),
         place = paste(City, State, ZIP, sep = " , "),
         hour = hour(DateTime),
         day = weekdays(DateTime),
         category = case_when(
           grepl('Monitor', Product) ~ 'Monitors',
           grepl('Laptop', Product) ~ 'Laptops',
           grepl('Headphones', Product) ~ 'Headphones',
           grepl('Phone', Product) ~ 'Phones',
           grepl('Cable|Batteries', Product) ~ 'Accessories',
           TRUE ~ 'Home Appliances'
         ))

view(sales)

sales$category <- as.factor(sales$category)
sales$day <- as.factor(sales$day)

# Sales Performance over time
p <- sales %>%  
  group_by(month) %>%
  summarise(monthly_sales = sum(sales_value)) %>%
  ggplot(aes(x = month, y = monthly_sales)) +
  geom_line(linewidth = 1.5, colour = '#2171B5', group = 1) + geom_point(size = 3, shape = 21, fill = '#2171B5') +
  geom_text(aes(label = paste0(comma(round(monthly_sales/1000)), 'K')), hjust = -.25, vjust = .10, family = 'serif', size = 11/.pt) +
  geom_label(data = data.frame(x = 9.3820086903463, y = 4726441.49624471, label = "Sales peak the most in December\n month with a revenue of $ 4,613K"),
             mapping = aes(x = x, y = y, label = label),
             label.padding = unit(0.8, "lines"),
             label.size = 0.8, label.r = unit(0.8, "lines"),
             inherit.aes = FALSE, family = 'serif', fontface = 2, size = 14/.pt, fill = '#DEEBF7') +
  labs(title = "Sales by month, 2019", subtitle = 'Sales Performance over time', x = "Month", y = "Monthly Sales (in thousands $)") +
  scale_y_continuous(limits = c(1500000, 5000000), breaks = c(2000000, 3000000, 4000000, 5000000), labels = comma_format(scale = 1/1000, prefix = "$", suffix = 'K')) +
  theme_classic() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm')),
        plot.subtitle = element_text(face = 'bold',size = 16,family = 'serif'))

# Revenue contribution by location
p2 <- sales %>%
  group_by(place) %>%
  summarise(sales_city = sum(sales_value),
            qty_city = sum(Quantity)) %>%
  ggplot(aes(area = sales_city, label = paste0(place,'\nQuantity sold: ', comma(round(qty_city)),'\nRevenue Contribution: ', '$',comma(round(sales_city/1000)), 'K'), fill = place)) +
  geom_treemap() + geom_treemap_text(family = 'serif', size = 16) +
  labs(title = 'Quantity sold and Revenue Contribution by Location (ZIP code)') +
  guides(fill = 'none') + 
  scale_fill_brewer(palette = 'Set3') +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
    plot.title = element_text(family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm')))

# Correlation between revenue and price
p3 <- sales %>%
  group_by(Product, PriceEach) %>%
  summarise(revenue = sum(PriceEach * Quantity)) %>%
  ggplot(aes(x = PriceEach, y = revenue)) + geom_point(color = '#2171B5', shape = 'circle', size = 3.5) + 
  geom_smooth(method = 'lm', se = FALSE, linewidth = 1.25, color = '#2171B5') + 
  geom_label(data = data.frame(x = 985.442792608862, y = 6977162.54002543, label = "There is strong positive correlation\n between revenue contribution\n and the price of each product"),
             mapping = aes(x = x, y = y, label = label),
             label.padding = unit(0.8, "lines"),
             label.size = 0.8, label.r = unit(0.8, "lines"),
             inherit.aes = FALSE, family = 'serif', size = 13/.pt, fontface = 2, fill = '#DEEBF7') +
  geom_curve(data = data.frame(x = 1395.43555800745, y = 6284228.4814082, xend = 1285.05289040014, yend = 7294757.31689167),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(30L, unit(0.1, "inches"),
                           "last", "closed"),
             inherit.aes = FALSE) +
  labs(title = "Revenue vs Price of Each Product in 2019", x = "Product Price Each", y = "Product Revenue (in thousands $)") +
  scale_y_continuous(labels = comma_format(scale = 1/1000, prefix = "$", suffix = 'K')) +
  scale_x_continuous(labels = dollar_format()) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(face = 'bold',size = 18,family = 'serif', margin = margin(0,0,0.5,0,'cm')))


x_aixs_points <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

# Optimal time for product advertisements
p4 <- sales %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n, color = -n)) + geom_line(linewidth = 1.5) + geom_point(aes(x = x_aixs_points , y = n), size = 2.5) +
  geom_text(data = data.frame(x = c(19.6448624330457, 11.4289494790031, 18.3454068127634, 12.5607334063457, 20.4832208977439, 13.5248456407487),
                              y = c(13609.7038915174, 13355.2643387096, 13253.4885175865, 13406.1522492712, 13049.9368753403, 12897.2731436557),
                              label = c("12,905", "12,411", "12,280", "12,587", "12,228", "12,129")),
            mapping = aes(x = x, y = y, label = label),
            size = 4.23, angle = 45L, colour = "navy", family = "serif", fontface = 2, inherit.aes = FALSE) + 
  labs(title = 'Time of the day most orders placed in 2019', x = 'Time in Hour', y = 'Orders count', color = 'Count') +
  geom_label(data = data.frame(x = 3.90886842616698, y = 12979.379786752, label = "Most orders were placed at\n 12 AM and 7 PM of the day"),
             mapping = aes(x = x, y = y, label = label),
             label.padding = unit(0.8, "lines"),
             label.size = 0.8, label.r = unit(0.8, "lines"),
             inherit.aes = FALSE, family = 'serif', size = 13/.pt, fontface = 2, fill = '#DEEBF7') +
  scale_x_continuous(limits = c(0,23), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  scale_y_continuous(limits = c(500,14000), breaks = c(2000, 4000, 6000, 8000, 10000, 12000, 14000), labels = comma_format()) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(face = 'bold',size = 18,family = 'serif', margin = margin(0,0,0.5,0,'cm')),
        axis.text = element_text(size = 12,family='serif')) +
  guides(color = 'none')

# Revenue Contribution by Product 
p5 <- sales %>%
  group_by(Product) %>%
  summarise(sales_product = sum(sales_value)) %>%
  ggplot(aes(x = sales_product, y = reorder(Product, sales_product), fill = Product)) + geom_col() +
  geom_text(aes(label = paste0(comma(round(sales_product/1000)), 'K')), position = position_dodge(.9), size = 3.5, family = 'serif', fontface = 'bold', hjust = -.10) +
  geom_label(data = data.frame(x = 7072373.67046586, y = 7.39351499680223, label = "Macbook Pro Laptop has the highest\n revenue contribution while AAA Batteries\n (4-pack) has the least revenue contribution"),
             mapping = aes(x = x, y = y, label = label),
             label.padding = unit(0.8, "lines"), 
             label.size = 0.8, label.r = unit(0.8, "lines"),
             inherit.aes = FALSE, family = 'serif', size = 13/.pt, fontface = 2, fill = '#DEEBF7') +
  scale_x_continuous(labels = comma_format(scale = 1/1000, prefix = "$", suffix = 'K'), limits = c(0, 10000000), breaks = c(0, 2500000, 5000000, 7500000, 10000000)) +
  scale_fill_manual(values = c('AAA Batteries (4-pack)' = 'red', 'Macbook Pro Laptop' = '#2171B5', '20in Monitor' = 'grey', '27in 4K Gaming Monitor'= 'grey', 
                               '27in FHD Monitor' = 'grey', '34in Ultrawide Monitor' = 'grey', 'AA Batteries (4-pack)'='grey', 'Apple Airpods Headphones' = 'grey', 
                               'Bose SoundSport Headphones'= 'grey', 'Flatscreen TV'='grey', 'Google Phone'='grey', 'iPhone'= 'grey', 'LG Dryer'='grey', 
                               'LG Washing Machine'='grey', 'Lightning Charging Cable'='grey', 'ThinkPad Laptop'= 'grey', 'USB-C Charging Cable'='grey', 
                               'Vareebadd Phone'='grey', 'Wired Headphones'='grey')) +
  labs(x = 'Sales(in thousands $)', y = 'Product Name', title = 'Revenue Contribution by Product, 2019') +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), 'cm'),
        axis.text.y = element_text(size = 11,family='serif'),
        axis.text.x = element_text(family = 'serif', size = 11),
        axis.title = element_text(family = 'serif', size = 12, face = 'bold'),
        plot.title = element_text(family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm'))) +
  guides(fill = 'none')

# Top selling products (in qty) Vs their price per unit
p6 <- sales %>%
  group_by(Product, PriceEach) %>%
  summarise(quantity_ordered = sum(Quantity)) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Product, -quantity_ordered), y = quantity_ordered, fill = Product), stat = "identity") + 
  geom_line(aes(x = reorder(Product, -quantity_ordered), y = PriceEach * 35000 / 1750), color = 'red', group = 1, , linewidth = 1.25) +
  geom_point(aes(x = Product, y = PriceEach * 35000 / 1750), size = 2.5, shape = 21, fill = 'red') +
  labs(title = 'Correlation between top selling products and its price', x = 'Product Name') +
  scale_y_continuous(name = expression("Quantity Ordered"), limits = c(0, 35000), breaks = seq(0, 35000, 5000), 
                     sec.axis = sec_axis(~. * 1750 / 35000, name = "Price", breaks = seq(0, 1750, 250), labels = dollar_format()), labels = comma_format()) +
  scale_fill_manual(values = c('AAA Batteries (4-pack)' = '#2171B5', 'Macbook Pro Laptop' = '#2171B5', '20in Monitor' = 'grey', '27in 4K Gaming Monitor'= 'grey', 
                               '27in FHD Monitor' = 'grey', '34in Ultrawide Monitor' = 'grey', 'AA Batteries (4-pack)'='grey', 'Apple Airpods Headphones' = 'grey', 
                               'Bose SoundSport Headphones'= 'grey', 'Flatscreen TV'='grey', 'Google Phone'='grey', 'iPhone'= 'grey', 'LG Dryer'='red', 
                               'LG Washing Machine'='red', 'Lightning Charging Cable'='grey', 'ThinkPad Laptop'= 'grey', 'USB-C Charging Cable'='grey', 
                               'Vareebadd Phone'='grey', 'Wired Headphones'='grey')) +
  theme(axis.text.x = element_text(size = 11,family='serif', angle = 45, hjust = 1),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(face = 'bold',size = 18,family = 'serif', margin = margin(0,0,0.5,0,'cm')),
        axis.text = element_text(size = 12,family='serif')) +
  guides(fill = 'none') +
  geom_text(data = data.frame(x = c(14.5533758488249, 1.01060058576893 ),
                              y = c(34158.7142911239, 1270.93750966341),
                              label = c("1,700", "2.99")),
            mapping = aes(x = x, y = y, label = label),
            colour = "red", inherit.aes = FALSE, family = 'serif', fontface = 2) + 
  geom_text(data = data.frame(x = c(1.02087262320736, 14.010927511879 ),
                              y = c(31930.8326381863, 5620.61121301787),
                              label = c("31,012", "4,727")),
            mapping = aes(x = x, y = y, label = label),
            colour = '#2171B5', inherit.aes = FALSE, family = 'serif', fontface = 2) +
  geom_text(data = data.frame(x = c(18.0164336458697, 19.035378188727 ),
                              y = c(13300.7375406287, 13300.7375406287),
                              label = c("600", "600")),
            mapping = aes(x = x, y = y, label = label),
            colour = "red", family = "serif", fontface = 2, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = c(18.0164336458697, 19.0002421700077 ),
                              y = c(1600.30872418868, 1600.30872418868),
                              label = c("666", "646")),
            mapping = aes(x = x, y = y, label = label),
            colour = "red", family = "serif", fontface = 2, inherit.aes = FALSE)

# Popular product combinations
duplicates <- sales %>%
  filter(duplicated(DateTime) | duplicated(DateTime, fromLast = TRUE)) %>%
  group_by(DateTime) %>%
  mutate(Grouped = paste(unique(Product), collapse = ", ")) %>%
  select(DateTime, Grouped) %>%
  distinct() %>%
  separate_rows(Grouped, sep = ", ")

df_combinations <- duplicates %>%
  group_by(DateTime) %>%
  summarise(product_comb = if (n() == 1) list(as.character(Grouped)) else 
    list(combn(sort(Grouped), 2, paste0, collapse = ","))) %>%
  unnest(cols = c(product_comb)) %>%
  separate(product_comb, c("ProductX", "ProductY"), sep = ",", fill = "right") %>%
  na.omit %>%
  count(ProductX, ProductY)

my_cols <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF",
                        "#E16462FF", "#FCA636FF", "#F0F921FF")
                        
p7 <- ggballoonplot(as.data.frame(df_combinations),
              x = "ProductX", y = "ProductY", size = "n", fill = "n") +
  labs(x = 'Product', y = 'Product', fill = 'Count', size = 'Count', title = 'Combinations of Products most often sold together') +
  geom_label(data = data.frame(x = 12.1054906841001, y = 5.0024321859571, label = "Iphone and Lightning Charging Cable\n is the best selling product combination\n with 1305 transactions in 2019"),
             mapping = aes(x = x, y = y, label = label),
             label.padding = unit(0.8, "lines"),
             label.size = 0.8, label.r = unit(0.8, "lines"),
             inherit.aes = FALSE, family = 'serif', size = 13/.pt, fontface = 2, fill = 'lavender') +
  scale_fill_gradientn(colors = my_cols) +
  theme(legend.position = c(.95,.40),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text = element_text(family = 'serif', size = 10),
        plot.title = element_text(family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm')))

sales_day <- sales %>%
  group_by(Product, day) %>%
  mutate(sales_day = sum(sales_value)) %>%
  summarise(day_sales = sum(sales_day)) 

sales_melted <- melt(sales_day, id.vars = c("Product", "day"), measure.vars = "day_sales")

# Sales Analysis by Day and Product
p8 <- ggplot(sales_melted, aes(x = day, y = reorder(Product, -value), fill = -value)) +
  geom_tile() +
  scale_x_discrete(limits = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) +
  labs(x = "Day", y = "Product", fill = "Sales", title = 'Sales by Day and Product, 2019') +
  scale_fill_distiller(palette = "RdPu") +
  theme_classic() +
  theme(axis.line = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text = element_text(family = 'serif', size = 11),
        axis.title = element_text(family = 'serif', size = 12, face = 'bold'),
        plot.title = element_text(family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm'))) +
  guides(fill = 'none')


