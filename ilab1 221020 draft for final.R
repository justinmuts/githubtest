
# iLab1 2020 

# SUMMARY -----------------------------------------------------------------

# Codes for extracting Google Trends data for analysis and combine with BOSCAR
# data for correlation plot analysis. This will contain codes for time series
# analysis showing trends, seasonality and unaccounted fluctuations.


# LOAD PACKAGES -----------------------------------------------------------


library(dplyr)
library(TSstudio)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dygraphs)
library(xts)         
library(tidyverse)
library(corrplot)
library(gtrendsR)
library(tseries)
library(prophet)
library(tidytext)
library(igraph)
library(ggraph)
library(scales)


# GOOGLE TRENDS DATA  -----------------------------------------------------


# 5 KEYWORD TERMS USED .

search_items <- c('domestic violence',
                  'domestic assault',
                  'domestic abuse',
                  'avo', 
                  'spousal abuse')

# We are narrowing the search down to Australia New South Wales
location <- "AU-NSW"


# The 10 year time period we are looking into 
yrs10 <- "2010-06-01 2020-06-30"


# Initiating GOOGLE TREND SEARCH:
gtsearch_1 <- gtrends(keyword = search_items,
                      geo = location,
                      time = yrs10,
                      low_search_volume = T) # Including low search volumes within the location

# Plot the information for the relative interest for each keywords  
plot(gtsearch_1)

# review the structure of the object "gtsearch_1"
str(gtsearch_1) 


# Save Interest over time dataframe as "gt1_intot"
gt1_intot <- gtsearch_1$interest_over_time

# Converting date column into date format using ymd() function from lubridate
# package.
gt1_intot$date <- ymd(gt1_intot$date)


# Graphing interest overtime using 6 monthly increments 
ggplot(data = gt1_intot) + aes(x = date, y = hits, color = keyword) + 
  geom_line() + scale_colour_viridis_d(option  = "D") + 
  labs(title = "Google Trends on Domestic Violence in NSW", 
       subtitle =  "Interest Overtime from June 2010 to June 2020", 
       x = "Date", y = "Relative Interest (%)", caption = 'Source: Google Trends') + 
  theme_light() +
  theme(legend.position = 'right') + 
  geom_smooth(se = T, method = 'loess') +
  theme_bw() +
  scale_x_date(date_labels = "%b %y",date_breaks = "6 month",
               limits = c(as.Date("2010-06-30"),as.Date("2020-06-30"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0,to = 100, by = 5))


# Get rid of these columns
gt1_interest_OT <- gt1_intot %>% select(-geo,-time,-gprop,-category)

# Lets see the first 10 rows of "gt1_interest_OT"
head(gt1_interest_OT)

# spread the data converting the keywords in their own columns 
gt1_interest_OT <- spread(gt1_interest_OT, key = keyword, value = hits)


# Vincents original script ------------------------------------------------
# BOSCAR DATA  -----------------------------------------------------

#read in csv file with DV data
dvdata <- read.csv("/Users/Justin_Mah/Downloads/DVdata.csv",header = TRUE,sep = ",")

# Since my time is currently a factor, I have to convert it to a date-time format!
dvdata$datetime <- dmy(dvdata$datetime)

#Find the correlations that exist between the series

cordata <- cor(dvdata[,unlist(lapply(dvdata, is.numeric))])
corrplot(cordata, method="circle", type = "upper")

# rename the 'datetime' column in dvdata to 'date'
dvdata <- rename(dvdata,date = datetime)

# Review the first 10 rows of dvdata dataset
head(dvdata)

# THE CORRELATION PLOT ----------------------------------------------------

# combine gt_interest_OT and bocsar

# Combine the two datasets together from Google Trends and BOCSAR
j_all <- left_join(dvdata,gt_interest_OT, by = 'date')

# View the joined dataframe "j_all"
View(j_all)

cordata <- cor(j_all[,n])
corrplot(cordata, method = "number", type = "upper")
str(dv_interest_OT)
cor(j_all,)

# Correlation plot with BOCSAR data and Google Trends  

correlated <- cor(j_all[,-1], use="pairwise", method="pearson")
corrplot(correlated, method = 'number', type = 'upper',
         tl.col = 'red',
         title = 'Correlation plot BOCSAR and Google Trends')




# PROPHET PACKAGE --------------------------------------------------------------

# We are going to do a times series analysis using the Prophet package created
# by Facebook.

# We are selecting AVO to do our time series as save it as "prop_avo"
prop_avo <- j_all %>%
  select(date,avo)

# We have to rename the columns "ds" for date (predictor variable) and "y" the
# (response variable) as it is mandatory by the commands in prophet package.
prop_avo <- prop_avo %>% rename(ds=date,y=avo)

# lets check the structure of "prop_avo"
str(prop_avo)

# want to make sure the ds column is a date format so we use ymd() function from
# lubridate package.
prop_avo$ds <- ymd(prop_avo$ds)

#date column to become the index
#prop_avo <- column_to_rownames(prop_avo, var = "ds")

# Initialising time series in prophet 
avo <-prophet(prop_avo,growth = 'linear', weekly.seasonality = F, daily.seasonality = F)

avo_fut <-make_future_dataframe(avo,freq = 'month', periods = 12)

# prediction using prophet
avo_fore <- predict(avo,avo_fut)

# Timeseries plot forecast model for AVO
plot(avo,avo_fore,uncertainty = T,xlabel='year',ylabel="relative interest (%)",label='df') + 
  labs(title='AVO Timeseries with 12 months prediction forecast',
       subtitle = "July 2010 to June 2020 + 12 months prediction",
       caption = "Source: Google Trends")

# The trends and average yearly trend for Google keywords search term "avo"
prophet_plot_components(avo, avo_fore, uncertainty = T,plot_cap = F)


# Let's display and Interactive plot for Google search term "avo". Note the blue
# shadow represents the predicted certainty level
dyplot.prophet(avo, avo_fore,main="AVO with Forecast for next 12 months",y="relative interest (%)")



# TIME SERIES FORECASTING USING FORECAST PACKAGE -------------------------------

# Reusing object "prop_avo" 
head(prop_avo)
# #AVO
# prop_avo <- j_all %>%
#   select(date,avo)
# 
# prop_avo <- prop_avo %>% rename(ds=date,y=avo)


# Using ts() to decompose trends 

# Transform to `ts` class
avorder_ts <- ts(prop_avo$y, start = c(2010,7), end = c(2020,6), freq = 12)  # Specify start and end year, measurement frequency (monthly = 12)

# Decompose using `stl()`, This will generate Seasonal, trend and remainder
# components.
avorder_stl <- stl(avorder_ts, s.window = "period",robust = T)


# Generate plots
plot(avorder_stl, main = "Decomposition Time Series - 'avo' from Google Trends")  # top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation

# Monthplot to show the seasonal pattern of keyword "avo" between 2010 to 2020
monthplot(avorder_ts, choice = "seasonal",
          main=" Seasonal pattern for keyword 'AVO' From 2010 to 2020",
          type= c("h"),
          xlab ='month',ylab = "relative interest %")




# Text mining on related queries ------------------------------------------


# NOTE: I AM STILL WORKING ON THIS TO REPRESENT THE RELATED QUERIES FOR EACH
# KEYWORD SEARCH AND DISPLAY IT LIKE A DECISION TREE.

# The 5 keywords are:
# 1. domestic violence
# 2. domestic assault
# 3. domestic abuse
# 4. avo
# 5. spousal abuse
related_queries <- gtsearch_1$related_queries
View(gtsearch_1$related_queries)
View(related_queries)
head(gtsearch_1$related_queries)
head(related_queries)
# subject related_queries                          value    geo           keyword category
# 1     100             top    domestic violence australia AU-NSW domestic violence        0
# 2      90             top          domestic violence nsw AU-NSW domestic violence        0
# 3      41             top                family violence AU-NSW domestic violence        0
# 4      40             top   domestic violence statistics AU-NSW domestic violence        0
# 5      38             top domestic violence in australia AU-NSW domestic violence        0
# 6      25             top     domestic violence services AU-NSW domestic violence        0


# Column names of related queries
colnames(related_queries)

#Related Queries 
dviolence_RQueries <- related_queries %>% 
  filter(keyword == 'domestic violence')

dabuse_RQueries <- related_queries %>% 
  filter(keyword == 'domestic abuse')

dassault_RQueries <- related_queries %>% 
  filter(keyword == 'domestic assault')

avo_RQueries <- related_queries %>% 
  filter(keyword == 'avo')

sabuse_RQueries <- related_queries %>% 
  filter(keyword == 'spousal abuse')

# generate the arrow
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))




##### update related queries



dv_related <- as_tibble(dv_search$related_queries)

#food becomes dv related TOP QUERIES
topqueries_bigram <- dv_related %>% 
  filter(related_queries == 'top') %>% 
  unnest_tokens(bigram, value, token = 'ngrams', n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(!is.na(word1), !is.na(word2)) %>% 
  graph_from_data_frame() 


set.seed(0612)

ggraph(topqueries_bigram, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = 'orange', size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Google Trends: top queries related searches in domestic Violence",
       subtitle = "From June 2010 to June 2020 ",
       caption = "by: Justin Mah ")

#  experiment 
dv_related %>%
  #sum(dv_related$value) %>%
  count(value, sort = TRUE) %>%
  #filter(n > 1) %>%
  mutate(related_queries = reorder(value, n)) %>%
  ggplot(aes(related_queries, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()




# TOP QUERIES -----------------------------------------


topqueries_bigram <- dAbuse_RQ %>% 
  filter(related_queries == 'top') %>% 
  unnest_tokens(bigram, value, token = 'ngrams', n = 1) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(!is.na(word1), !is.na(word2)) %>% 
  graph_from_data_frame() 



da_related <- as_tibble(dv_search$related_queries)

# Google Trends TOP QUERIES
topqueries_bigram <- dv_related %>% 
  filter(related_queries == 'top') %>% 
  unnest_tokens(bigram, value, token = 'ngrams', n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(!is.na(word1), !is.na(word2)) %>% 
  graph_from_data_frame() 


set.seed(0612)

ggraph(topqueries_bigram, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = 'orange', size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Google Trends Top queries for avo & domestic violence",
       subtitle = "From June 2010 to June 2020 ",
       caption = "by: Justin Mah ")

#  experiment 
dv_related %>%
  #sum(dv_related$value) %>%
  count(value, sort = TRUE) %>%
  #filter(n > 1) %>%
  mutate(related_queries = reorder(value, n)) %>%
  ggplot(aes(related_queries, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



###### BAR GRAPH



# BAR GRAPH for related topics searches for the 5 keywords

# for breakout
# breakout <- related_queries %>%
#   filter(subject == 'Breakout')

# for related queries without breakout
related_queries_amend <- related_queries %>% 
  filter(subject != 'Breakout')

# related queries "rising'
related_queries_rising <- related_queries_amend %>% 
  filter(related_queries == 'rising')

#related queries 'top'
related_queries_top <- related_queries %>%
  filter(related_queries == 'top')

# Bar Graph for related queries 'top'

related_queries_top$subject <- as.numeric(related_queries_top$subject)
ggplot(related_queries_top,aes(fct_reorder(value,subject),subject)) + 
  geom_bar(aes(fill = keyword),stat = 'identity') + 
  facet_grid(cols=vars(keyword)) +
  coord_flip() +
  scale_colour_viridis_d(option  = "viridis") + 
  labs(title = "Most frequent related queries searched terms for 5 keywords chosen in Google Trends",
       subtitle =  "Related queries searches between June 2010 to June 2020", 
       x = "Most search queries related to the 5 keywords", 
       y = "popularity %") + 
  scale_y_continuous(breaks = seq(from = 0,to = 100, by = 5)) +
  # theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.5)) +
  theme_minimal() + theme(legend.position = 'right')




 