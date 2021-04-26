# install.packages("corrplot")
library(readxl) 
library(dplyr)  
library(ggplot2) 
library(xlsx) 
library(tidyr)
library(corrplot)
library(RColorBrewer)

# Data
{
## ESG Data
esg_score <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG-Annual.xlsx", 
                        sheet = "ESG Combined Score", col_types = c("text", "skip", "skip", "skip", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
esg_score_narm <- na.omit(esg_score)
## Volatility Data
volatility <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG-Annual.xlsx", 
                         sheet = "volatility", col_types = c("text", "text", "text", "skip", "text","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric"))
volatility_narm <- na.omit(volatility)

## Total Asset Data
total_asset <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG-Annual.xlsx", 
                          sheet = "total-asset-us$WC07230", col_types = c("skip", 
                                                                          "text", "skip", "skip", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
total_asset_narm <- na.omit(total_asset)

## ROA Data
roa <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG-Annual.xlsx", 
                  sheet = "roa", col_types = c("text", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
roa_narm <- na.omit(roa)

## Tobinq Data
Tobinq <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG-Annual.xlsx", 
                        sheet = "tobinq", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
Tobinq_narm <- na.omit(Tobinq)

## Re-constructure data from wide to long
esg_score_narm_longer <- pivot_longer(esg_score_narm, 2:11, names_to = "Year", values_to = "ESG_score")
volatility_narm_longer <- pivot_longer(volatility_narm, 5:15, names_to = "Year", values_to = "Volatility")
total_asset_narm_longer <- pivot_longer(total_asset_narm, 2:11, names_to = "Year", values_to = "Total_Asset")
roa_narm_longer <- pivot_longer(roa_narm, 2:11, names_to = "Year", values_to = "ROA")
Tobinq_narm_longer <- pivot_longer(Tobinq_narm, 3:12, names_to = "Year", values_to = "Tobinq")

## Combine all data
esg_universe <- left_join(volatility_narm_longer, total_asset_narm_longer, 
                          by = c("Code" = "Code", "Year" = "Year"))
esg_universe <- left_join(esg_universe, esg_score_narm_longer, 
                          by = c("Name" = "Name", "Year" = "Year"))
esg_universe <- left_join(esg_universe, roa_narm_longer)
esg_universe <- left_join(esg_universe, Tobinq_narm_longer, 
                          by = c("Name"="Name", "Code" = "Code", "Year" = "Year"))
esg_universe <- na.omit(esg_universe) 
write.xlsx(esg_universe, "~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG_Universe_long.xlsx")


## Total company average performance 
total_company <- esg_universe %>% group_by(Year) %>% mutate(Count_year = n()) %>%
        group_by(Year, Count_year) %>% 
        summarize_at(vars(Volatility, Total_Asset, ESG_score, ROA), funs(mean(., na.rm=TRUE))) 
}


# Generate Correlation Matrix 

esg_universe_cor <- esg_universe %>% 
  select(-Name, -Code, -Market, -Industry) 
  
esg_universe_cor$Year <- as.numeric(esg_universe_cor$Year)
correlation_matrix <- cor(esg_universe_cor)
corrplot(correlation_matrix, method = "square", 
         type = "upper", tl.col = "black", 
         order = "hclust", col = brewer.pal(n = 6, name = "RdYlBu"), 
         title = "Correlation Matrix that Shows Correlation between ESG and Other Factors")














