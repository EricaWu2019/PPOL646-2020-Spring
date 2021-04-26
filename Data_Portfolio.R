library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)
library(ggpubr)

# Load data
esg_universe <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/ESG_Universe_long.xlsx")

# Data pre-processing
# Create two new variable that count number of observations each year for difference sectors and markets   
esg_universe <- esg_universe %>% 
  group_by(Industry, Year) %>% mutate(Count_Sector = n()) %>% 
  group_by(Market, Year) %>% mutate(Count_Market = n())   


# Box Plots #
{
  ggplot (data = esg_universe %>% filter(Count_Market >= 25), 
        mapping = aes(x=reorder(Market, ESG_score, FUN = mean, desc = FALSE), y=ESG_score)) +
  geom_boxplot(color = "pink", fill = "pink",  
              alpha=0.6) +
  labs (
    title = "ESG Overall Score of Four Countries in 2019", 
    x = "Countries", 
    y = "ESG Overall Score (2019)", 
    caption = "Source: Thomson Reuters Datastream") +
  theme(  
    plot.title = element_text(size = 16, face = "bold", vjust = 4, color = "steelblue"),
    plot.subtitle = element_text(size = 12.5, face = "italic", vjust = 3.5, color = "steelblue"),
    plot.caption = element_text(face = "italic", vjust = -3),
    axis.title.y = element_text(vjust = 1.5, size=12, face="bold", color = "steelblue"), 
    axis.title.x = element_text(vjust = -2.5, size=12, face="bold", color = "steelblue"),
    plot.margin = unit(rep(1,4), "lines"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )   

sector_box <-
  ggplot (data = esg_universe %>% filter(Count_Sector >= 25), 
          mapping = aes(x=ESG_score, y=reorder(Industry,ESG_score, FUN = mean, .desc = FALSE))) +
    geom_boxplot(color = "steelblue", fill = "steelblue", 
                 alpha=0.6) +
    labs (
      title = "Corporate ESG Scores across Sectors",
      subtitle = "Using ESG score data of 1100 companies from different sectors across 10 years (2010 to 2019), this visulization   ESG scores are calculated based on Using 10-year ESG ", 
      x = "ESG Scores for 10 Years", 
      y = "ESG Overall Score (2019)", 
      caption = "Source: Thomson Reuters Datastream") +
    theme(  
      plot.title = element_text(size = 16, face = "bold", vjust = 4, color = "steelblue"),
      plot.subtitle = element_text(size = 12.5, face = "italic", vjust = 3.5, color = "steelblue"),
      plot.caption = element_text(face = "italic", vjust = -3),
      axis.title.y = element_text(vjust = 1.5, size=12, face="bold", color = "steelblue"), 
      axis.title.x = element_text(vjust = -2.5, size=12, face="bold", color = "steelblue"),
      plot.margin = unit(rep(1,4), "lines"), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )   

ggarrange(country_box, sector_box,
         # labels = c("A", "B", "C"),
         ncol = 1, nrow = 2)


}


# Heat Plot #
library(forcats)
# Data Wrangling 
## Convert industry to factor 
esg_universe$Industry = factor(esg_universe$Industry)
#### Reverse the industry factor to display a ascending order in y-axis 
esg_universe$Industry = fct_rev(esg_universe$Industry)
## Converse year to numeric 
esg_universe$Year = as.numeric(esg_universe$Year) 
## Summarize corporate average performance based on industry & year 
esg_sector_avg <- esg_universe %>% filter(Count_Sector >= 25) %>% ## Keep sectors that have at least 25 annual observations
  group_by(Industry, Year) %>%   
  summarise_at(vars(ESG_score, ROA, Volatility, Total_Asset, Count_Market), funs(mean(., na.rm=TRUE))) 
## Plot the heat map
ggplot(data = esg_sector_avg, 
       mapping = aes(x=Year, y = Industry)) + 
  geom_tile(aes(fill = ESG_score), color = "white") +
  scale_x_continuous(breaks = seq(2010, 2019)) + 
  scale_fill_gradient(low = "steelblue", high = "white") +
  labs(
    title = "Average Environmental,Social, and Governance (ESG) Performance by Sectors Across Past Decades", 
    legend = "ESG Score (Percentage Points)") +
  theme(
    plot.title = element_text(size = 14, face = "bold", vjust=1, hjust = 0.5),
    panel.background = element_blank())

test <- esg_universe %>% filter(Count_Sector >= 25)
unique(test$Name)

# Plot & GDP # 
{
# Data Wrangling
## Load GDP data
countries_gdp <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/countries_gdp.xls")
countries_gdp_2019 <- select(countries_gdp, "Country Name", "2019") 
## Load continent name data
continents <- read_excel("~/Desktop/Fall_2020/Data Vis/Assignment/Data/continents_list.xls", skip = 3) %>%
  select(Country, Region)
## Summarize corporate average performance based on market
esg_score_ctmarket <- esg_universe %>% group_by(Market, Year) %>% mutate(Count_Market=n()) %>% 
  filter(Count_Market >= 15 | Market == "Canada") %>% filter(Year >= 2015) %>% 
  group_by(Market) %>%
  summarise_at(vars(ESG_score, ROA, Volatility, Total_Asset, Count_Market), funs(mean(., na.rm=TRUE))) %>%
  filter(Market != "China")
## Combine ESG data with GDP data
esg_by_gdp <- left_join(esg_score_ctmarket, countries_gdp_2019, by = c("Market" = "Country Name")) %>%
              rename(gdppc2019 = "2019") %>%
              left_join(continents,
                        by = c("Market" = "Country")) %>% na.omit()
sum(esg_by_gdp$Count_Market)
## Plot the scatter plot
ggplot(data=esg_by_gdp, 
       mapping = aes(x=ESG_score, y=ROA, size=gdppc2019)) +
  geom_smooth (method = "lm", se = FALSE, col = "grey", size = 1.5, alpha = 0.3) + 
  geom_point(aes(fill = Region), color = "grey",
             shape=21,  alpha = 0.7) + 
  scale_color_brewer(type='qual')+
  ggrepel::geom_text_repel(aes(label = Market),  # use ggrepel to repel overlapping text labels
                           size = 3, segment.color = NA,
                           point.padding = NA) +
  scale_size_continuous(name="GDP Per Capita (US$)", range=c(6, 20)) +
  labs(
    title = "Average Environmental, Social and Governance (ESG) Score & Return on Asset (ROA)\nof Companies Across Countries,Year 2019", 
    subtitle = "Does companies performance on ESG and financial performance varies based on economic development\n(measured by GDP per Capita) of countries accross continents?",
    caption = "Source: Thomson Reuters Datastream",
    x="3-Year Average ESG Score\nPercentage Points", y="Return on Asset (ROA) Ratio") +
  theme(
    plot.title = element_text(size = 15, face = "bold", vjust = 4, color = "steelblue"),
    plot.subtitle = element_text(size = 12.5, face = "italic", vjust = 3.5, color = "steelblue"),
    plot.caption = element_text(face = "italic", vjust = -3),
    axis.title.y = element_text(vjust = 1.5, size=12, face="bold", color = "steelblue"), 
    axis.title.x = element_text(vjust = -2.5, size=12, face="bold", color = "steelblue"),
    plot.margin = unit(rep(3,4), "lines"), 
    strip.text = element_text(size = 10.5, color = "white", face = "bold.italic"), 
    strip.background = element_rect(fill="steelblue")
  ) +     
  theme_minimal()
}

# ESG vs Return by markets facet_wrap #

# Data Wranggling
## Load Data
esg_universe <- read_excel("ESG_Universe_long.xlsx")
## Keep only last 3-years; Countries that have data of at least 20 companies; ROA > 2  
esg_universe_mkt_20 <- esg_universe %>% filter(Count_Market >= 20) %>% 
                                          filter(Year >= 2017) %>% filter(Market != "Taiwan") %>%
                                          filter(ROA > 2)
unique(esg_universe_mkt_20$Name)

ggplot(data=esg_universe_mkt_15, 
       mapping = aes(x=ESG_score, y=ROA)) +
  geom_point(alpha=0.7, color="steelblue", size=0.5) +  # position = "jitter", size = 2
  facet_wrap(~Market, scales = "free") +
  scale_color_brewer(type='qual') +
  labs(
    title = "ESG Score vs. Return on Assets (ROA) of Companies Across 13 Countries, through Year 2017 to 2019", 
    subtitle = "Does Environmental, Social, and Governance score have a relationship with Earning per Share of a company?",
    caption = "Source: Thomson Reuters Datastream",
    x="Annual ESG Score (Percentage Points)", y="Return on Assets (ROA) Ratio") +
  theme(
    plot.title = element_text(size = 15, face = "bold", vjust = 4, color = "steelblue"),
    plot.subtitle = element_text(size = 12.5, face = "italic", vjust = 3.5, color = "steelblue"),
    plot.caption = element_text(face = "italic", vjust = -3),
    axis.title.y = element_text(vjust = 1.5, size=12, face="bold", color = "steelblue"), 
    axis.title.x = element_text(vjust = -2.5, size=12, face="bold", color = "steelblue"),
    # plot.margin = unit(rep(3,4), "lines"), 
    strip.text = element_text(size = 9, color = "white", face = "bold.italic"), 
    strip.background = element_rect(fill="steelblue")
  ) + 
  geom_smooth(method = "loess", se = FALSE, size = 0.5, color = "orange") 


