# Import data  
library(readr)
layoffs <- read_csv("Downloads/layoffs_data(3).csv")

######### TREND ANAYSIS #########
library(ggplot2)   
library(dplyr) 

# Convert 'Date' column to Date format
layoffs$Date <- as.Date(layoffs$Date)

# Aggregate layoffs data by month
layoffs_monthly <- layoffs %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  summarise(Total_Layoffs = sum(Laid_Off_Count))

# Plot trend of layoffs over time with rotated x-axis labels
ggplot(layoffs_monthly, aes(x = YearMonth, y = Total_Layoffs)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Trend of Layoffs Over Time",
       x = "Year-Month",
       y = "Total Layoffs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### SECTOR ANAYSIS #########
library(forcats)   

# Aggregate layoffs data by industry
layoffs_by_sector <- layoffs %>%
  group_by(Industry) %>%
  summarise(Total_Layoffs = sum(Laid_Off_Count, na.rm = TRUE)) %>%
  filter(!is.na(Industry))  

# Plot industry-wise distribution of layoffs
ggplot(layoffs_by_sector, aes(x = fct_reorder(Industry, Total_Layoffs), 
                              y = Total_Layoffs, label = Total_Layoffs)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(size = 3, vjust = -0.5, color = "black") +  
  labs(title = "Industry-wise Distribution of Layoffs",
       x = "Industry",
       y = "Total Layoffs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######### CORRELATION ANAYSIS #########
library(corrplot)  

# Data Cleaning
layoffs$Percentage <- as.numeric(sub("%", "", layoffs$Percentage))

# Correlation Analysis
correlation_matrix <- cor(layoffs[c("Funds_Raised", "Percentage")], use = "pairwise.complete.obs")

# Visualization with corrplot
corrplot(correlation_matrix, method = "color", type = "lower", order = "hclust", 
         addCoef.col = "black", tl.col = "black", tl.srt = 0)


######### GEOGRAPHIC ANAYSIS #########
library(maps) 
library(sf) 
library(rnaturalearth)

# Aggregate data by country
country_data <- layoffs %>%
  group_by(Country) %>%
  summarise(Count = n())

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge world map with country data
world_data <- left_join(world, country_data, by = c("name" = "Country"))

# Plotting
ggplot() +
  geom_sf(data = world_data, aes(fill = Count)) +
  scale_fill_gradient(name = "Count", low = "#66C2A5", high = "#FC8D62") +
  labs(title = "Geographic Distribution of Layoffs by Country",
       fill = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "bottom")

