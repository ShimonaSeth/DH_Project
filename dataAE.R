#loading packages required to clean data
library(tidyverse)
library(purrr)
pacman::p_load(
  rio,        # Importing data  
  here,       # Relative file pathways  
  janitor,    # Data cleaning and tables
  lubridate,  # Working with dates
  matchmaker, # Dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,  # Data management and visualization
  forecast,
  styler,
  sf,
  tmap,
  openStreetMap,
  spdep
)

#loading the csv file
data <- read.csv("adverse_events.csv")
data

#check data set
str(data)

# Remove commas and convert "Count" and "Population" to numeric
data$Count <- as.numeric(gsub(",", "", data$Count))
data$Population <- as.numeric(gsub(",", "", data$Population))

# Convert "Year" to Date format
data$Year <- as.Date(as.character(data$Year), format="%Y")

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values[missing_values > 0])

#Viewing data
summary(data)
view(data)
head(data)
glimpse(data)

#Create plot

# Load necessary libraries
library(ggplot2)
library(RColorBrewer)

# Define the path to the shapefile
tiger_shapefile <- "/Users/shimonaseth/Desktop/rproject/data_AECalifornia/tl_2015_us_county/tl_2015_us_county.shp"

# Read the shapefile
shapefile_data <- st_read(tiger_shapefile)
head(shapefile_data)


#Aggregate data by Year to get the total count
total_count_data <- data %>%
  group_by(Year) %>%
  summarise(total_count = sum(Count))
glimpse(total_count_data)

# Create a visually appealing line plot for the total count over the years
ggplot(total_count_data, aes(x = Year, y = total_count)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Line color and size
  geom_point(color = "#0072B2", size = 3) +  # Point color and size
  labs(title = "Total Count Trend Over the Years", y = "Total Count", x = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_blank(),  # Remove plot border
        axis.line = element_line(color = "black"),  # Set axis line color
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Title styling
        axis.title = element_text(size = 14, face = "bold"))  # Axis label styling

# Group by County, calculate the total count
# Group by County, calculate the total count
county_summary <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(ObsRate)) %>%
  arrange(desc(total_count)) %>%
  top_n(5, total_count)

county_summary <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(ObsRate)) %>%
  arrange(desc(total_count)) %>%
  top_n(5, total_count)

# Use a darker shade of blue from the "Blues" palette
blue_palette <- colorRampPalette(rev(brewer.pal(9, "Blues")))(5)

# Create a bar plot with a range of blue shades(reorders the levels of a factor based on a numeric or character variable, -total_count: The numeric variable by which to reorder the factor levels. The negative sign (-) is used to sort in descending order.)
ggplot(county_summary, aes(x = reorder(County, -total_count), y = total_count, fill = as.factor(County))) +
  geom_bar(stat = "identity", color = "white") + 
  scale_fill_manual(values = blue_palette) +  # Use the defined blue color palette
  labs(title = "Top 5 Counties by Total Count of Adverse Events", y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_line(color = "black"),  # Set axis line color
        panel.grid.major.y = element_line(color = "darkgray"),  # Darken grid lines
        panel.border = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Bold title
        axis.title = element_text(size = 14, face = "bold"))  # Axis label styling

















#plotting top 10 counties by total Count
top_counties <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(Count)) %>%
  top_n(10, total_count)

ggplot(data %>% filter(County %in% top_counties$County), aes(x = County)) +
  geom_bar() +
  labs(title = "Count of Observations by Top 10 Counties", y = "Count")



# Filter and arrange the data for the top 5 counties
top5_counties <- total_count_data %>%
  top_n(5, total_count) %>%
  arrange(desc(total_count))
str(top5_counties)

# Create a visually appealing bar plot for the top 5 counties
ggplot(top5_counties, aes(x = reorder(County, -total_count), y = total_count, fill = County)) +
  geom_bar(stat = "identity", color = "white", size = 1) +  # White border for bars
  labs(title = "Top 5 Counties by Total Count of Adverse Events", y = "Total Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust text orientation
    axis.title = element_text(size = 14, face = "bold"),  # Axis label styling
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Title styling
    legend.position = "none",  # Remove legend for better clarity
    panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed"),  # Dashed gridlines
    panel.border = element_blank()  # Remove plot border
  )

# Filter and arrange the data for the top 5 counties based on total_count
top5_counties <- total_count_data %>%
  group_by(County) %>%
  summarise(total_count = sum(total_count)) %>%
  top_n(5, total_count) %>%
  arrange(desc(total_count))






# analyze the most reported PSI descriptions within the top counties
# Subset data for the top counties
top_counties_data <- data %>%
  filter(County %in% top_counties$County)

# Count the occurrences of each PSI Description within the top counties
psi_counts <- top_counties_data %>%
  group_by(PSIDescription) %>%
  summarise(count = sum(Count))

# Order the data by count in descending order
psi_counts <- psi_counts %>%
  arrange(desc(count))

# Create a bar plot for the top PSI Descriptions
ggplot(psi_counts, aes(x = reorder(PSIDescription, -count), y = count, fill = PSIDescription)) +
  geom_bar(stat = "identity") +
  labs(title = "Top PSI Descriptions in the Top Counties", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))














#Time series plot of Count over the years
ggplot(data, aes(x = Year, y = Count, color = County, group = County)) +
  geom_line() +
  labs(title = "Time Series Analysis by County", y = "Count") +
  theme_minimal() 

# Create categories for ObsRate
data$category <- cut(data$ObsRate, breaks = c(0, 1, 2, 3), labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# Bar plot of Count by ObsRate category
ggplot(data, aes(x = category, fill = category)) +
  geom_bar() +
  labs(title = "Count of Observations by ObsRate Category", x = "ObsRate Category", y = "Count")


# Create a scatter plot of cases by location
ggplot(data, aes(x = longitude, y = latitude, color = PSI_description)) +
  geom_point() +
  labs(title = "Cases by Location in Different Counties",
       x = "Longitude",
       y = "Latitude",
       color = "PSI Description") +
  theme_minimal()








# Aggregate data by year
agg_data <- aggregate(Count ~ Year + PSIDescription, data = data, sum)

# Plot the trend
library(ggplot2)

#Plot to time trend
 ggplot(agg_data, aes(x = Year, y = Count, group = PSIDescription, color = PSIDescription)) +
  geom_line() +
  labs(title = "Trend of Adverse Events Over the Years",
       x = "Year", y = "Count",
       color = "Adverse Event Type") +
  theme_minimal()


#Bar plot for total count of adverse events
ggplot(data, aes(x = PSIDescription, y = Count, fill = PSIDescription)) +
  geom_bar(stat = "sum") +
  labs(title = "Total Counts of Adverse Events",
       x = "Adverse Event",
       y = "Total Count",
       fill = "Adverse Event") +
  theme_minimal()

ggplot(data, aes(x = PSIDescription, y = ObsRate, fill = PSIDescription)) +
  geom_boxplot() +
  labs(title = "Distribution of Observation Rates for Each Adverse Event",
       x = "Adverse Event",
       y = "Observation Rate",
       fill = "Adverse Event") +
  theme_minimal()

ggplot(data, aes(x = Year, y = ObsRate, color = PSIDescription)) +
  geom_line() +
  facet_wrap(~PSIDescription, scales = "free_y") +
  labs(title = "Observation Rate Trend for Each Adverse Event Over Time",
       x = "Year",
       y = "Observation Rate",
       color = "Adverse Event") +
  theme_minimal()

# Loop through unique adverse event types
for (event_type in unique(data$PSIDescription)) {
  
  # Subset data for the current event type
  subset_data <- subset(data, PSIDescription == event_type)
  
  # Bar Plot for Total Counts
  p1 <- ggplot(subset_data, aes(x = Year, y = Count, fill = PSIDescription)) +
    geom_bar(stat = "sum") +
    labs(title = paste("Total Counts of", event_type),
         x = "Year",
         y = "Total Count",
         fill = "Adverse Event") +
    theme_minimal()
  

  # Box Plot for Observation Rates
  p2 <- ggplot(subset_data, aes(x = Year, y = ObsRate)) +
    geom_boxplot() +
    labs(title = paste("Observation Rates of", event_type),
         x = "Year",
         y = "Observation Rate") +
    theme_minimal()
  
  # Line Plot for Observation Rate Trend
  p3 <- ggplot(subset_data, aes(x = Year, y = ObsRate, color = PSIDescription)) +
    geom_line() +
    labs(title = paste("Observation Rate Trend of", event_type),
         x = "Year",
         y = "Observation Rate",
         color = "Adverse Event") +
    theme_minimal()
  
  # Print the plots
  print(p1)
  print(p2)
  print(p3)
  
  # Create a heatmap
  heatmap_plot <- ggplot(data, aes(x = County, y = PSIDescription, fill = Count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +  
    labs(title = "Adverse Events Heatmap", x = "County", y = "PSIDescription", fill = "Count") +
    theme_minimal()
  
  # Print the plot
  print(heatmap_plot)
  df_County <- data$County
  df_County
  
 # Data annalysis by dividing the data by County wise, Yeatwise and PSI Description 
  
  # Aggregate data by Year and calculate the mean ObsRate
  agg_data <- data %>%
    group_by(Year) %>%
    summarise(mean_ObsRate = mean(ObsRate, na.rm = TRUE))
  
  # Create a line plot
  ggplot(agg_data, aes(x = Year, y = mean_ObsRate)) +
    geom_line() +
    labs(title = "Trend of Adverse Events Over Time",
         x = "Year",
         y = "Mean Observation Rate") +
    theme_minimal()
  
  # Convert Year to a factor for better visualization
  df$Year <- as.factor(df$Year)
  
  # Create a line plot with color representing Statewide
  ggplot(df, aes(x = Year, y = ObsRate, color = Statewide)) +
    geom_line() +
    labs(title = "Trend of Adverse Events Over Time",
         x = "Year",
         y = "Observation Rate (per 100,000)") +
    theme_minimal()
 