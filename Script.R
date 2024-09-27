# Required R-Packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(corrplot)
library(explore)

############################### Data Preparation ###############################

## Proccessing of the Weather Data

# Weather datas from 14 different weather station 
# List all .txt files in the directory 
lists <- list.files(pattern = "\\.txt$")

# Mapping filenames to canton names
canton_mapping <- list(
  "climate-reports-tables-homogenized_ALT.txt" = "Uri",
  "climate-reports-tables-homogenized_BAS.txt" = "Basel-Landschaft", 
  "climate-reports-tables-homogenized_BER.txt" = "Bern",
  "climate-reports-tables-homogenized_DAV.txt" = "Graubunden",
  "climate-reports-tables-homogenized_ELM.txt" = "Glarus",
  "climate-reports-tables-homogenized_ENG.txt" = "Obwalden",
  "climate-reports-tables-homogenized_GVE.txt" = "Geneva",
  "climate-reports-tables-homogenized_LUG.txt" = "Ticino",
  "climate-reports-tables-homogenized_LUZ.txt" = "Lucerne",
  "climate-reports-tables-homogenized_NEU.txt" = "Neuchâtel",
  "climate-reports-tables-homogenized_PAY.txt" = "Vaud",
  "climate-reports-tables-homogenized_SIO.txt" = "Valais",
  "climate-reports-tables-homogenized_SMA.txt" = "Zurich",
  "climate-reports-tables-homogenized_STG.txt" = "St. Gallen")

# Initialize an empty list to store datasets
datasets <- list()

# Loop over each file in the list
for (filename in lists) {
  
  # Read the entire file to identify the starting line
  lines <- readLines(filename)
  
  # Find the line number where the word "Year" starts
  start_line <- grep("^Year", lines)[1]
  
  # Read the file into a dataframe, skipping lines up to 'start_line - 1'
  datasets[[filename]] <- read.table(file = filename,
                                     header = TRUE,
                                     skip = start_line  -1)
  
  # Get the corresponding canton name from the mapping
  canton_name <- canton_mapping[[filename]]
  
  # Add the canton name as a new column to the dataset
  datasets[[filename]]$Canton <- canton_name
  
  # Ensure that the dataset has a 'Year' column
  if ("Year" %in% colnames(datasets[[filename]])) {
    
    # Filter the data to include only rows where Year is between 2005 and 2023
    datasets[[filename]] <- subset(datasets[[filename]], Year >= 2005 & 
                                     Year <= 2023)
    
  } else {
    warning(paste("No 'Year' column found in file:", filename))
  }
}

# Check the structure of one dataset to verify the new column
str(datasets[[1]])

# Bind rows (combines them into one large data frame)
d.temp <- bind_rows(datasets) %>%
  as_tibble()
print(d.temp)


## Proccessing of the Tourist arrivals Data

# Skip the first three rows of the excel sheet to remove the titel 
d.hotel <- read_excel("Hotel.new.xlsx", skip = 3)

# Store the column names from the raw 3 (the first 6 columns are without names)
d.hotel.names <- read_excel("Hotel.new.xlsx", skip = 2) %>%
  colnames()
d.hotel.names

colnames(d.hotel.names)

# Create a vector with missing column names for the 1st 6 columns from raw 3/4
#  raw number 4 
add_names <- c("Year","Year_1","Month_num", "Month", "Row_numbers","Canton")

# Join both vector names 
d.hotel.names[1:6] <- add_names

# Assigns the values from d.hotel.names to the d.hotel data frame.
colnames(d.hotel) <- d.hotel.names 

head(d.hotel)
str(d.hotel)

d.hotel_filled <- d.hotel %>%
  fill(Year, Year_1, Month_num, Month, .direction = "down")

write.csv(d.hotel_filled, "transformed.csv",  row.names = FALSE)

# Load the dataset
data <- read.csv("transformed.csv")

# Convert only relevant strings to numerical values without affecting the 
# month and canton
data <- data %>%
  mutate(across(where(is.character) & !c("Month", "Canton"), as.numeric))

# Transform the dataset from wide to long format
d.data_1 <- data %>%
  pivot_longer(
    cols = -c(Year, Year_1, Month_num, Month, Row_numbers, Canton), 
    names_to = "Country", 
    values_to = "Arrivals"
  )

# Check the data frame
head(d.data_1, 10)

# Check the data frame
# Create the 'Date' column in the format dd.mm.yyyy
d.date <- d.data_1 %>%
  mutate(
    Day = "01",  # Assuming day "01" for all rows, or use an existing day column if you have it
    Date = paste(Day, Month_num, Year, sep = ".")
  )

# Convert the Date column to a Date object to ensure it's recognized as a date
d.date$Date <- as.Date(d.date$Date, format = "%d.%m.%Y")

head(d.date)

# Create a 'Season' column based on the 'month_num'
d.season <- d.date %>%
  mutate(Season = case_when(
    Month_num %in% c(12, 1, 2) ~ "Winter",
    Month_num %in% c(3, 4, 5) ~ "Spring",
    Month_num %in% c(6, 7, 8) ~ "Summer",
    Month_num %in% c(9, 10, 11) ~ "Autumn"
  ))


# Create a 'Origin' column based on the 'Country'
d.origin <- d.season %>%
  mutate(Origin = ifelse(Country == "Switzerland", "Domestic", "International"))

str(d.origin)

# Create a 'Continent' column based on the 'Countries'
country_to_continent <- c(
  # Europe
  Switzerland = "Europe", `Baltic.States` = "Europe", Germany = "Europe", France = "Europe", 
  Italy = "Europe", Austria = "Europe", `United.Kingdom` = "Europe", Irland = "Europe", 
  Netherlands = "Europe", Belgium = "Europe", Luxembourg = "Europe", Denmark = "Europe", 
  Sweden = "Europe", Norway = "Europe", Finland = "Europe", Spain = "Europe", Portugal = "Europe", 
  Greece = "Europe", Turkey = "Europe", Liechtenstein = "Europe", Iceland = "Europe", 
  Poland = "Europe", Hungary = "Europe", Belarus = "Europe", Bulgaria = "Europe", 
  Malta = "Europe", Cyprus = "Europe", `Other.European.countries` = "Europe", 
  Estonia = "Europe", Latvia = "Europe", Lithuania = "Europe", `Serbia.and.Montenegro` = "Europe", 
  Croatia = "Europe", Romania = "Europe", Russia = "Europe", Slovakia = "Europe", 
  Slovenia = "Europe", Czechia = "Europe", Ukraine = "Europe", Serbia = "Europe",
  # North America
  `United.States` = "North America", Canada = "North America", Mexico = "North America", 
  `Central.America..Caribbean` = "North America", `Other.Central.American.countries` = "North America",
  # South America
  Chile = "South America", Brazil = "South America", Argentina = "South America", 
  `Other.South.American.countries` = "South America",
  # Africa
  Egypt = "Africa", `Other.North.African.countries` = "Africa", `South.Africa` = "Africa", 
  `Other.African.countries` = "Africa",
  # Asia
  Bahrain = "Asia", Israel = "Asia", India = "Asia", Japan = "Asia", 
  `Other.South.East.Asian.countries` = "Asia", Qatar = "Asia", Kuwait = "Asia", China = "Asia", 
  `Gulf.States` = "Asia", `Hong.Kong` = "Asia", Indonesia = "Asia", `South.Korea` = "Asia", 
  Malaysia = "Asia", Philippines = "Asia", Singapore = "Asia", Oman = "Asia", 
  `Taiwan..Chinese.Taipei.` = "Asia", Thailand = "Asia", `Other.West.Asian.countries` = "Asia", 
  `Saudi.Arabia` = "Asia", `United.Arab.Emirates` = "Asia",
  # Oceania
  Australia = "Oceania", `Australia..New.Zealand..Oceania` = "Oceania", 
  `New.Zealand..Oceania` = "Oceania"
)

d.continent <- d.origin %>% 
  mutate(Continent = country_to_continent[Country])
print(d.continent)

# Check NA and identify rows with any NA values
any(is.na(d.continent$Continent))

# Create the final data frame for the tourist arrivals
d.arrivals <- d.continent[, c("Day","Year","Month_num","Month","Date","Canton",
                              "Country","Arrivals", "Origin", "Continent", "Season")]

# Check dataset  
str(d.arrivals)


############################# Merging the data frame #############################

# Merge the datasets by the ID column 
colnames(d.arrivals)
colnames(d.temp)
d.weather.tourist <- full_join(d.arrivals, d.temp, by = c("Year" = "Year", 
                                                          "Month_num" = "Month", 
                                                          "Canton" = "Canton"))
# Check dataset
d.weather.tourist
unique(d.weather.tourist$Canton)
str(d.weather.tourist)

################################ Data Cleaning #################################

# Check if there are any NA values in the data frame
any(is.na(d.weather.tourist))

# Count the total number of NA values in the data frame
sum(is.na(d.weather.tourist))

# Count the number of NA values in each column
colSums(is.na(d.weather.tourist))

# Remove rows with NA in the 'Arrivals' column
d.clean_data <- d.weather.tourist %>%
  filter(!is.na(Arrivals))

# Check again NA values in each column
colSums(is.na(d.clean_data))

###################### Exploratory Data Analysis (EDA) #########################

## Summary Statistic

# Calculate the summary statistic
summary(d.clean_data[, c("Arrivals", "Temperature", "Precipitation")])

# Set up a 1x2 layout for the boxplot and histogram side by side
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # `oma` adds outer margins for a common title

# Creating boxplot for Arrivals variable
boxplot(d.clean_data$Arrivals, 
        main = "",  # Suppress individual plot title
        xlab = "Tourist Arrivals",
        horizontal = TRUE,   # Makes the box plot horizontal
        col = "lightblue")

# Create the histogram for Arrivals variable
hist_data <- hist(d.clean_data$Arrivals, 
                  main = "",  # Suppress individual plot title
                  xlab = "Tourist Arrivals",
                  col = "lightblue", 
                  yaxt = 'n')  # Suppresses the default Y-axis

# Customize the Y-axis to show values in thousands with 'K' label
y_ticks <- axTicks(2)  # Get default Y-axis tick positions
axis(2, at = y_ticks, labels = paste0(y_ticks / 1000, "K"), las = 1)

# Add Y-axis label
mtext("Frequency", side = 2, line = 3)

# Add a common title in the center of the plot panel
mtext("Tourist Arrivals Distribution (2005-2023)", outer = TRUE, cex = 1.5)


## Temperature Distribution

# Set up a 1x2 layout for the boxplot and histogram side by side
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # `oma` adds outer margins for a common title

# Creating boxplot for Temperature variable
boxplot(d.clean_data$Temperature, 
        main = "",  # Suppress individual plot title
        xlab = "Temperature",
        horizontal = TRUE,   # Makes the box plot horizontal
        col = "lightblue")

# Create the histogram for Precipitation variable
hist_data <- hist(d.clean_data$Temperature, 
                  main = "",  # Suppress individual plot title
                  xlab = "Temperature",
                  col = "lightblue", 
                  yaxt = 'n')  # Suppresses the default Y-axis

# Customize the Y-axis to show values in thousands with 'K' label
y_ticks <- axTicks(2)  # Get default Y-axis tick positions
axis(2, at = y_ticks, labels = paste0(y_ticks / 1000, "K"), las = 1)

# Add Y-axis label
mtext("Frequency", side = 2, line = 3)

# Add a common title in the center of the plot panel
mtext("Temperature Distribution (2005-2023)", outer = TRUE, cex = 1.5)


## Precipitation Distribution

# Set up a 1x2 layout for the boxplot and histogram side by side
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  # `oma` adds outer margins for a common title

# Creating boxplot for Precipitation variable
boxplot(d.clean_data$Precipitation, 
        main = "",  # Suppress individual plot title
        xlab = "Precipitation",
        horizontal = TRUE,   # Makes the box plot horizontal
        col = "lightblue")

# Create the histogram for Precipitation variable
hist_data <- hist(d.clean_data$Precipitation, 
                  main = "",  # Suppress individual plot title
                  xlab = "Precipitation",
                  col = "lightblue", 
                  yaxt = 'n')  # Suppresses the default Y-axis

# Customize the Y-axis to show values in thousands with 'K' label
y_ticks <- axTicks(2)  # Get default Y-axis tick positions
axis(2, at = y_ticks, labels = paste0(y_ticks / 1000, "K"), las = 1)

# Add Y-axis label
mtext("Frequency", side = 2, line = 3)

# Add a common title in the center of the plot panel
mtext("Precipitation Distribution (2005-2023)", outer = TRUE, cex = 1.5)

############################### Visualization #################################

## Analyze arrival trends over time
yearly_trends <- d.clean_data %>%
  group_by(Year) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE))

# Calculate the average total arrivals
avg_total_arrivals <- yearly_trends %>%
  summarise(avg_total_arrivals = mean(Total_Arrivals, na.rm = TRUE)) %>%
  pull(avg_total_arrivals)

# Plot the trend
ggplot(yearly_trends, aes(x = Year, y = Total_Arrivals)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  
  # Add vertical lines for the COVID-19 period (e.g., 2020 and 2021)
  geom_vline(xintercept = 2020, linetype = "dotted", color = "blue", linewidth = 1) +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "blue", linewidth = 1) +
  
  # Add horizontal line for the average total arrivals without showing it in the legend
  geom_hline(aes(yintercept = avg_total_arrivals), linetype = "dashed", 
             color = "red", linewidth = 1, show.legend = FALSE) +
  
  # Add label for the AVG line with comma separators and adjust the position for visibility
  annotate("text", x = 2005.5, y = avg_total_arrivals + 700000,  # Adjust the position above the line
           label = paste0("mean=", format(avg_total_arrivals, big.mark = ",")),
           color = "red", 
           fontface = "bold", size = 5, hjust = 0) +  # Left align the text
  
  # Add label for the COVID-19 period at the level of 20 million arrivals
  annotate("text", x = 2020.5, y = 20000000,  # Set y to 20 million
           label = "Covid-19", color = "blue", fontface = "bold", size = 5) +
  
  # Customize axis lines and remove background grid
  labs(
    title = "Tourist Arrivals Over Time (2005-2023)",
    x = "Year",
    y = "Total Arrivals (Millions)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black"),  # Add black lines to x and y axes
    axis.text = element_text(color = "black"),  # Set axis text color to black
    axis.ticks = element_line(color = "black"),  # Set axis ticks color to black
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels by 45 degrees
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  
  # Ensure the plot starts from 2005 and the x-axis has one-year intervals
  scale_x_continuous(limits = c(2005, max(yearly_trends$Year)), breaks = seq(2005, max(yearly_trends$Year), by = 1), expand = c(0, 0)) +
  
  # Format the y-axis in millions and ensure breaks every 1 million
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"),
                     limits = c(0, 21e6), expand = c(0, 0),  # Set y-axis limits from 0 to 21million
                     breaks = seq(1e6, 21e6, by = 1e6))  # Set y-axis breaks every 1 million


## Yearly Differences in Tourist Arrivals

# Analyze arrival trends over time
d.yearly.trends <- d.clean_data %>%
  group_by(Year) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE))

# Calculate the difference in arrivals year-to-year
d.yearly.trends$Difference_in_Arrivals <- c(NA, diff(d.yearly.trends$Total_Arrivals))

# Remove the first row with NA (since there's no previous year to compare for 2006)
d.yearly.trends <- d.yearly.trends[-1, ]

ggplot(d.yearly.trends, aes(x = Year, y = Difference_in_Arrivals, fill = Difference_in_Arrivals > 0)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(
    values = c("red", "green"), # Red for negative, Green for positive
    labels = c("Negative", "Positive")  # Custom labels for the legend
  ) +   # Red for negative, Green for positive
  geom_text(aes(label = ifelse(Difference_in_Arrivals < 0, 
                               paste0(round(Difference_in_Arrivals / 1e6, 2), "M"), 
                               "")), 
            vjust = 1.1,  # Position the text below the red bars
            color = "black") +  # Text color
  labs(title = "Difference in Arrivals Over Years",
       x = "Year",
       y = "Difference in Arrivals (Millions)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black"),  # Add black lines to x and y axes
    axis.ticks = element_line(color = "black"),  # Set axis ticks color to black
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels by 45 degrees
    plot.title = element_text(hjust = 0.5)) +  
  scale_x_continuous(limits = c(2005, max(d.yearly.trends$Year)), 
                     breaks = seq(min(d.yearly.trends$Year), max(d.yearly.trends$Year), by = 1), 
                     expand = c(0, 0)) +  
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"),  # Y-axis in millions
                     breaks = seq(floor(min(d.yearly.trends$Difference_in_Arrivals) / 1e6) * 1e6, 
                                  ceiling(max(d.yearly.trends$Difference_in_Arrivals) / 1e6) * 1e6, 
                                  by = 1e6))


## Tourist Arrivals by Continent

# Summarize arrivals by Continent from 2005 to 2023
continent_trends <- d.clean_data %>%
  group_by(Continent) %>%
  summarise(Total_Arrivals = sum(Arrivals))


# Bar plot comparing arrivals by continent
ggplot(continent_trends, aes(x = Continent, 
                             y = Total_Arrivals, 
                             fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Tourist Arrivals by Continent (2005-2023)",
    x = "Continent",
    y = "Total Arrivals (Millions)"
  ) +
  
  # Format the y-axis in millions with a 20M interval
  scale_y_continuous(labels = number_format(scale = 1e-6, suffix = "M", big.mark = ","),
                     breaks = seq(0, max(c(260e6, continent_trends$Total_Arrivals)), by = 20e6)) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black"),  # Add X and Y axis lines
    axis.ticks = element_line(color = "black"),  # Add axis ticks
    axis.title.x = element_text(size = 12),  # Display x-axis label
    axis.title.y = element_text(size = 12),  # Display y-axis label
    axis.text.x = element_text(size = 10),  # Show x-axis text labels
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    plot.title = element_text(hjust = 0.5)   # Center the title
  )


## Analyse Trends in the top ten Countries

# Summarize arrivals by top ten country
country_trends <- d.clean_data %>%
  group_by(Country) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE)) %>%
  arrange(desc(Total_Arrivals)) %>% 
  head(10)

# Plot the trend
ggplot(country_trends, aes(x = reorder(Country, Total_Arrivals), 
                           y = Total_Arrivals, 
                           fill = Country)) +
  geom_bar(stat = "identity") +
  
  # Adjust labels to bring them closer to the bars
  geom_text(aes(label = formatC(Total_Arrivals, format = "d", big.mark = ",")),  
            hjust = - 0.01,  # Adjust this value to move labels closer
            color = "black", 
            size = 3.5) +
  
  # Add title and axis labels
  labs(title = "Top 10 Countries by Tourist Arrivals (2005-2023)", 
       x = "Countries",  # Label for x-axis
       y = "Total Arrivals") +     # Label for y-axis
  
  # Remove grid and axis lines, but keep axis titles and text
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_blank(),   # Remove axis lines
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title.x = element_text(size = 12),  # Display x-axis label
    axis.title.y = element_text(size = 12),  # Display y-axis label
    axis.text.x = element_blank(),  # Remove total arrival numbers on the x-axis
    axis.text.y = element_text(size = 10),   # Adjust y-axis text size (country names)
    plot.title = element_text(hjust = 0.5)   # Center the title
  ) +
  
  coord_flip() +  # Flip coordinates for horizontal bars
  
  theme(legend.position = "none") +  # Remove legend
  
  expand_limits(y = max(country_trends$Total_Arrivals) * 1.2)  # Expand y-axis limit slightly for text visibility


## Seasonal Tourism Patterns

# Summarize arrivals by origin and season
origin_season_trends <- d.clean_data %>%
  group_by(Origin, Season) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE))

# Stacked bar plot of seasonal trends by origin with labels, without y-axis 
# values and grid background

ggplot(origin_season_trends, aes(x = Season, 
                                 y = Total_Arrivals, 
                                 fill = Origin)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar
  geom_text(aes(label = scales::comma(Total_Arrivals, accuracy = 1)),  # Add labels
            position = position_stack(vjust = 0.5),  # Position labels inside the stacked bars
            color = "black",  # Set label color for readability
            size = 4) +  # Adjust text size
  labs(
    title = "Seasonal Tourist Arrivals: Domestic vs. International (2005-2023)",
    x = "Season",
    y = "Total Arrivals"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid = element_blank(),  # Remove all grid lines
    panel.background = element_blank(),  # Remove the background panel
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  )


## Tourist Arrivals trend by Origin

# Summarize arrivals by origin and season
origin_year_trends <- d.clean_data %>%
  group_by(Origin, Year) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE), 
            .groups = 'drop') # Ensure 'Year' is preserved


## Domestic vs. International arrival line plot overtime 
ggplot(origin_year_trends %>% filter(Year >= 2005),  # Filter data to include only years from 2005 onwards
       aes(x = Year, 
           y = Total_Arrivals, 
           color = Origin, 
           group = Origin)) +
  geom_line() +
  geom_point() +
  
  # Add labels for 2019 and 2020 with bold text and corresponding color
  geom_text(data = origin_year_trends %>% filter(Year %in% c(2020)),
            aes(label = scales::comma(Total_Arrivals), color = Origin),  # Color according to Origin
            vjust = -1,  # Adjust the position of the labels
            size = 3.5,
            fontface = "bold") +  # Make the text bold
  labs(
    title = "Domestic vs. International Tourist Arrivals Over Time (2005-2023)",
    x = "Year",
    y = "Total Arrivals (Millions)"
  ) +
  scale_x_continuous(limits = c(2005, max(origin_year_trends$Year))) +  # Set the x-axis to start at 2005
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove the background grid
    axis.line.x = element_line(color = "black"),  # Add black x-axis
    axis.line.y = element_line(color = "black"),   # Add black y-axis
    axis.ticks = element_line(color = "black"), # Set axis ticks color to black
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)  # Rotate x-axis labels by 45 degrees
  ) +
  
  # Ensure the plot starts from 2005 and the x-axis has one-year intervals
  scale_x_continuous(limits = c(2005, max(d.yearly.trends$Year)), breaks = seq(2005, max(d.yearly.trends$Year), by = 1), expand = c(0, 0)) +
  
  # Format the y-axis in millions with breaks every 1 million
  scale_y_continuous(breaks = seq(1e6, 11e6, by = 1e6),  # Breaks every 1 million
                     limits = c(0, 11e6), expand = c(0, 0), # Ensure the y-axis ends at 11 million
                     labels = scales::label_number(scale = 1e-6, suffix = "M")
  ) 


## Tourism Distribution by Canton (Domestic vs. International Dynamics)

# Summarize arrivals by canton and origin
origin_canton_trends <- d.clean_data %>%
  group_by(Canton, Origin) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE), 
            .groups = 'drop') # Ensure 'Year' is preserved

# Plotting the total sum of arrivals by canton and origin
ggplot(origin_canton_trends, aes(x = reorder(Canton, -Total_Arrivals), 
                                 y = Total_Arrivals, 
                                 fill = Origin)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # Add title and axis labels
  labs(
    title = "Tourist Arrivals by Canton: Domestic vs. International (2005-2023)",
    x = "Canton",
    y = "Total Arrivals (Millions)"
  ) +
  
  # Format the y-axis in millions with decimal separators
  scale_y_continuous(labels = number_format(scale = 1e-6, suffix = "M", big.mark = ",")) +
  
  # Apply theme settings
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    panel.grid = element_blank(),  # Remove grid background
    axis.line.x = element_line(color = "black"),  # Add black x-axis
    axis.line.y = element_line(color = "black"),   # Add black y-axis
    axis.ticks = element_line(color = "black"), # Set axis ticks color to black
  )


## Comparative Overview of Domestic vs International Tourist Arrivals

# Aggregate and Compare Totals
# Summarize total arrivals by origin
origin_trends <- d.clean_data %>%
  group_by(Origin) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE))

# Bar plot comparing domestic vs international arrivals
ggplot(origin_trends, aes(x = Origin, 
                          y = Total_Arrivals, 
                          fill = Origin)) +
  geom_bar(stat = "identity") +
  
  # Add text labels inside the bars
  geom_text(aes(label = scales::comma(Total_Arrivals)),  # Format the numbers with commas
            vjust = 1.5,  # Position the text inside the bars (adjust this if necessary)
            color = "black",  # Set text color to black
            size = 5) +  # Set text size
  
  labs(
    title = "Total Domestic vs. International Tourist Arrivals",
    x = "Origin",
    y = "Total Arrivals"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid = element_blank(),  # Remove all grid lines
    panel.background = element_blank(),  # Remove the background panel
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  )


## Top Cantons for International Tourists

# Combine data for all seasons fo top ten visited cantons by internationals
all_season_data_i <- d.clean_data %>%
  filter(Origin == "International", Season %in% c("Summer", "Autumn", "Spring", "Winter")) %>%
  group_by(Canton, Season) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE)) %>%
  arrange(desc(Total_Arrivals))


# Select top 10 cantons for each season
top_cantons_per_season <- all_season_data_i %>%
  group_by(Season) %>%
  top_n(10, Total_Arrivals)

ggplot(top_cantons_per_season, aes(x = reorder(Canton, Total_Arrivals), 
                                   y = Total_Arrivals, 
                                   fill = Canton)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Total_Arrivals, accuracy = 1)), 
            hjust = -0.3, 
            color = "black", 
            size = 3.5) +
  labs(
    title = "Top 10 Cantons Visited by International Tourists ",
    x = "Canton",
    y = "Total Arrivals"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis (y-axis after flip) text
    axis.ticks.x = element_blank(),  # Remove x-axis (y-axis after flip) ticks
    axis.text.y = element_text(angle = 0, hjust = 1),  # Rotate y-axis labels (x-axis before flip)
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "none",  # Remove the legend
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank()  # Remove background
  ) +
  coord_flip() +  # Flip the coordinates to make it horizontal
  facet_wrap(~ Season, scales = "free_y") +  # Facet by Season with independent y-axis
  expand_limits(y = max(top_cantons_per_season$Total_Arrivals) * 1.5)  # Add more space to the right


## Top Cantons for Domesitc tourists

# Combine data for all seasons fo top ten visited cantons by domestic
all_season_data <- d.clean_data %>%
  filter(Origin == "Domestic", Season %in% c("Summer", "Autumn", "Spring", "Winter")) %>%
  group_by(Canton, Season) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE)) %>%
  arrange(desc(Total_Arrivals))

# Select top 10 cantons for each season
top_cantons_per_season <- all_season_data %>%
  group_by(Season) %>%
  top_n(10, Total_Arrivals)

ggplot(top_cantons_per_season, aes(x = reorder(Canton, Total_Arrivals), 
                                   y = Total_Arrivals, 
                                   fill = Canton)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Total_Arrivals, accuracy = 1)), 
            hjust = -0.3, 
            color = "black", 
            size = 3.5) +
  labs(
    title = "Top 10 Cantons Visited by Swiss Tourists ",
    x = "Canton",
    y = "Total Arrivals"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis (y-axis after flip) text
    axis.ticks.x = element_blank(),  # Remove x-axis (y-axis after flip) ticks
    axis.text.y = element_text(angle = 0, hjust = 1),  # Rotate y-axis labels (x-axis before flip)
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "none",  # Remove the legend
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank()  # Remove background
  ) +
  coord_flip() +  # Flip the coordinates to make it horizontal
  facet_wrap(~ Season, scales = "free_y") +  # Facet by Season with independent y-axis
  expand_limits(y = max(top_cantons_per_season$Total_Arrivals) * 1.5)  # Add more space to the right


## Attraction of the alpine regions in terms of number of tourists
mountain_region <- c("Graubünden", "Wallis", "Bern", "Uri", "Obwalden", "Nidwalden", "Glarus", "Schwyz", "Tessin", "Waadt", "Fribourg", "St. Gallen")

# Summarize arrivals by Season in mountain region vsother region 

season_region_origin <- d.clean_data %>% 
  mutate(Canton_Type = ifelse(Canton %in% mountain_region, "Alpine regions", 
                              "Urban regions"))
summary_seas_reg_orig <- season_region_origin %>% 
  group_by(Year, Season, Canton_Type, Origin) %>%
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE)) %>%
  ungroup()

# Creating the combined diagram
ggplot(summary_seas_reg_orig, aes(x = Year, y = Total_Arrivals, fill = Origin)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Season ~ Canton_Type) +  #
  labs(
    title = "Tourist Arrivals by Season, Region Type, and Origin",
    x = "Year",
    y = "Total Arrivals",
    fill = "Origin"
  ) +
  # Format the y-axis in millions with decimal separators
  scale_y_continuous(labels = number_format(scale = 1e-5, suffix = "M", big.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 6),  
    legend.key.size = unit(0.5, "cm"),
    axis.text.y = element_text(size = 6)
  )

# Summarize All Arrivals Across All Years:
summary_all_years <- season_region_origin %>%
  group_by(Season, Canton_Type, Origin) %>%  # Group by Season, Canton_Type, and Origin
  summarise(Total_Arrivals = sum(Arrivals, na.rm = TRUE)) %>%  # Summarize the total arrivals
  ungroup()

# Reshape the data so International and Domestic are separate columns
summary_wide <- summary_all_years %>%
  spread(key = Origin, value = Total_Arrivals)  # Convert Origin (Domestic, International) into separate columns

# Calculate the difference between International and Domestic arrivals
summary_wide <- summary_wide %>%
  mutate(Difference = International - Domestic)  # Calculate the difference

# Print the result
print(summary_wide)


## Analyse trends of the weather data 

# To get the boxplots of mean temperatures VS each month of all weather 
# stations from 2005 to 2023:

# Summarize temperature for each canton per month across all years
# Clean the NA values in the columns 'Temperature' and 'Precipitation'
d.clean_weather <- d.weather.tourist %>% 
  filter(!is.na(Temperature) & !is.na(Precipitation))

# Ensure the 'Month' column is a factor with levels ordered from January to December
d.clean_weather <- d.clean_weather%>%
  mutate(Month = factor(Month, levels = c("January", "February", "March", "April",
                                          "May", "June", "July", "August", 
                                          "September", "October", "November", 
                                          "December")))


# Create a boxplot of temperatures by month across all years
ggplot(d.clean_weather, aes(x = Month, y = Temperature)) +  # Add 'fill = Month'
  geom_boxplot() +  # Create a boxplot
  labs(
    title = "Average Temperature Distribution",
    x = "Month",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# To get the boxplots of total precipitation VS each month of all weather 
# stations from 2005 to 2023:

# Create a boxplot of total precipitation by month across all years
ggplot(d.clean_weather, aes(x = Month, y = Precipitation)) +  # Add 'fill = Month'
  geom_boxplot() +  # Create a boxplot
  labs(
    title = "Total Precipitaiton Distribution",
    x = "Month",
    y = "Total Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


############################## Data Modeling ################################

## Exploring the Impact of Temperature on Tourist Arrivals

# Calculate the average temperature and precipitation by Year and Month_num
monthly_averages <- d.weather.tourist %>%
  group_by(Year, Month_num) %>%
  summarize(
    Avg_Temperature = mean(Temperature, na.rm = TRUE),
    Avg_Precipitation = mean(Precipitation, na.rm = TRUE),
    Sum_Arrivals = sum(Arrivals, na.rm = TRUE)
  ) %>%
  ungroup()

# Scatter plot for Arrivals vs. Temperature with regression line
ggplot(monthly_averages, aes(x = Avg_Temperature, y = Sum_Arrivals)) +
  geom_point(alpha = 0.3) +  # Scatter plot points with some transparency
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Regression line
  labs(title = "Impact of Temperature on Tourist Arrivals",
       x = "Average Temperature (°C)",
       y = "Number of Arrivals") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


## Exploring the Impact of Precipitation on Tourist Arrivals

# Scatter plot for Arrivals vs. Precipitation with regression line
ggplot(monthly_averages, aes(x = Avg_Precipitation, y = Sum_Arrivals)) +
  geom_point(alpha = 0.3) +  # Scatter plot points with some transparency
  geom_smooth(method = "lm", se = TRUE, color = "green") +  # Regression line
  labs(title = "Impact of Precipitation on Tourist Arrivals",
       x = "Average Precipitation (mm)",
       y = "Number of Arrivals") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


## Correlation Analysis: Tourist Arrivals in Relation to Temperature and Precipitation

# Select relevant columns
filtered_data <- monthly_averages %>%
  select(Sum_Arrivals, Avg_Temperature, Avg_Precipitation)  

# Calculate the correlation matrix
cor_matrix <- cor(filtered_data, use = "complete.obs")  # Use complete.obs to handle missing values

# Plot the correlation matrix with corrplot
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 0, # Adjust text label color and rotation
         addCoef.col = "black", # Add correlation coefficients in the plot
         col = colorRampPalette(c("red", "white", "blue"))(200))  # Color scale from red (negative) to blue (positive)


## Analyzing Tourist Arrivals through Multiple Linear Regression: 
## Temperature and Precipitation as Predictors

# Fit a multiple regression model with Arrivals as the dependent variable
model <- lm(Sum_Arrivals ~ Avg_Temperature + Avg_Precipitation, 
            data = monthly_averages)

# View the summary of the model
summary(model)
