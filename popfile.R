#libraries
library(tidyverse)
library(RColorBrewer)
library(scales)
library(ggtext)
library(ggplot2)

#Loading the dataset
census_data <- read.csv("C:/Users/mgujj/OneDrive/Desktop/pdata.csv")

# Removing unnecessary columns
censusDataCleaned <- census_data %>%
  select(-c(DGUID, ALT_GEO_CODE, SYMBOL, SYMBOL.1, SYMBOL.2, SYMBOL.3, SYMBOL.4, SYMBOL.5,
            TNR_SF, TNR_LF, DATA_QUALITY_FLAG, CHARACTERISTIC_NOTE))

# Renaming the columns for readability 
censusDataCleaned <- censusDataCleaned %>%
  rename(
    CensusYear = CENSUS_YEAR,
    GeoLevel = GEO_LEVEL,
    GeoName = GEO_NAME,
    CharacteristicID = CHARACTERISTIC_ID,
    CharacteristicName = CHARACTERISTIC_NAME,
    TotalCount = C1_COUNT_TOTAL,
    CountMen = C2_COUNT_MEN.,
    CountWomen = C3_COUNT_WOMEN.,
    TotalRate = C10_RATE_TOTAL,
    RateMen = C11_RATE_MEN.,
    RateWomen = C12_RATE_WOMEN.
  )

# filtering the data for plots
cdc <- censusDataCleaned %>%
  filter(GeoLevel == "Province", CharacteristicID == 1) %>%
  select(GeoName, TotalCount) %>%
  rename(Province = GeoName, Total_Population = TotalCount)

# code for bar plot
ggplot(cdc, aes(x = reorder(Province, -Total_Population), y = Total_Population, fill = Province)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Population by Province in Canada (2021)",
    x = "Province",
    y = "Total Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = nrow(cdc), "Set3"))


# pie chart code
cdc %>%
  mutate(Percentage = Total_Population / sum(Total_Population) * 100) %>%  
  ggplot(aes(x = "", y = Percentage, fill = Province)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Population Distribution by Province in Canada (2021)",
    fill = "Province"
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_stack(vjust = 0.5),
    size = 4  
  ) +
  scale_fill_brewer(palette = "Set3") 


print(censusDataCleaned %>% 
        filter(GeoLevel == "Country", 
               CharacteristicID %in% c(8, 9, 13, 24)) %>%
        select(CharacteristicName, TotalCount))


# retrieving age group data for all provinces in canada
ageDistribution <- censusDataCleaned %>%
  filter(GeoLevel == "Province", 
         CharacteristicID %in% c(8, 9, 13, 24)) %>%
  select(GeoName, CharacteristicName, TotalCount) %>%
  filter(CharacteristicName != "Total - Age groups of the population - 100% data")


ageDistribution <- ageDistribution %>%
  mutate(TotalCountMillions = TotalCount / 1e6)

# bar plot for age distribution by province in millions
ggplot(ageDistribution, aes(x = reorder(GeoName, TotalCountMillions), y = TotalCountMillions, fill = CharacteristicName)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Age Distribution by Province in Canada (2021)",
    x = "Province",
    y = "Population (in Millions)",
    fill = "Age Group"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma)



# selecting education data
educationData <- censusDataCleaned %>%
  filter(GeoLevel == "Country",
         CharacteristicID %in% c(2014, 2015, 2016, 2020, 2021, 2022, 2023, 2024)) %>%
  select(CharacteristicName, TotalCount)

totalPopulation <- educationData$TotalCount[1]
educationData <- educationData %>%
  mutate(Percentage = (TotalCount / totalPopulation) * 100) %>%
  filter(CharacteristicName != "Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data")

# Creating a bar plot
ggplot(educationData, aes(x = reorder(CharacteristicName, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Educational Attainment in Canada (Ages 25-64)",
    x = "Education Level",
    y = "Percentage"
  ) +
  theme(axis.text.y = element_text(size = 8))


wideData <- wideData %>%
  mutate(
    Population_2021 = as.numeric(Population_2021),
    Population_2016 = as.numeric(Population_2016),
    percentageChange = as.numeric(percentageChange)
  )

wideData <- data_unique %>%
  pivot_wider(
    names_from = Character,
    values_from = Total,
    values_fn = function(x) {
      if (length(x) > 1) stop("Duplicate values detected!")
      x
    }
  ) %>%
  rename(
    Population_2021 = `Population, 2021`,
    Population_2016 = `Population, 2016`,
    percentageChange = `Population percentage change, 2016 to 2021`
  )

# Create a bar plot for percentage change
ggplot(wideData, aes(x = reorder(Name, percentageChange), y = percentageChange)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Population Percentage Change (2016 to 2021)",
    x = "Province/Territory",
    y = "Percentage Change"
  ) +
  theme_minimal()



commutingData <- censusDataCleaned %>%
  filter(CharacteristicID %in% c(2604, 2605, 2606, 2607, 2608, 2609)) %>%
  mutate(
    GeoName = as.factor(GeoName),  # Provinces
    CharacteristicID = as.factor(CharacteristicID)  # Modes of commuting
  )

aggregatedData <- commutingData %>%
  group_by(GeoName, CharacteristicID) %>%
  summarise(
    Total_Commuters = sum(TotalCount, na.rm = TRUE),
    Men_Commuters = sum(CountMen, na.rm = TRUE),
    Women_Commuters = sum(CountWomen, na.rm = TRUE),
    .groups = "drop"  
  )

# Calculating transport mode share percentages
modeShare <- commutingData %>%
  group_by(GeoName, CharacteristicID, CharacteristicName) %>%
  summarise(
    Total_Commuters = sum(TotalCount, na.rm = TRUE)
  ) %>%
  group_by(GeoName) %>%
  mutate(
    modeSharePercentage = Total_Commuters / sum(Total_Commuters) * 100
  ) %>%
  ungroup()

modeSharefiltered <- modeShare %>%
  filter(GeoName != "Canada")

# bar plot for modes of transport
ggplot(modeSharefiltered, aes(x = GeoName, y = modeSharePercentage, fill = CharacteristicName)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Mode Share by Province (Excluding Canada)",
    x = "Province",
    y = "Percentage of Commuters",
    fill = "Mode of Transport"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

immigrantsSummary <- censusDataCleaned %>%
  mutate(CharacteristicID = as.numeric(CharacteristicID)) %>%
  filter(GeoLevel == "Province", 
         CharacteristicID %in% c(1530, 1531, 1532, 1533, 1534, 1535, 1536)) %>%
  group_by(GeoName, CharacteristicID) %>%
  summarize(
    totalImmigrants = sum(TotalCount, na.rm = TRUE),
    .groups = "drop" 
  )

# Plot for immigration trends over time
ggplot(immigrantsSummary, aes(x = CharacteristicID, y = totalImmigrants, color = GeoName, group = GeoName)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  labs(
    title = "Immigration Trends by Province (1980-2021)",
    x = "Time Period", 
    y = "Total Immigrants",
    color = "Province"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(1530, 1531, 1532, 1533, 1534, 1535, 1536), 
    labels = c("Before 1980", "1980-1990", "1991-2000", "2001-2010", 
               "2011-2021", "2011-2015", "2016-2021")
  )


