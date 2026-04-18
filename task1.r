library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/Vedant/Desktop/Prodigy Tasks/API_SP.POP.TOTL_DS2_en_csv_v2_58.csv", skip = 4, header = TRUE)

year_cols <- grep("^X[0-9]{4}$", colnames(data), value = TRUE)
latest_year <- tail(year_cols[colSums(!is.na(data[year_cols])) > 0], 1)
cat("Using year column:", latest_year, "\n")

non_countries <- c("World", "IDA & IBRD total", "Low & middle income",
                   "Middle income", "IBRD only", "Upper middle income",
                   "Lower middle income", "Low income", "IDA total",
                   "IDA only", "IDA blend", "Early-demographic dividend",
                   "Late-demographic dividend", "Pre-demographic dividend",
                   "Post-demographic dividend", "East Asia & Pacific",
                   "Europe & Central Asia", "Latin America & Caribbean",
                   "Middle East & North Africa", "North America",
                   "South Asia", "Sub-Saharan Africa",
                   "Euro area", "European Union", "High income",
                   "Fragile and conflict affected situations",
                   "Heavily indebted poor countries (HIPC)",
                   "Least developed countries: UN classification",
                   "Small states", "Other small states",
                   "Pacific island small states", "Caribbean small states")

pop_data <- data %>%
  dplyr::select(Country.Name, all_of(latest_year)) %>%
  dplyr::rename(Country = Country.Name, Population = all_of(latest_year)) %>%
  dplyr::filter(!is.na(Population)) %>%
  dplyr::filter(!Country %in% non_countries) %>%
  dplyr::arrange(desc(Population))

cat("Number of countries:", nrow(pop_data), "\n")

top10 <- head(pop_data, 10)
print(top10)

p1 <- ggplot(top10, aes(x = reorder(Country, -Population), y = Population / 1e9)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(
    title = "Top 10 Most Populous Countries",
    x = "Country",
    y = "Population (Billions)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
ggsave("bar_chart_top10.png", plot = p1, width = 10, height = 6)

p2 <- ggplot(pop_data, aes(x = Population / 1e6)) +
  geom_histogram(bins = 30, fill = "coral", color = "black") +
  labs(
    title = "Distribution of Country Populations",
    x = "Population (Millions)",
    y = "Number of Countries"
  ) +
  theme_minimal()

print(p2)
ggsave("histogram_population.png", plot = p2, width = 10, height = 6)