## --------------------------------------------------------------------------------------------------
df <- read.csv("shark_tank.csv")
dim(df)
summary(df)


## --------------------------------------------------------------------------------------------------
check_missing <- function(x) {
  sum(is.na(x) | x == ""| x == " " | x == "NA" | x == "N/A" | x == "NULL")*100 / length(x)
}

missing_proportion <- sapply(df, check_missing)
missing_proportion


## --------------------------------------------------------------------------------------------------
duplicate_rows <- df[duplicated(df), ]
print(duplicate_rows)


## --------------------------------------------------------------------------------------------------
library(ggplot2)

deal_distribution <- table(df$deal)
deal_proportion <- prop.table(deal_distribution)

deal_distribution_df <- data.frame(
  Outcome = ifelse(names(deal_distribution) == "TRUE", 
                     "Successful", 
                     "Failed"),
  Percentage = as.numeric(deal_proportion) * 100
)

ggplot(deal_distribution_df, aes(x = "", y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", width = 0.3, color = "black") +
  xlab("") +
  ylab("Percentage of Pitches (%)") +
  ggtitle("Deal Distribution") +
  theme_minimal() +
  scale_fill_manual(
    values = c("Successful" = "skyblue", 
               "Failed" = "coral"),
    name = "Outcome"
  )


## --------------------------------------------------------------------------------------------------
library(stringr)
multi_entr <- df[df$Multiple.Entreprenuers == TRUE, ]
and_indicator <- apply(multi_entr, 1, function(i) any(str_detect(i, "and")))
nrow(multi_entr[!and_indicator,c("Multiple.Entreprenuers", "entrepreneurs")])


## --------------------------------------------------------------------------------------------------
entreprenuers_num_distribution <- table(df$Multiple.Entreprenuers)
entreprenuers_num_proportion <- prop.table(entreprenuers_num_distribution)

entreprenuers_num_df <- data.frame(
  Indicator = ifelse(names(entreprenuers_num_distribution) == "TRUE", 
                     "Multiple Entrepreneurs", 
                     "Single Entrepreneur"),
  Percentage = as.numeric(entreprenuers_num_proportion) * 100
)

ggplot(entreprenuers_num_df, aes(x = "", y = Percentage, fill = Indicator)) +
  geom_bar(stat = "identity", width = 0.3, color = "black") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Proportion of Single vs. Multiple Entrepreneurs") +
  theme_minimal() +
  scale_fill_manual(
    values = c("Single Entrepreneur" = "skyblue", 
               "Multiple Entrepreneurs" = "coral"),
    name = "Type of Pitch"
  )


## --------------------------------------------------------------------------------------------------
library(dplyr)
entrepreneurs_deal_summary <- df %>%
  group_by(Multiple.Entreprenuers, deal) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(
    Indicator = ifelse(Multiple.Entreprenuers == TRUE, 
                       "Multiple Entrepreneurs", 
                       "Single Entrepreneur"),
    DealOutcome = ifelse(deal == TRUE, "Successful", "Failed"),
    Percentage = count / sum(count) * 100
  )

# Plot the proportion by deal outcome
ggplot(entrepreneurs_deal_summary, aes(x = Indicator, y = Percentage, fill = DealOutcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.6) +
  xlab("Type of Pitch") +
  ylab("Percentage (%)") +
  ggtitle("Proportion of Single vs. Multiple Entrepreneurs by Deal Outcome") +
  theme_minimal() +
  scale_fill_manual(
    values = c("Successful" = "skyblue", "Failed" = "coral"),
    name = "Outcome"
  )


## --------------------------------------------------------------------------------------------------
unidentified_multi_entr <- df[df$Multiple.Entreprenuers == FALSE, ]
deliminator_indicator <- apply(unidentified_multi_entr, 1, function(i) {
  str_detect(i["entrepreneurs"], "&") | str_detect(i["entrepreneurs"], ",")
})
unidentified_multi_entr[deliminator_indicator, c("Multiple.Entreprenuers", "entrepreneurs")]


## --------------------------------------------------------------------------------------------------
indices_to_update <- c(4, 77, 80, 174, 491)
df$Multiple.Entreprenuers[indices_to_update] <- TRUE
df[indices_to_update, c("Multiple.Entreprenuers", "entrepreneurs")]


## --------------------------------------------------------------------------------------------------
df$entrepreneurs[indices_to_update] <- str_replace_all(df$entrepreneurs[indices_to_update], pattern = "&", replacement = "and")
df$entrepreneurs[indices_to_update] <- str_replace_all(df$entrepreneurs[indices_to_update], pattern = ",", replacement = " and")

df[indices_to_update, c("Multiple.Entreprenuers", "entrepreneurs")]


## --------------------------------------------------------------------------------------------------
library(tidyr)

df_long <- df %>%
  separate_rows(entrepreneurs, sep = "and", convert = TRUE)

df_long$entrepreneurs <- trimws(df_long$entrepreneurs)
head(df_long)


## --------------------------------------------------------------------------------------------------
individual_frequency <- table(df_long$entrepreneurs)

sorted_individuals <- sort(individual_frequency, decreasing = TRUE)
top_individuals <- data.frame(Entrepreneur = names(sorted_individuals), Frequency = as.vector(sorted_individuals))

top_10_individuals <- head(top_individuals, 10)
top_10_individuals


## --------------------------------------------------------------------------------------------------
individual_frequency <- table(df_long$entrepreneurs)

top_individuals <- head(sort(individual_frequency, decreasing = TRUE), 10)
cat("Top 10 individual entrepreneurs:\n")
print(top_individuals)

ggplot(data.frame(Entrepreneur = names(top_individuals), Frequency = as.numeric(top_individuals)),
       aes(x = reorder(Entrepreneur, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "coral", color = "black") +
  xlab("Entrepreneur") +
  ylab("Frequency") +
  ggtitle("Top 10 Individual Entrepreneurs") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------
df <- df[, !names(df) %in% "entrepreneurs"]


## --------------------------------------------------------------------------------------------------
library(dplyr)

df$shark_list <- apply(df[, c("shark1", "shark2", "shark3", "shark4", "shark5")], 1, paste, collapse = ",")

inconsistent_sharks <- df %>%
  group_by(episode.season) %>%
  summarize(unique_shark_lists = n_distinct(shark_list)) %>%
  filter(unique_shark_lists > 1)

if (nrow(inconsistent_sharks) > 0) {
  inconsistent_rows <- df %>%
    filter(episode.season %in% inconsistent_sharks$episode.season)
  
  print(inconsistent_rows[, c("episode.season", "shark1", "shark2", "shark3", "shark4", "shark5")])
} else {
  print("All episode-season entries have consistent shark lists.")
}


## --------------------------------------------------------------------------------------------------
shark_columns <- c("shark1", "shark2", "shark3", "shark4", "shark5")

shark_combined <- df %>%
  pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
  drop_na(shark) %>%
  mutate(metric = "Participation", value = 1) %>%  
  group_by(shark) %>%
  summarize(participation_frequency = sum(value), .groups = "drop") %>%
  mutate(metric = "Participation", value = participation_frequency) %>%
  bind_rows(
    df %>%
      pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
      filter(deal == TRUE) %>%
      drop_na(shark) %>%
      count(shark, name = "deal_frequency") %>%
      mutate(metric = "Deal Frequency", value = deal_frequency)
  ) %>%
  bind_rows(
    df %>%
      pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
      drop_na(shark) %>%
      group_by(shark) %>%
      summarize(
        total_pitches = n(),
        total_deals = sum(deal, na.rm = TRUE),
        success_rate = total_deals / total_pitches,
        .groups = "drop"
      ) %>%
      mutate(metric = "Success Rate", value = success_rate * 100)  # Convert success rate to percentage
  )

# Plot with facets
ggplot(shark_combined, aes(x = reorder(shark, -value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~metric, scales = "free_y") +
  xlab("Shark") +
  ylab("Metric Value") +
  ggtitle("Shark Metrics (Participation, Deals, and Success Rate)") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(scale = ifelse(max(shark_combined$value) > 100, 1, 0.01)))


## --------------------------------------------------------------------------------------------------
season_episodes <- df %>%
  group_by(season) %>%
  summarize(total_episodes = n_distinct(episode), .groups = "drop")

shark_participation <- df %>%
  pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
  drop_na(shark) %>%
  group_by(shark, season) %>%
  summarize(episodes_participated = n_distinct(episode), .groups = "drop") %>%
  left_join(season_episodes, by = "season") %>%
  mutate(participation_rate = (episodes_participated / total_episodes) * 100)

shark_deals_season <- df %>%
  pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
  drop_na(shark) %>%
  group_by(shark, season, deal) %>%
  summarize(frequency = n(), .groups = "drop") %>%
  mutate(deal_outcome = ifelse(deal == TRUE, "Successful", "Failed")) %>%
  group_by(shark, season) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

max_successful_frequency <- max(shark_deals_season$frequency[shark_deals_season$deal_outcome == "Successful"])
successful_deals_normalized <- shark_deals_season %>%
  filter(deal_outcome == "Successful") %>%
  mutate(normalized_frequency = (frequency / max_successful_frequency) * 100)

ggplot(shark_deals_season, aes(x = as.factor(season), y = percentage, fill = deal_outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_line(data = successful_deals_normalized, aes(x = as.factor(season), y = normalized_frequency, group = shark),
            color = "red", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = successful_deals_normalized, aes(x = as.factor(season), y = normalized_frequency),
             color = "red", size = 2, inherit.aes = FALSE) +
  geom_line(data = shark_participation, aes(x = as.factor(season), y = participation_rate, group = shark),
            color = "blue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = shark_participation, aes(x = as.factor(season), y = participation_rate),
             color = "blue", size = 2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Successful" = "skyblue", "Failed" = "coral"), name = "Deal Outcome") +
  xlab("Season") +
  ylab("Percentage of Pitches (%)") +
  ggtitle("Shark Success Rate, Participation, and Frequency by Season") +
  theme_minimal() +
  facet_wrap(~shark, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Participation Rate & Frequency (Scaled to 100%)"))


## --------------------------------------------------------------------------------------------------
deals_by_shark_episode <- df %>%
  pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
  drop_na(shark) %>%
  filter(deal == TRUE) %>%  # Only consider successful deals
  group_by(shark, episode.season) %>%
  summarize(
    total_deals = n(),
    .groups = "drop"
  )

ggplot(deals_by_shark_episode, aes(x = episode.season, y = total_deals, fill = as.factor(episode.season))) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Episode.Season") +
  ylab("Number of Deals") +
  ggtitle("Deals by Shark by Episode.Season (Faceted)") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Episode.Season") +
  facet_wrap(~shark, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"  # Remove the legend
  )


## --------------------------------------------------------------------------------------------------
shark_trend <- df %>%
  pivot_longer(
    cols = starts_with("shark"),  
    names_to = "shark_slot",      
    values_to = "shark"           
  ) 
  
shark_presence_heatmap <- shark_trend %>%
  distinct(episode.season, shark) %>%  
  mutate(Presence = 1) %>%            
  complete(episode.season, shark, fill = list(Presence = 0))

shark_success_rate_time <- df %>%
  pivot_longer(
    cols = starts_with("shark"),  
    names_to = "shark_slot",      
    values_to = "shark"           
  ) %>%
  drop_na(shark) %>%
  group_by(shark, episode.season) %>%
  summarize(
    total_pitches = n(),
    total_deals = sum(deal, na.rm = TRUE),
    success_rate = total_deals / total_pitches,
    .groups = "drop"
  )

ggplot(shark_success_rate_time, aes(x = episode.season, y = success_rate, fill = shark)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Episode.Season") +
  ylab("Success Rate (%)") +
  ggtitle("Shark Success Rate Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  facet_wrap(~shark, scales = "free_y")


## --------------------------------------------------------------------------------------------------
season_deals <- df %>%
  group_by(season, deal) %>%
  summarize(frequency = n(), .groups = "drop") %>%
  mutate(deal_outcome = ifelse(deal == TRUE, "Successful", "Failed"))

season_deals <- season_deals %>%
  group_by(season) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

successful_deals_season <- season_deals %>%
  filter(deal_outcome == "Successful") %>% 
  group_by(season) %>%
  summarize(total_successful_frequency = sum(frequency), .groups = "drop") %>%
  mutate(normalized_successful_frequency = (total_successful_frequency / max(total_successful_frequency)) * 100)  # Normalize

ggplot(season_deals, aes(x = as.factor(season), y = percentage, fill = deal_outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.7) +
  geom_line(data = successful_deals_season, aes(x = as.factor(season), y = normalized_successful_frequency, group = 1),
            color = "darkblue", size = 1, inherit.aes = FALSE) +
  geom_point(data = successful_deals_season, aes(x = as.factor(season), y = normalized_successful_frequency),
             color = "darkblue", size = 2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Successful" = "skyblue", "Failed" = "coral"), name = "Deal Outcome") +
  xlab("Season") +
  ylab("Percentage of Pitches (%)") +
  ggtitle("Deal Distribution and Frequency of Successful Deals by Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Successful Deals Frequency (Scaled to 100%)", labels = scales::percent))


## --------------------------------------------------------------------------------------------------
episode_deals <- df %>%
  group_by(season, episode, deal) %>%
  summarize(frequency = n(), .groups = "drop") %>%
  mutate(deal_outcome = ifelse(deal == TRUE, "Successful", "Failed"))

episode_deals <- episode_deals %>%
  group_by(season, episode) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

successful_deals_episode <- episode_deals %>%
  filter(deal_outcome == "Successful") %>%  
  group_by(season, episode) %>%
  summarize(successful_deals_frequency = sum(frequency), .groups = "drop")

ggplot(episode_deals, aes(x = as.factor(episode), y = percentage, fill = deal_outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.7) +
  geom_line(data = successful_deals_episode, aes(x = as.factor(episode), y = successful_deals_frequency / max(successful_deals_frequency) * 100, group = 1),
            color = "darkblue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = successful_deals_episode, aes(x = as.factor(episode), y = successful_deals_frequency / max(successful_deals_frequency) * 100),
             color = "darkblue", size = 2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Successful" = "skyblue", "Failed" = "coral"), name = "Deal Outcome") +
  xlab("Episode") +
  ylab("Percentage of Pitches (%)") +
  ggtitle("Success Rate and Frequency of Successful Deals Across Episodes by Season") +
  theme_minimal() +
  facet_wrap(~season, scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Frequency of Successful Deals (Scaled to 100%)", labels = scales::percent))


## --------------------------------------------------------------------------------------------------
calculated_episode_season <- paste(df$season, df$episode, sep = "-")
mismatches <- df[calculated_episode_season != df$episode.season, ]

mismatches[, c("episode", "season", "episode.season")]


## --------------------------------------------------------------------------------------------------
df$episode.season <- factor(df$episode.season, levels = unique(df$episode.season), ordered = TRUE)

deal_trend <- df %>%
  group_by(episode.season) %>%
  summarize(
    Total_Pitches = n(),
    Total_Deals = sum(deal, na.rm = TRUE),
    Success_Rate = mean(deal, na.rm = TRUE) * 100
  )

ggplot(deal_trend, aes(x = episode.season, y = Success_Rate, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  xlab("Episode.Season") +
  ylab("Success Rate (%)") +
  ggtitle("Deal Success Rate Over Time (By Episode.Season)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate labels for readability


## --------------------------------------------------------------------------------------------------
top_10_locations <- df %>%
  count(location, name = "total_pitches") %>%
  arrange(desc(total_pitches)) %>%
  dplyr::slice(1:10)

top_locations_deals <- df %>%
  filter(location %in% top_10_locations$location) %>%
  group_by(location, deal) %>%
  summarize(frequency = n(), .groups = "drop") %>%
  mutate(deal_outcome = ifelse(deal == TRUE, "Successful", "Failed"))

top_locations_deals <- top_locations_deals %>%
  group_by(location) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

max_successful_frequency <- max(top_locations_deals$frequency[top_locations_deals$deal_outcome == "Successful"])

successful_deals_normalized <- top_locations_deals %>%
  filter(deal_outcome == "Successful") %>%
  mutate(normalized_frequency = (frequency / max_successful_frequency) * 100)

ggplot(top_locations_deals, aes(x = reorder(location, -frequency), y = percentage, fill = deal_outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.7) +
  geom_line(data = successful_deals_normalized, aes(x = reorder(location, -frequency), y = normalized_frequency, group = 1),
            color = "darkblue", size = 1.2, inherit.aes = FALSE) +
  geom_point(data = successful_deals_normalized, aes(x = reorder(location, -frequency), y = normalized_frequency),
             color = "darkblue", size = 2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Successful" = "skyblue", "Failed" = "coral"), name = "Deal Outcome") +
  xlab("Top 10 Locations") +
  ylab("Percentage of Pitches (%)") +
  ggtitle("Deal Distribution and Scaled Frequency of Successful Deals by Location") +
  theme_minimal() +
  coord_flip() + 
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Scaled Frequency of Successful Deals (0-100%)")
  )


## --------------------------------------------------------------------------------------------------
Northeast_New_England <- c("CT", "ME", "MA", "NH", "RI", "VT")
Northeast_Mid_Atlantic <- c("NJ", "NY", "PA")
Midwest_East_North_Central <- c("IL", "IN", "MI", "OH", "WI")
Midwest_West_North_Central <- c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
South_South_Atlantic <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "DC")
South_East_South_Central <- c("AL", "KY", "MS", "TN")
South_West_South_Central <- c("AR", "LA", "OK", "TX")
West_Mountain <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY")
West_Pacific <- c("AK", "CA", "HI", "OR", "WA")

division_mapping <- data.frame(
  state_abbr = c(
    Northeast_New_England, Northeast_Mid_Atlantic,
    Midwest_East_North_Central, Midwest_West_North_Central,
    South_South_Atlantic, South_East_South_Central, South_West_South_Central,
    West_Mountain, West_Pacific
  ),
  division = c(
    rep("Northeast - New England", length(Northeast_New_England)),
    rep("Northeast - Mid Atlantic", length(Northeast_Mid_Atlantic)),
    rep("Midwest - East North Central", length(Midwest_East_North_Central)),
    rep("Midwest - West North Central", length(Midwest_West_North_Central)),
    rep("South - South Atlantic", length(South_South_Atlantic)),
    rep("South - East South Central", length(South_East_South_Central)),
    rep("South - West South Central", length(South_West_South_Central)),
    rep("West - Mountain", length(West_Mountain)),
    rep("West - Pacific", length(West_Pacific))
  )
)

df_copy <- df
df_copy <- df_copy %>%
  separate(location, into = c("city", "state_abbr"), sep = ", ", remove = FALSE)

df_copy <- df_copy %>%
  left_join(division_mapping, by = "state_abbr")

df_copy <- df_copy %>%
  mutate(location = division) %>% 
  select(-division) 

df$location <- df_copy$location

division_summary <- df %>%
  count(location, name = "frequency") %>%
  arrange(desc(frequency))

ggplot(division_summary, aes(x = reorder(location, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "coral", color = "black") +
  xlab("Census Division") +
  ylab("Frequency of Pitches") +
  ggtitle("Frequency of Pitches by Census Division") +
  theme_minimal() +
  coord_flip()


## --------------------------------------------------------------------------------------------------
locations_season_deals <- df %>%
  group_by(location, season, deal) %>%
  summarize(frequency = n(), .groups = "drop") %>%
  mutate(deal_outcome = ifelse(deal == TRUE, "Successful", "Failed"))

locations_season_deals <- locations_season_deals %>%
  group_by(location, season) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

total_pitches <- locations_season_deals %>%
  group_by(location, season) %>%
  summarize(total_frequency = sum(frequency), .groups = "drop")

ggplot(locations_season_deals, aes(x = as.factor(season), y = percentage, fill = deal_outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.7) +
  geom_line(data = total_pitches, aes(x = as.factor(season), y = total_frequency / max(total_frequency) * 100, group = 1),
            color = "darkblue", size = 1, inherit.aes = FALSE) +
  geom_point(data = total_pitches, aes(x = as.factor(season), y = total_frequency / max(total_frequency) * 100),
             color = "darkblue", size = 2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Successful" = "skyblue", "Failed" = "coral"), name = "Deal Outcome") +
  xlab("Season") +
  ylab("Percentage of Pitches (%)") +
  ggtitle("Deal Distribution and Frequency by Census Division Across Seasons") +
  theme_minimal() +
  facet_wrap(~ location, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Frequency (Scaled to 100%)", labels = scales::percent))


## --------------------------------------------------------------------------------------------------
category_deals <- df %>%
  group_by(category, deal) %>%
  summarize(frequency = n(), .groups = "drop") %>%
  mutate(deal_outcome = ifelse(deal == TRUE, "Successful", "Failed"))

category_deals <- category_deals %>%
  group_by(category) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

category_frequency <- df %>%
  group_by(category) %>%
  summarize(total_frequency = n(), .groups = "drop")

category_deals <- category_deals %>%
  left_join(category_frequency, by = "category")

ggplot(category_deals, aes(x = reorder(category, -total_frequency), y = percentage, fill = deal_outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_line(
    aes(x = reorder(category, -total_frequency), y = total_frequency / max(total_frequency) * 100, group = 1),
    color = "blue", size = 1, linetype = "dashed"
  ) +
  geom_point(
    aes(x = reorder(category, -total_frequency), y = total_frequency / max(total_frequency) * 100),
    color = "blue", size = 2
  ) +
  scale_fill_manual(values = c("Successful" = "skyblue", "Failed" = "coral"), name = "Deal Outcome") +
  scale_y_continuous(
    name = "Percentage of Pitches (%)",
    sec.axis = sec_axis(~ . * max(category_frequency$total_frequency) / 100, name = "Frequency of Pitches")
  ) +
  xlab("Category") +
  ggtitle("Deal Distribution by Category with Frequency") +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )


## --------------------------------------------------------------------------------------------------
selected_categories <- c(
  "Specialty Food", "Novelties", "Baby and Children's Care",
  "Cycling", "Home Improvement", "Storage and Cleaning Products",
  "Personal Care and Cosmetics", "Baby and Children's Apparel and Accessories"
)

df_selected <- df %>% filter(category %in% selected_categories)

category_trend <- df_selected %>%
  group_by(category, season) %>%
  summarize(
    total_pitches = n(),
    successful_pitches = sum(deal, na.rm = TRUE),
    success_rate = (successful_pitches / total_pitches) * 100,
    .groups = "drop"
  )

ggplot(category_trend, aes(x = as.factor(season), y = success_rate, group = category, color = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  xlab("Season") +
  ylab("Success Rate (%)") +
  ggtitle("Category Success Rate Over Seasons (Updated Selection)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## --------------------------------------------------------------------------------------------------
shark_category_cleaned <- df %>%
  pivot_longer(cols = starts_with("shark"), names_to = "shark_slot", values_to = "shark") %>%
  filter(!is.na(shark), deal == TRUE) %>%
  separate_rows(shark, sep = ",") %>%  # Split multiple sharks into individual rows
  group_by(shark, category) %>%
  summarize(deals = n(), .groups = "drop") %>%
  arrange(shark, desc(deals))

shark_category_percent <- shark_category_cleaned %>%
  group_by(shark) %>%
  mutate(total_deals = sum(deals), percentage = (deals / total_deals) * 100) %>%
  ungroup()

shark_top10_percent <- shark_category_percent %>%
  group_by(shark) %>%
  slice_max(order_by = percentage, n = 10) %>%
  ungroup()

ggplot(shark_top10_percent, aes(x = reorder(category, -percentage), y = percentage, fill = shark)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  xlab("Category") +
  ylab("Percentage of Deals (%)") +
  ggtitle("Shark Preferences by Category (Top 10 by Percentage for Each Shark)") +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ shark, scales = "free_y")


## --------------------------------------------------------------------------------------------------
df_selected <- df %>% filter(category %in% c(
  "Specialty Food", "Storage and Cleaning Products", "Baby and Child Care",
  "Novelties", "Baby and Children's Apparel and Accessories",
  "Personal Care and Cosmetics", "Outdoor Recreation", "Online Services", 
  "Fitness Apparel and Accessories"
))

category_location <- df_selected %>%
  group_by(category, location) %>%
  summarize(
    successful_pitches = sum(deal, na.rm = TRUE),
    total_pitches_in_category = sum(deal[!is.na(deal)]), 
    .groups = "drop"
  ) %>%
  mutate(percentage = (successful_pitches / sum(successful_pitches)) * 100) %>%
  ungroup()

ggplot(category_location, aes(x = reorder(category, -percentage), y = percentage, fill = location)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  xlab("Category") +
  ylab("Percentage of Successful Pitches (%)") +
  ggtitle("Distribution of Successful Deals by Location and Category") +
  theme_minimal() +
  coord_flip()


## --------------------------------------------------------------------------------------------------
table(df$category)



## --------------------------------------------------------------------------------------------------
library(cluster)
library(factoextra)

shark_columns <- c("shark1", "shark2", "shark3", "shark4", "shark5")

category_features <- df %>%
  pivot_longer(cols = all_of(shark_columns), names_to = "shark_slot", values_to = "shark") %>%
  drop_na(shark) %>%  # Remove rows without a shark
  group_by(category) %>%
  summarize(
    success_rate = sum(deal, na.rm = TRUE) / n() * 100,   # Success rate
    frequency = n(),                                      # Total pitches
    region_distribution = n_distinct(location),           # Distribution across regions
    avg_episode_presence = n_distinct(episode) / n_distinct(season), # Average Episode Presence
    unique_sharks = n_distinct(shark),                    # Count unique sharks
    avg_askedFor = mean(askedFor, na.rm = TRUE),          # Average askedFor
    avg_requested_equity = mean(exchangeForStake, na.rm = TRUE), # Average Equity
    .groups = "drop"
  )

category_features


## --------------------------------------------------------------------------------------------------
category_features_normalized <- category_features %>%
  select(-category) %>% 
  mutate(across(everything(), scale))


## --------------------------------------------------------------------------------------------------
dist_matrix <- dist(category_features_normalized)

hclust_model <- hclust(dist_matrix, method = "ward.D2") 

library(cluster)
library(factoextra)

silhouette_scores <- function(k) {
  cluster_assignments <- cutree(hclust_model, k = k)
  silhouette_result <- silhouette(cluster_assignments, dist_matrix) 
  mean(silhouette_result[, 3])
}

k_values <- 2:10
avg_silhouette_widths <- sapply(k_values, silhouette_scores)

plot(k_values, avg_silhouette_widths, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width",
     main = "Silhouette Analysis for Optimal k")

optimal_k <- k_values[which.max(avg_silhouette_widths)]
cat("Optimal number of clusters (k):", optimal_k, "\n")

category_features$cluster <- cutree(hclust_model, k = optimal_k)

plot(hclust_model, labels = category_features$category, main = "Dendrogram for Categories")
rect.hclust(hclust_model, k = optimal_k, border = 2:4) 


## --------------------------------------------------------------------------------------------------
grouped_metrics_hclust <- category_features %>%
  group_by(cluster) %>%
  summarise(
    Average_Asked_For = mean(avg_askedFor, na.rm = TRUE),
    Average_Requested_Equity = mean(avg_requested_equity, na.rm = TRUE),
    Region_Distribution = mean(region_distribution, na.rm = TRUE),
    Frequency = mean(frequency, na.rm = TRUE),
    Average_Episode_Presence = mean(avg_episode_presence, na.rm = TRUE),
    Unique_Sharks = mean(unique_sharks, na.rm = TRUE)
  )

grouped_metrics_long <- grouped_metrics_hclust %>%
  pivot_longer(
    cols = c(Average_Asked_For, Average_Requested_Equity, Region_Distribution,
             Frequency, Average_Episode_Presence, Unique_Sharks),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(grouped_metrics_long, aes(x = as.factor(cluster), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Metrics by Cluster",
    x = "Cluster",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2)


## --------------------------------------------------------------------------------------------------
grouped_metrics_hclust <- category_features %>%
  group_by(cluster) %>%
  summarise(
    Average_Asked_For = mean(avg_askedFor, na.rm = TRUE),
    Average_Requested_Equity = mean(avg_requested_equity, na.rm = TRUE),
    Region_Distribution = mean(region_distribution, na.rm = TRUE),
    Frequency = mean(frequency, na.rm = TRUE),
    Average_Episode_Presence = mean(avg_episode_presence, na.rm = TRUE),
    Unique_Sharks = mean(unique_sharks, na.rm = TRUE)
  )

cluster_interpretation <- grouped_metrics_hclust %>%
  mutate(
    Interpretation = paste0(
      "Cluster ", cluster, ": ",
      "Average Asked For: $", round(Average_Asked_For, 2), ", ",
      "Average Requested Equity: ", round(Average_Requested_Equity, 2), "%, ",
      "Region Distribution: ", round(Region_Distribution, 2), " regions on average, ",
      "Frequency: ", round(Frequency, 2), " pitches on average, ",
      "Average Episode Presence: ", round(Average_Episode_Presence, 2), ", ",
      "Unique Sharks: ", round(Unique_Sharks, 2)
    )
  )

for (i in 1:nrow(cluster_interpretation)) {
  cat("**Cluster", cluster_interpretation$cluster[i], "**\n")
  cat("Average Asked For: $", format(round(cluster_interpretation$Average_Asked_For[i], 2), big.mark = ","), "\n", sep = "")
  cat("Average Requested Equity: ", round(cluster_interpretation$Average_Requested_Equity[i], 2), "%\n", sep = "")
  cat("Region Distribution: ", round(cluster_interpretation$Region_Distribution[i], 2), " regions on average\n", sep = "")
  cat("Frequency: ", round(cluster_interpretation$Frequency[i], 2), " pitches on average\n", sep = "")
  cat("Average Episode Presence: ", round(cluster_interpretation$Average_Episode_Presence[i], 2), "\n", sep = "")
  cat("Unique Sharks: ", round(cluster_interpretation$Unique_Sharks[i], 2), "\n\n", sep = "")
}


## --------------------------------------------------------------------------------------------------
categories_by_cluster <- category_features %>%
  group_by(cluster) %>%
  summarise(Categories = paste(category, collapse = ", "))

for (i in 1:nrow(categories_by_cluster)) {
  cat("Cluster:", categories_by_cluster$cluster[i], "\n")
  cat("Categories:", categories_by_cluster$Categories[i], "\n\n")
}


## --------------------------------------------------------------------------------------------------
feature_matrix <- category_features %>%
  select(-category) %>%
  mutate(across(everything(), scale))

pca_result <- prcomp(feature_matrix, center = TRUE, scale. = TRUE)

summary(pca_result)

library(factoextra)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

fviz_pca_biplot(
  pca_result, 
  label = "var",           
  repel = TRUE,            
  col.var = "blue",        
  col.ind = "red",         
  geom.ind = "point",      
  addEllipses = TRUE       
)

pca_scores <- as.data.frame(pca_result$x)
category_features <- cbind(category_features, pca_scores)

head(category_features)



## --------------------------------------------------------------------------------------------------
absolute_pca_scores <- pca_scores %>%
  select(PC1, PC2, PC3, PC4) %>%
  mutate(across(everything(), abs))

category_features <- category_features %>%
  mutate(dominant_PC = apply(absolute_pca_scores, 1, function(row) {
    colnames(absolute_pca_scores)[which.max(row)]
  }))

grouped_metrics <- category_features %>%
  group_by(dominant_PC) %>%
  summarise(
    Average_Asked_For = mean(avg_askedFor, na.rm = TRUE),
    Average_Requested_Equity = mean(avg_requested_equity, na.rm = TRUE),
    Region_Distribution = mean(region_distribution, na.rm = TRUE),
    Frequency = mean(frequency, na.rm = TRUE),
    Average_Episode_Presence = mean(avg_episode_presence, na.rm = TRUE),
    Unique_Sharks = mean(unique_sharks, na.rm = TRUE)
  )

grouped_metrics_long <- grouped_metrics %>%
  pivot_longer(
    cols = c(Average_Asked_For, Average_Requested_Equity, Region_Distribution, 
             Frequency, Average_Episode_Presence, Unique_Sharks),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(grouped_metrics_long, aes(x = dominant_PC, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Metrics by Dominant Principal Component",
    x = "Dominant Principal Component",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2)


## --------------------------------------------------------------------------------------------------
categories_by_PC <- category_features %>%
  group_by(dominant_PC) %>%
  summarise(Categories = paste(category, collapse = ", "))

for (i in 1:nrow(categories_by_PC)) {
  cat("Dominant PC:", categories_by_PC$dominant_PC[i], "\n")
  cat("Categories:", categories_by_PC$Categories[i], "\n\n")
}


## --------------------------------------------------------------------------------------------------
if (!"cluster" %in% colnames(category_features)) {
  category_features$cluster <- cutree(hclust_model, k = optimal_k)  
}

if (!"dominant_PC" %in% colnames(category_features)) {
  absolute_pca_scores <- pca_scores %>%
    select(PC1, PC2, PC3, PC4) %>%
    mutate(across(everything(), abs)) 

  category_features <- category_features %>%
    mutate(dominant_PC = apply(absolute_pca_scores, 1, function(row) {
      colnames(absolute_pca_scores)[which.max(row)]
    }))
}

category_features_with_tags <- category_features %>%
  select(category, dominant_PC, cluster)

df_with_tags <- df %>%
  left_join(category_features_with_tags, by = "category")

head(df_with_tags)


## --------------------------------------------------------------------------------------------------
distribution_table <- table(df_with_tags$cluster, df_with_tags$dominant_PC)
print(distribution_table)


## --------------------------------------------------------------------------------------------------
df_with_tags <- df_with_tags %>%
  mutate(
    cluster = ifelse(category %in% selected_categories, category, cluster),
    dominant_PC = ifelse(category %in% selected_categories, category, dominant_PC)
  )


## --------------------------------------------------------------------------------------------------
library(tm)      
library(textstem)

preprocess_text <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "\\d+", "") 
  text <- str_replace_all(text, "[[:punct:]]", "") 
  text <- str_replace_all(text, "[^a-z\\s]", "") 
  text <- removeWords(text, stopwords("en")) 
  text <- lemmatize_strings(text) 
  text <- str_squish(text)
  
  return(text)
}

df_with_tags <- df_with_tags %>%
  mutate(description_cleaned = preprocess_text(description))

df_with_tags <- df_with_tags %>%
  mutate(description_tokens = str_split(description_cleaned, "\\s+"))

head(df_with_tags)


## --------------------------------------------------------------------------------------------------
colnames(hedonometer_dict)


## --------------------------------------------------------------------------------------------------
library(tidytext)
library(readr)
library(dplyr)
library(tidyr)

nrc_dict_unique <- get_sentiments("nrc") %>%
  group_by(word) %>%
  dplyr::slice(1) %>%
  ungroup()

tokenized_data <- df_with_tags %>%
  unnest(description_tokens) %>%
  rename(word = description_tokens) %>%  
  mutate(word = trimws(word))

hedonometer_dict <- read_csv("Hedonometer.csv")
hedonometer_counts <- tokenized_data %>%
  inner_join(
    hedonometer_dict %>% distinct(Word, .keep_all = TRUE),
    by = c("word" = "Word")
  ) %>%
  group_by(title) %>%
  summarise(
    Happiness_Positive = sum(`Happiness Score`[`Happiness Score` > 5], na.rm = TRUE),
    Happiness_Negative = sum(`Happiness Score`[`Happiness Score` < 5], na.rm = TRUE),
    Word_Count = n_distinct(word)
  ) %>%
  mutate(
    Happiness_Positive_Ratio = Happiness_Positive / Word_Count,
    Happiness_Negative_Ratio = Happiness_Negative / Word_Count
  )

nrc_counts <- tokenized_data %>%
  inner_join(nrc_dict_unique, by = "word") %>%
  count(title, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  mutate(
    Positive_Sociability = sum(c_across(joy:trust), na.rm = TRUE),
    Negative_Sociability = sum(c_across(fear:anger), na.rm = TRUE),
    Positive_Morality = anticipation,
    Negative_Morality = disgust,
    Total_Sentiments = sum(c_across(joy:disgust), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(title) %>%
  summarise(
    Positive_Sociability_Ratio = Positive_Sociability / Total_Sentiments,
    Negative_Sociability_Ratio = Negative_Sociability / Total_Sentiments,
    Positive_Morality_Ratio = Positive_Morality / Total_Sentiments,
    Negative_Morality_Ratio = Negative_Morality / Total_Sentiments
  )

sentiment_features <- hedonometer_counts %>%
  full_join(nrc_counts, by = "title")

df_with_tags <- df_with_tags %>%
  group_by(title) %>%
  dplyr::slice(1) %>%
  ungroup()

df_with_sentiments <- df_with_tags %>%
  left_join(sentiment_features, by = "title")


## --------------------------------------------------------------------------------------------------
df_with_sentiments <- df_with_sentiments %>%
  mutate(across(ends_with("_Ratio"), ~ replace_na(.x, 0)))


## --------------------------------------------------------------------------------------------------
df_with_sentiments <- df_with_sentiments %>%
  mutate(
    Happiness_Valence = Happiness_Positive_Ratio - Happiness_Negative_Ratio,
    Sociability_Valence = Positive_Sociability_Ratio - Negative_Sociability_Ratio,
    Morality_Valence = Positive_Morality_Ratio - Negative_Morality_Ratio
  )


## --------------------------------------------------------------------------------------------------
na_rows <- df_with_sentiments %>%
  filter(is.na(Word_Count))

print(na_rows)


## --------------------------------------------------------------------------------------------------
df_with_sentiments <- df_with_sentiments %>%
  mutate(
    Word_Count = case_when(
      description == "Socially-conscious, recyclable sneakers." ~ 3,
      description == "A high-end porcelain mug with the integrated functionality of a coaster." ~ 6,
      TRUE ~ Word_Count
    )
  )


## --------------------------------------------------------------------------------------------------
drop_cols <- c("description", "category", "title", "website", "description_cleaned", "description_tokens", "Happiness_Positive", "Happiness_Negative", "Happiness_Positive_Ratio", "Happiness_Negative_Ratio", "Positive_Sociability_Ratio", "Negative_Sociability_Ratio", "Positive_Morality_Ratio", "Negative_Morality_Ratio")
df_with_sentiments <- df_with_sentiments %>%
  select(-all_of(drop_cols))


## --------------------------------------------------------------------------------------------------
shark_indicators <- df_with_sentiments %>%
  separate_rows(shark_list, sep = ",") %>%  
  mutate(shark_presence = 1) %>%  
  pivot_wider(names_from = shark_list, values_from = shark_presence, values_fill = 0)  # Convert to wide format

df_with_sharks_indicators <- shark_indicators

df_with_sharks_indicators <- df_with_sharks_indicators %>%
  select(-starts_with("shark"))


## --------------------------------------------------------------------------------------------------
summary(df_with_sharks_indicators[c("Word_Count", "Happiness_Valence", "Sociability_Valence", "Morality_Valence")])


## --------------------------------------------------------------------------------------------------
valence_data <- df_with_sharks_indicators %>%
  select(Happiness_Valence, Sociability_Valence, Morality_Valence) %>%
  pivot_longer(cols = everything(), names_to = "Valence_Type", values_to = "Score")

ggplot(valence_data, aes(x = Valence_Type, y = Score, fill = Valence_Type)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Valence Scores",
    x = "Valence Type",
    y = "Valence Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## --------------------------------------------------------------------------------------------------
ggplot(df_with_sharks_indicators, aes(x = "", y = Word_Count)) +
  geom_boxplot(fill = "blue", alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Box Plot of Word Count",
    x = "",
    y = "Word Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


## --------------------------------------------------------------------------------------------------
valence_vs_deal <- df_with_sharks_indicators %>%
  select(deal, Happiness_Valence, Sociability_Valence, Morality_Valence) %>%
  pivot_longer(cols = ends_with("_Valence"), names_to = "Valence_Type", values_to = "Score")

ggplot(valence_vs_deal, aes(x = deal, y = Score, fill = Valence_Type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  facet_wrap(~ Valence_Type, scales = "free_y") +
  labs(
    title = "Valence Scores by Deal Success",
    x = "Deal Success",
    y = "Valence Score"
  )


## --------------------------------------------------------------------------------------------------
ggplot(df_with_sharks_indicators, aes(x = as.factor(deal), y = Word_Count, fill = as.factor(deal))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Box Plot of Word Count by Deal",
    x = "Deal Success (0 = Unsuccessful, 1 = Successful)",
    y = "Word Count"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "green"), name = "Deal") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------
summary(df_with_sharks_indicators[, c("exchangeForStake", "valuation", "askedFor")])

facet_data <- df_with_sharks_indicators %>%
  select(exchangeForStake, valuation, askedFor) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(facet_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7, outlier.color = "black") +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Boxplot of Exchange for Stake, Valuation, and Asked For",
    x = "Variable",
    y = "Value"
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


## --------------------------------------------------------------------------------------------------
library(ggcorrplot)

cor_data <- df_with_sharks_indicators %>%
  select(exchangeForStake, valuation, askedFor)

cor_matrix <- cor(cor_data, use = "complete.obs")
p_matrix <- cor_pmat(cor_data) 

ggcorrplot(cor_matrix, p.mat = p_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, insig = "blank", title = "Correlation Plot with Significance")


## --------------------------------------------------------------------------------------------------
df_final <- df_with_sharks_indicators %>%
  select(-valuation)


## --------------------------------------------------------------------------------------------------
df_final <- df_final %>%
  mutate(
    season = as.integer(as.character(ifelse(season %in% 1:3, 0, 1)))  # Ensure numeric conversion
  )

df_final <- df_final %>%
  select(-c("episode.season", "episode"))


## --------------------------------------------------------------------------------------------------
df_final <- df_final %>%
  mutate(across(c(location, cluster, dominant_PC), as.factor))
lapply(df_final[c("location", "cluster", "dominant_PC")], levels)


## --------------------------------------------------------------------------------------------------
library(fastDummies)

df_final <- df_final %>%
  mutate(dominant_PC = factor(dominant_PC, levels = c("PC1", setdiff(levels(dominant_PC), "PC1"))))

df_final <- dummy_cols(df_final, 
                       select_columns = c("location", "cluster", "dominant_PC"), 
                       remove_first_dummy = TRUE, 
                       remove_selected_columns = TRUE) 



## --------------------------------------------------------------------------------------------------
colnames(df_final)


## --------------------------------------------------------------------------------------------------
df_final <- df_final %>%
  select(-`Kevin O'Leary`)


## --------------------------------------------------------------------------------------------------
dummy_vars <- names(df_final)[apply(df_final, 2, function(x) all(x %in% c(0, 1)))]

df_standardized <- df_final %>%
  mutate(across(-all_of(dummy_vars), scale))


## --------------------------------------------------------------------------------------------------
df_dominant_PC <- df_standardized %>%
  select(-starts_with("cluster"))  

df_cluster <- df_standardized %>%
  select(-starts_with("dominant_PC"))


## --------------------------------------------------------------------------------------------------
df_dominant_PC <- df_dominant_PC %>%
  mutate(across(where(is.logical), as.numeric))

df_cluster <- df_cluster %>%
  mutate(across(where(is.logical), as.numeric))

str(df_cluster)


## --------------------------------------------------------------------------------------------------
cor_matrix_dominant_PC <- cor(df_dominant_PC, use = "complete.obs")
ggcorrplot(cor_matrix_dominant_PC, 
           lab = TRUE,                       
           lab_size = 1.5,                   
           hc.order = TRUE,                  
           type = "lower",                   
           insig = "blank",                  
           colors = c("#6D9EC1", "white", "#E46726"), 
           title = "Improved Correlation Plot for dominant_PC Data Frame") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8),  
    axis.text.y = element_text(size = 8),                                    
    plot.title = element_text(size = 14, face = "bold")                      
  )


## --------------------------------------------------------------------------------------------------
cor_matrix_cluster <- cor(df_cluster, use = "complete.obs")
ggcorrplot(cor_matrix_cluster, 
           lab = TRUE,                      
           lab_size = 1.5,                   
           hc.order = TRUE,                  
           type = "lower",                   
           insig = "blank",                  
           colors = c("#6D9EC1", "white", "#E46726"), 
           title = "Improved Correlation Plot for cluster data frame") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8),  
    axis.text.y = element_text(size = 8),                                    
    plot.title = element_text(size = 14, face = "bold")                      
  )


## --------------------------------------------------------------------------------------------------
cor_matrix <- cor(df_dominant_PC, use = "complete.obs")
high_corr_pairs <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr_pairs)

df_dominant_PC <- df_dominant_PC %>%
  select(-c("Kevin Harrington", "Nick Woodman", "Steve Tisch", "Jeff Foxworthy", "John Paul DeJoria", "Lori Greiner"))

df_cluster <- df_cluster %>%
  select(-c("Kevin Harrington", "Nick Woodman", "Steve Tisch", "Jeff Foxworthy", "John Paul DeJoria", "Lori Greiner")) 


## --------------------------------------------------------------------------------------------------
library(car)

lm_model <- lm(deal ~ ., data = df_dominant_PC)

vif_values <- vif(lm_model)
print(vif_values)

high_vif <- vif_values[vif_values > 10] 
print("Variables with high VIF ( > 10 ):")
print(high_vif)


## --------------------------------------------------------------------------------------------------
lm_model <- lm(deal ~ ., data = df_cluster)

vif_values <- vif(lm_model)
print(vif_values)

high_vif <- vif_values[vif_values > 10] 
print("Variables with high VIF ( > 10 ):")
print(high_vif)


## --------------------------------------------------------------------------------------------------
library(caret)
colnames(df_dominant_PC) <- make.names(colnames(df_dominant_PC))
colnames(df_cluster) <- make.names(colnames(df_cluster))

set.seed(123)

train_index <- createDataPartition(df_dominant_PC$deal, p = 0.8, list = FALSE)

train_data_PC <- df_dominant_PC[train_index, ]
test_data_PC <- df_dominant_PC[-train_index, ]

set.seed(123)

train_index <- createDataPartition(df_cluster$deal, p = 0.8, list = FALSE)

train_data_cluster <- df_cluster[train_index, ]
test_data_cluster <- df_cluster[-train_index, ]


## --------------------------------------------------------------------------------------------------
library(randomForest)
train_data_PC$deal <- as.factor(train_data_PC$deal)

set.seed(123)
rf_model <- randomForest(deal ~ ., data = train_data_PC, importance = TRUE, ntree = 200)

importance_values <- rf_model$importance  
importance_df_PC <- data.frame(
  Feature = rownames(importance_values),
  Importance = importance_values[, "MeanDecreaseGini"] 
)

if (!"MeanDecreaseGini" %in% colnames(importance_values)) {
  stop("MeanDecreaseGini column not found. Check if the model is in classification mode.")
}

importance_df_PC <- importance_df_PC[order(-importance_df_PC$Importance), ]

important_features_PC <- importance_df_PC %>%
  filter(Importance > 4) %>%
  pull(Feature)

ggplot(importance_df_PC, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance by Gini Index",
    x = "Feature",
    y = "Gini Importance"
  ) +
  theme_minimal()


## --------------------------------------------------------------------------------------------------
train_data_cluster$deal <- as.factor(train_data_cluster$deal)

set.seed(123)
rf_model <- randomForest(deal ~ ., data = train_data_cluster, importance = TRUE, ntree = 200)

importance_values <- rf_model$importance 
importance_df_cluster <- data.frame(Feature = rownames(importance_values), 
                            Importance = importance_values[, "MeanDecreaseGini"])

importance_df_cluster <- importance_df_cluster[order(-importance_df_cluster$Importance), ]

important_features_cluster <- importance_df_cluster %>%
  filter(Importance > 4) %>%
  pull(Feature) 

ggplot(importance_df_cluster, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance by Gini Index",
    x = "Feature",
    y = "Gini Importance"
  ) +
  theme_minimal()


## --------------------------------------------------------------------------------------------------
logistic_model <- glm(deal ~ ., data = train_data_cluster, family = binomial)
summary(logistic_model)

train_data_cluster$predicted_prob_logistic <- predict(logistic_model, type = "response")
train_data_cluster$predicted_class_logistic <- ifelse(train_data_cluster$predicted_prob_logistic > 0.5, 1, 0)

confusion_logistic <- confusionMatrix(
  factor(train_data_cluster$predicted_class_logistic),
  factor(train_data_cluster$deal)
)
print("Logistic Regression Confusion Matrix:")
print(confusion_logistic)


## --------------------------------------------------------------------------------------------------
train_data_cluster_filtered <- train_data_cluster %>%
  dplyr::select(deal, all_of(important_features_cluster))

test_data_cluster_filtered <- test_data_cluster %>%
  dplyr::select(deal, all_of(important_features_cluster))

logistic_model_filtered <- glm(deal ~ ., data = train_data_cluster_filtered, family = binomial)
summary(logistic_model_filtered)

train_data_cluster_filtered$predicted_prob_logistic <- predict(logistic_model_filtered, type = "response")
train_data_cluster_filtered$predicted_class_logistic <- ifelse(train_data_cluster_filtered$predicted_prob_logistic > 0.5, 1, 0)

confusion_logistic_filtered <- confusionMatrix(
  factor(train_data_cluster_filtered$predicted_class_logistic),
  factor(train_data_cluster_filtered$deal)
)
print("Logistic Regression Confusion Matrix (Filtered Features):")
print(confusion_logistic_filtered)


## --------------------------------------------------------------------------------------------------
logistic_model <- glm(deal ~ ., data = train_data_PC, family = binomial)
summary(logistic_model)

train_data_PC$predicted_prob_logistic <- predict(logistic_model, type = "response")
train_data_PC$predicted_class_logistic <- ifelse(train_data_PC$predicted_prob_logistic > 0.5, 1, 0)

confusion_logistic <- confusionMatrix(
  factor(train_data_PC$predicted_class_logistic),
  factor(train_data_PC$deal)
)
print("Logistic Regression Confusion Matrix:")
print(confusion_logistic)


## --------------------------------------------------------------------------------------------------
train_data_PC_filtered <- train_data_PC %>%
  dplyr::select(deal, all_of(important_features_PC))

test_data_PC_filtered <- test_data_PC %>%
  dplyr::select(deal, all_of(important_features_PC))

logistic_model_filtered <- glm(deal ~ ., data = train_data_PC_filtered, family = binomial)
summary(logistic_model_filtered)

train_data_PC_filtered$predicted_prob_logistic <- predict(logistic_model_filtered, type = "response")
train_data_PC_filtered$predicted_class_logistic <- ifelse(train_data_PC_filtered$predicted_prob_logistic > 0.5, 1, 0)

confusion_logistic_filtered <- confusionMatrix(
  factor(train_data_PC_filtered$predicted_class_logistic),
  factor(train_data_PC_filtered$deal)
)
print("Logistic Regression Confusion Matrix (Filtered Features):")
print(confusion_logistic_filtered)


## --------------------------------------------------------------------------------------------------
train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

logistic_model_cv <- train(
  deal ~ . + cluster_2_location_West_Pacific + cluster_3_location_West_Pacific, 
  data = train_data_cluster_filtered_1,
  method = "glmnet",               
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "ROC"                  
)

print(logistic_model_cv)
plot(logistic_model_cv)
cat("Best Parameters for Logistic Regression:\n")
print(logistic_model_cv$bestTune)
cat("Best ROC:", max(logistic_model_cv$results$ROC), "\n")

train_data_cluster_filtered_1$predicted_prob_logistic <- predict(
  logistic_model_cv, 
  train_data_cluster_filtered_1, 
  type = "prob"
)[, "Yes"]  

threshold <- 0.5  
train_data_cluster_filtered_1$predicted_class_logistic <- ifelse(
  train_data_cluster_filtered_1$predicted_prob_logistic > threshold, 
  "Yes", 
  "No"
)

confusion_logistic_cv <- confusionMatrix(
  factor(train_data_cluster_filtered_1$predicted_class_logistic),
  factor(train_data_cluster_filtered_1$deal)
)

print("Logistic Regression Confusion Matrix (Cross-Validated):")
print(confusion_logistic_cv)


## --------------------------------------------------------------------------------------------------
library(rpart)
train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

train_control <- trainControl(
  method = "cv",       
  number = 10,         
  classProbs = TRUE,   
  summaryFunction = twoClassSummary 
)

tune_grid <- expand.grid(
  cp = seq(0.01, 0.1, by = 0.01)
)

set.seed(123)
classification_tree_depth <- train(
  deal ~ .,    
  data = train_data_cluster_filtered_1,
  method = "rpart",  
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "ROC"
)

print(classification_tree_depth)
plot(classification_tree_depth)

cat("Best Parameters for Classification Tree:\n")
print(classification_tree_depth$bestTune)
cat("Best ROC:", max(classification_tree_depth$results$ROC), "\n")

train_data_cluster_filtered_1$predicted_prob_classification_tree <- predict(
  classification_tree_depth,    
  train_data_cluster_filtered_1,    
  type = "prob"
)[, "Yes"]

threshold <- 0.5
train_data_cluster_filtered_1$predicted_class_classification_tree <- ifelse(
  train_data_cluster_filtered_1$predicted_prob_classification_tree > threshold,    
  "Yes",    
  "No"
)

confusion_classification_tree_cv <- confusionMatrix(
  factor(train_data_cluster_filtered_1$predicted_class_classification_tree),
  factor(train_data_cluster_filtered_1$deal)
)
print("Classification Tree Confusion Matrix (Cross-Validated):")
print(confusion_classification_tree_cv)


## --------------------------------------------------------------------------------------------------
library(ranger)
train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

set.seed(123)

train_control <- trainControl(
  method = "cv",       
  number = 10,         
  classProbs = TRUE    
)

tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5),                
  splitrule = c("gini", "extratrees"), 
  min.node.size = c(1, 5, 10)          
)

rf_model_cv <- train(
  deal ~ ., 
  data = train_data_cluster_filtered_1, 
  method = "ranger",       
  trControl = train_control,
  tuneGrid = tune_grid,    
  metric = "Accuracy",     
  importance = "impurity"  
)

print(rf_model_cv)
plot(rf_model_cv)

cat("Best Parameters for Random Forest:\n")
print(rf_model_cv$bestTune)
cat("Best ROC:", max(rf_model_cv$results$ROC), "\n")

train_data_cluster_filtered_1$predicted_prob_rf <- predict(
  rf_model_cv, 
  train_data_cluster_filtered_1, 
  type = "prob"
)[, "Yes"]

threshold <- 0.5
train_data_cluster_filtered_1$predicted_class_rf <- ifelse(
  train_data_cluster_filtered_1$predicted_prob_rf > threshold, 
  "Yes", 
  "No"
)

confusion_rf_cv <- confusionMatrix(
  factor(train_data_cluster_filtered_1$predicted_class_rf),
  factor(train_data_cluster_filtered_1$deal)
)
print("Random Forest Confusion Matrix (Cross-Validated):")
print(confusion_rf_cv)


## --------------------------------------------------------------------------------------------------
library(xgboost)
options(warn = -1) 

train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

xgb_train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data_xgb[, -1]),  
  label = train_data_xgb$deal             
)

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE
)

set.seed(123)
xgb_model <- train(
  deal ~ ., 
  data = train_data_cluster_filtered_1,  
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = expand.grid(
    nrounds = 100,       
    max_depth = 5,      
    eta = 0.1,     
    gamma = 1,             
    colsample_bytree = 0.8,  
    min_child_weight = 10,  
    subsample = 0.7        
  ),
  metric = "Accuracy"
)

print(xgb_model)
plot(xgb_model$results$Accuracy, type = "l", 
     xlab = "Model Iterations", 
     ylab = "Accuracy")

cat("Best Parameters for XGBoost:\n")
print(xgb_model$bestTune)
cat("Best ROC:", max(xgb_model$results$ROC), "\n")

train_data_cluster_filtered_1$predicted_prob_xgb <- predict(
  xgb_model, 
  train_data_cluster_filtered_1, 
  type = "prob"
)[, "Yes"]

threshold <- 0.5
train_data_cluster_filtered_1$predicted_class_xgb <- ifelse(
  train_data_cluster_filtered_1$predicted_prob_xgb > threshold, 
  "Yes", 
  "No"
)

confusion_xgb_cv <- confusionMatrix(
  factor(train_data_cluster_filtered_1$predicted_class_xgb),
  factor(train_data_cluster_filtered_1$deal)
)
print("XGBoost Confusion Matrix (Cross-Validated):")
print(confusion_xgb_cv)



## --------------------------------------------------------------------------------------------------
train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

test_data_cluster_filtered$deal <- factor(
  ifelse(test_data_cluster_filtered$deal == 1, "Yes", "No"), 
  levels = c("No", "Yes")
)

train_data_cluster_filtered_1$deal <- factor(
  train_data_cluster_filtered_1$deal, 
  levels = c("No", "Yes")
)

train_data_cluster_filtered_1$deal_numeric <- ifelse(train_data_cluster_filtered_1$deal == "Yes", 1, 0)
test_data_cluster_filtered$deal_numeric <- ifelse(test_data_cluster_filtered$deal == "Yes", 1, 0)

train_matrix <- xgb.DMatrix(
  data = as.matrix(select(train_data_cluster_filtered_1, -c(deal, deal_numeric))),
  label = train_data_cluster_filtered_1$deal_numeric
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(select(test_data_cluster_filtered, -c(deal, deal_numeric))),
  label = test_data_cluster_filtered$deal_numeric
)

xgb_model <- xgboost(
  data = train_matrix,
  max_depth = 5,
  eta = 0.1,
  gamma = 1,
  colsample_bytree = 0.8,
  min_child_weight = 10,
  subsample = 0.7,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "auc",  
  verbose = 0
)

test_predictions <- predict(xgb_model, test_matrix)

threshold <- 0.5
test_predicted_class <- factor(
  ifelse(test_predictions > threshold, "Yes", "No"), 
  levels = c("No", "Yes")
)

confusion_matrix <- confusionMatrix(
  test_predicted_class,
  test_data_cluster_filtered$deal
)
print(confusion_matrix)

library(pROC)
auc_score <- roc(test_data_cluster_filtered$deal, test_predictions, levels = c("No", "Yes"))$auc
cat("\nAUC for XGBoost on Test Data:", auc_score, "\n")


## --------------------------------------------------------------------------------------------------
train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

rf_model <- ranger(
  deal ~ ., 
  data = train_data_cluster_filtered_1,
  mtry = 2,
  splitrule = "gini",
  min.node.size = 5,
  probability = TRUE,  
  num.trees = 100      
)

rf_predictions <- predict(rf_model, data = test_data_cluster_filtered)

threshold <- 0.5
rf_predicted_class <- factor(
  ifelse(rf_predictions$predictions[, "Yes"] > threshold, "Yes", "No"), 
  levels = c("No", "Yes")
)

confusion_matrix <- confusionMatrix(
  rf_predicted_class,
  test_data_cluster_filtered$deal
)
print(confusion_matrix)

auc_score <- roc(
  test_data_cluster_filtered$deal, 
  rf_predictions$predictions[, "Yes"],
  levels = c("No", "Yes")
)$auc
cat("\nAUC for Random Forest on Test Data:", auc_score, "\n")

feature_importance <- rf_model$variable.importance
print("Feature Importance:")
print(sort(feature_importance, decreasing = TRUE))


## --------------------------------------------------------------------------------------------------
library(glmnet)

train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  )

train_data_cluster_filtered_1$deal_numeric <- ifelse(train_data_cluster_filtered_1$deal == "Yes", 1, 0)
train_matrix <- as.matrix(select(train_data_cluster_filtered_1, -c(deal, deal_numeric)))
train_label <- train_data_cluster_filtered_1$deal_numeric

test_matrix <- as.matrix(select(test_data_cluster_filtered, -c(deal, deal_numeric)))
test_label <- as.numeric(test_data_cluster_filtered$deal_numeric)  # Convert factor to 0/1

ridge_model <- glmnet(
  x = train_matrix,
  y = train_label,
  family = "binomial",  
  alpha = 0,           
  lambda = 0.08        
)

test_predictions <- predict(ridge_model, newx = test_matrix, type = "response")

threshold <- 0.5
test_predicted_class <- factor(
  ifelse(test_predictions > threshold, "Yes", "No"),
  levels = c("No", "Yes")
)

confusion_matrix <- confusionMatrix(
  test_predicted_class,
  test_data_cluster_filtered$deal
)

cat("\nConfusion Matrix for Ridge Logistic Regression on Test Data:\n")
print(confusion_matrix)

# Calculate AUC
library(pROC)
auc_score <- roc(
  test_data_cluster_filtered$deal,
  as.vector(test_predictions),  
  levels = c("No", "Yes")
)$auc

cat("\nAUC for Ridge Logistic Regression on Test Data:", auc_score, "\n")



## --------------------------------------------------------------------------------------------------
train_data_cluster_filtered_1 <- train_data_cluster_filtered %>%
  mutate(
    cluster_2_location_West_Pacific = ifelse(cluster_2 == 1 & location_West_Pacific == 1, 1, 0),
    cluster_3_location_West_Pacific = ifelse(cluster_3 == 1 & location_West_Pacific == 1, 1, 0)
  ) %>%

  mutate(deal = factor(
    deal, 
    levels = c(0, 1), 
    labels = c("No", "Yes")  
  ))

tree_model <- rpart(
  deal ~ ., 
  data = train_data_cluster_filtered_1, 
  method = "class",           
  control = rpart.control(cp = 0.03)  
)

print(summary(tree_model))
plot(tree_model)
text(tree_model, use.n = TRUE)

tree_predictions <- predict(tree_model, test_data_cluster_filtered, type = "prob")

threshold <- 0.5
tree_predicted_class <- factor(
  ifelse(tree_predictions[, "Yes"] > threshold, "Yes", "No"),
  levels = c("No", "Yes")
)

confusion_matrix <- confusionMatrix(
  tree_predicted_class,
  test_data_cluster_filtered$deal
)

cat("\nConfusion Matrix for Classification Tree on Test Data:\n")
print(confusion_matrix)

library(pROC)
auc_score <- roc(
  test_data_cluster_filtered$deal,
  tree_predictions[, "Yes"],  
  levels = c("No", "Yes")
)$auc

cat("\nAUC for Classification Tree on Test Data:", auc_score, "\n")

