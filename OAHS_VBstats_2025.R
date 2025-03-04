# Install and load necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("zoo")

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)


# Read the data from Excel
volleyball_data <- read_excel("/Users/jamiclayton/Documents/Volleyball/OAHS_MensVB/2025_Mens/Stats/2025_Stats.xlsx")

# Convert datetime to date
volleyball_data <- volleyball_data %>%
  mutate(Date = as.Date(Date))

# View the first few rows to check
head(volleyball_data)




### CATEGORY-SPECIFIC DATASETS ###

# Create dataset for serve stats
serve_data <- volleyball_data %>%
  select(Date, Opponent, Game, Player, Serve_Attempts, Aces, Serve_Errors)

# Create dataset for attack stats
attack_data <- volleyball_data %>%
  select(Date, Opponent, Game, Player, Att_Attempts, Kills, Att_Errors)

# Create dataset for block stats
block_data <- volleyball_data %>%
  select(Date, Opponent, Game, Player, Solo_Block, Block_Assist, Block_Error)

# Create dataset for reception stats
reception_data <- volleyball_data %>%
  select(Date, Opponent, Game, Player, Rec_Attempts, Rec_Errors, "3_Rec", "2_Rec", "1_Rec")

# Create dataset for dig stats
dig_data <- volleyball_data %>%
  select(Date, Opponent, Game, Player, Dig, Dig_Error)

# Create dataset for ball-handling stats
ballhandling_data <- volleyball_data %>%
  select(Date, Opponent, Game, Player, BH_Attempts, Assists, BH_Errors)



### FILTER BY GAME ###

# Define game dates
game_dates <- c("2025-02-18", "2025-02-20", "2025-02-25", "2025-02-27")


### PLAYER COLORS ###

# Adjusted color palette for higher contrast (optional, replace with your own colors)
player_colors <- c("Miles" = "#56BCC2", "Dustin" = "#CD9332", "Bido" = "#E77D72",
                   "Stew" = "#D772EC", "Zach" = "#ED6DB9", "Michael" = "#56BC82", 
                   "Seamus" = "#9490F8", "Sam" = "#4FAEF0", "Kitts" = "#60B334", 
                   "Grant" = "#A3A533")






### SERVE DATA ###

# Calculate Serve and Ace Percentages for the entire dataset
serve_data <- serve_data %>%
  mutate(
    Serve_Percentage = ifelse(Serve_Attempts > 0, (Serve_Attempts - Serve_Errors) / Serve_Attempts, 0),
    Ace_Percentage = ifelse(Serve_Attempts > 0, Aces / Serve_Attempts, 0)
  )

# Reshape data for ggplot (long format for both Serve and Ace Percentages)
serve_data_long <- serve_data %>%
  pivot_longer(cols = c(Serve_Percentage, Ace_Percentage), 
               names_to = "Metric", 
               values_to = "Percentage") %>%
  mutate(Metric = factor(Metric, levels = c("Serve_Percentage", "Ace_Percentage")))

# Define goals
serving_goal <- 0.90
ace_goal_min <- 0.10  # Lower bound of Ace Percentage goal
ace_goal_max <- 0.20  # Upper bound of Ace Percentage goal




### PLOT SERVE DATA (Single Game) ###

# Select a game date (change the index to select different games)
selected_game_date <- game_dates[2]  # Change this to select another game

# Filter data for the selected game
game_data <- serve_data %>% filter(Date == selected_game_date)

# Reshape data for ggplot
game_data_long <- game_data %>%
  pivot_longer(cols = c(Serve_Percentage, Ace_Percentage), 
               names_to = "Metric", 
               values_to = "Percentage") %>%
  mutate(Metric = factor(Metric, levels = c("Serve_Percentage", "Ace_Percentage")))

# Create the plot
ggplot(game_data_long, aes(x = Player, y = Percentage, fill = Player, alpha = Metric)) +
  geom_bar(stat = "identity", position = "identity") +
  
  # Transparency adjustment for Serve and Ace Percentages
  scale_alpha_manual(values = c("Serve_Percentage" = 0.5, "Ace_Percentage" = 1),
                     labels = c("Serve Percentage (Transparent)", "Ace Percentage (Solid)"),
                     name = "Metric") +
  
  # Player colors
  scale_fill_manual(values = player_colors, name = "Player") +
  
  # Add goal lines and shaded regions
  geom_hline(yintercept = serving_goal, linetype = "dashed", color = "red", linewidth = 0.6) +
  annotate("text", x = 1, y = serving_goal + 0.04, label = "Serve % Goal", color = "red", size = 4, hjust = 0) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ace_goal_min, ymax = ace_goal_max, 
           fill = "blue", alpha = 0.2) +
  annotate("text", x = 1, y = (ace_goal_min + ace_goal_max) / 2, 
           label = "Ace % Goal Range", color = "blue", size = 4, hjust = 0) +
  
  # Add labels and caption
  labs(title = paste("Player Serving and Ace Percentages for Game on", selected_game_date),
       y = "Percentage", x = "Player",
       caption = "Definitions:\nServing Percentage = (Serve Attempts - Errors) / Serve Attempts\nAce Percentage = Aces / Serve Attempts") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 13),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.position = "right")




### PLOT AVERAGED SERVE DATA ###

# Calculate average serve and ace percentages per player
average_serve_data <- serve_data %>%
  group_by(Player) %>%
  summarise(
    Avg_Serve_Percentage = mean(Serve_Percentage, na.rm = TRUE),
    Avg_Ace_Percentage = mean(Ace_Percentage, na.rm = TRUE)
  )

# Reshape the data for ggplot
average_serve_data_long <- average_serve_data %>%
  pivot_longer(cols = c(Avg_Serve_Percentage, Avg_Ace_Percentage), 
               names_to = "Metric", 
               values_to = "Percentage") %>%
  mutate(Metric = factor(Metric, levels = c("Avg_Serve_Percentage", "Avg_Ace_Percentage")))

# Create the average serve plot
ggplot(average_serve_data_long, aes(x = Player, y = Percentage, fill = Player, alpha = Metric)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_alpha_manual(values = c("Avg_Serve_Percentage" = 0.5, "Avg_Ace_Percentage" = 1)) +
  scale_fill_manual(values = player_colors, name = "Player") +
  annotate("text", x = 1, y = serving_goal + 0.04, label = "Serve % Goal", color = "red", size = 4, hjust = 0) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = ace_goal_min, ymax = ace_goal_max, 
           fill = "blue", alpha = 0.2) +
  annotate("text", x = 1, y = (ace_goal_min + ace_goal_max) / 2, 
           label = "Ace % Goal Range", color = "blue", size = 4, hjust = 0) +
  labs(title = "Average Player Serving and Ace Percentages",
       y = "Percentage", x = "Player",
       caption = "Definitions:\nServing Percentage = (Serve Attempts - Errors) / Serve Attempts\nAce Percentage = Aces / Serve Attempts") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Position the plot title
  geom_hline(yintercept = serving_goal, linetype = "dashed", color = "red", size = .6) +
  scale_y_continuous(limits = c(0, 1))  # Set the y-axis range from 0 to 1




### PLOT SERVE TRENDLINES ###

# Filter dataset for plotting Serve and Ace Percentages over time
serve_ace_time_data_filtered <- serve_data_long_filtered %>%
  select(Date, Player, Metric, Percentage)  

library(zoo)


# Calculate the rolling average (last 3 games)
serve_ace_time_data_filtered <- serve_ace_time_data_filtered %>%
  group_by(Player, Metric) %>%
  arrange(Date) %>%  # Ensure the data is sorted by date
  mutate(Rolling_Avg = rollmean(Percentage, 3, fill = NA, align = "right"))

# Calculate the difference between the last game and the rolling average
serve_ace_time_data_filtered <- serve_ace_time_data_filtered %>%
  group_by(Player, Metric) %>%
  mutate(Last_Game_Percentage = ifelse(row_number() == n(), Percentage, NA),
         Rolling_Avg_Diff = Last_Game_Percentage - lag(Rolling_Avg, order_by = Date)) %>%  # Use lag() for last game vs rolling average
  fill(Rolling_Avg_Diff, .direction = "down")

# Get the legend labels (difference between rolling average and last game)
legend_labels <- serve_ace_time_data_filtered %>%
  filter(row_number() == n()) %>%  # Keep only the most recent game for each player
  mutate(legend_label = paste(Player, "Change: (", 
                              ifelse(Rolling_Avg_Diff >= 0, 
                                     paste("+", round(Rolling_Avg_Diff, 2), sep = ""), 
                                     round(Rolling_Avg_Diff, 2)), 
                              ")", sep = ""))

# Combine legend labels with the player colors for the custom color scale
player_legend_labels <- setNames(legend_labels$legend_label, legend_labels$Player)

# Create the plot
ggplot(serve_ace_time_data_filtered, aes(x = Date, y = Percentage, color = Player, group = interaction(Player, Metric))) +
  
  # Trendlines for Serve % (solid) and Ace % (dashed)
  geom_line(aes(linetype = Metric), size = 1) +  
  scale_linetype_manual(values = c("Serve_Percentage" = "solid", "Ace_Percentage" = "dashed")) +  
  scale_color_manual(values = player_colors, labels = player_legend_labels) +  # Use custom player colors and labels
  
  # Serve percentage goal line  
  geom_hline(yintercept = serving_goal, linetype = "dashed", color = "red", linewidth = 0.6) +
  annotate("text", x = min(serve_ace_time_data_filtered$Date), y = serving_goal + 0.03, 
           label = "Serve % Goal", color = "red", size = 3.5, hjust = 0) +
  
  # Ace percentage goal shaded region  
  annotate("rect", xmin = min(serve_ace_time_data_filtered$Date), xmax = max(serve_ace_time_data_filtered$Date), 
           ymin = ace_goal_min, ymax = ace_goal_max, fill = "blue", alpha = 0.2) +
  annotate("text", x = min(serve_ace_time_data_filtered$Date), y = (ace_goal_min + ace_goal_max) / 2, 
           label = "Ace % Goal Range", color = "blue", size = 3.5, hjust = 0) +
  
  # Labels and theme
  labs(title = "Serve and Ace Percentages Over Time",
       x = "Date", y = "Percentage",
       caption = "Definitions:\nServe Percentage = (Serve Attempts - Errors) / Serve Attempts\nAce Percentage = Aces / Serve Attempts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5)) # Center title
