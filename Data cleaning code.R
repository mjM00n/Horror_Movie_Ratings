library(tidyverse)
library(ggplot2)

data <- read.csv("/Users/Michael/Dropbox/My Mac (Bobby’s MacBook Pro)/Documents/Data_602/Project/ZoomGloom/Horror Movies IMDb.csv", stringsAsFactors = FALSE)
data <- distinct(data)

# replacing missing values
data$Gross[data$Gross == ""] <- NA

# 'Votes' & 'Gross' to numeric
data$Votes <- as.numeric(gsub(",", "", data$Votes))
data$Gross <- as.numeric(gsub("[$]", "", gsub("M", "", data$Gross))) * 1e6

str(data)
summary(data[, c("Runtime", "Rating", "Votes", "Gross")])

# plot for distribution of Ratings
ggplot(data, aes(x = Rating)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Frequency") +
  theme_minimal()

# boxplot for gross earnings
ggplot(data, aes(y = Gross)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Gross Earnings", y = "Gross Earnings") +
  theme_minimal()

# bar Plot for movies by year
ggplot(data, aes(x = factor(Movie.Year))) +
  geom_bar(fill = "purple") +
  labs(title = "Number of Movies Released by Year", x = "Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot genre distribution
genre_counts <- data %>% 
  separate_rows(Genre, sep = ", ") %>% 
  count(Genre) %>% 
  arrange(desc(n))

ggplot(genre_counts, aes(x = reorder(Genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Distribution of Genres", x = "Genre", y = "Count") +
  coord_flip() +
  theme_minimal()

# scatter plot of votes to ratings
ggplot(data, aes(x = Votes, y = Rating)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Votes vs. Ratings", x = "Votes", y = "Rating") +
  scale_x_log10() +  # Use log scale for better visibility
  theme_minimal()

# scatter plot of rating vs gross
ggplot(data, aes(x = Rating, y = Gross)) +
  geom_point(alpha = 0.6) +
  labs(title = "Rating vs. Gross Earnings", x = "Rating", y = "Gross Earnings") +
  theme_minimal()
