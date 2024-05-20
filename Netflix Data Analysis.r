library(ggplot2)

netflix_data <- read.csv("C:\\Users\\vinay\\OneDrive\\Desktop\\Study Material\\5th Sem\\Intro to Data Science\\Assignments\\Final Project\\Netflix.csv")

summary(netflix_data)

ggplot(netflix_data, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Release Years on Netflix",
       x = "Release Year", y = "Frequency")

ggplot(netflix_data, aes(x = type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Counts of Movies and TV Shows",
       x = "Type", y = "Count")

ggplot(netflix_data, aes(x = type, y = runtime, fill = type)) +
  geom_boxplot() +
  labs(title = "Comparison of Runtimes between Movies and TV Shows",
       x = "Type", y = "Runtime")


ggplot(netflix_data, aes(x = imdb_score, y = imdb_votes)) +
  geom_point() +
  labs(title = "IMDb Score vs. IMDb Votes",
       x = "IMDb Score", y = "IMDb Votes")



#T TEST
t.test(netflix_data$imdb_score ~ netflix_data$release_year < 2010)

#ANOVA
aov_result <- aov(netflix_data$imdb_score ~ netflix_data$age_certification)
summary(aov_result)

#Correlation Analysis
correlation <- cor(netflix_data$imdb_score, netflix_data$imdb_votes, use = "complete.obs")
correlation

#Chi-Square Test
chisq.test(table(netflix_data$type, netflix_data$age_certification))

#Regression Analysis
lm_result <- lm(netflix_data$imdb_score ~ netflix_data$runtime)
summary(lm_result)
