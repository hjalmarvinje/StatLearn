library(carData)
?Salaries
GGally::ggpairs(Salaries)

model1 <- lm(salary ~ ., data = Salaries)
summary(model1)

# 2a ii.
model2 <- aov(salary ~ rank, data = Salaries)
summary(model2)

#2b
sex_model <- lm(salary ~ sex, data = Salaries)
summary(sex_model)



# Plot 1 - Salary by rank
library(ggplot2)
# calculate the average salary for each rank
avg_salary_by_rank <- aggregate(Salaries$salary, by = list(Salaries$rank), mean)
names(avg_salary_by_rank) <- c("rank", "avg_salary")

# add the geom_text layer to display the calculated average salaries
ggplot(Salaries, aes(x = rank, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(data = avg_salary_by_rank, aes(yintercept = avg_salary), color = "red", linetype = "dotted") +
  geom_text(data = avg_salary_by_rank, aes(x = rank, y = avg_salary, label = paste("Avg:", round(avg_salary, 0))), hjust = -0.05, vjust = -0.2) +
  labs(x = "Years of Service", y = "Salary")

# Plot 2 - Sex by rank
library(dplyr)
# Calculate the proportions of each rank made up by women
Salaries_proportions <- Salaries %>%
  group_by(rank, sex) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

# Create a bar chart of the number of males and females for each rank
ggplot(Salaries_proportions, aes(x=rank, y=proportion, fill=sex)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Rank", y="Proportion", fill="Sex") + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = scales::percent(proportion), y = proportion), 
            position = position_dodge(0.9), vjust = -0.5) +
  theme_classic()
