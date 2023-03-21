# install.packages("GGally")
library("ggplot2")
library("GGally")
library("tidyverse")

dataset=read.csv('Drug_Clean.csv')

# Exploration of data set
view(dataset)
head(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)
describe(dataset)



# Check for missing values

colSums(is.na(dataset))


# Calculate frequency table for Condition
freq_table_c = table(dataset$Condition)
prop_table_c = prop.table(freq_table_c)
freq_prop_table_c = data.frame(Frequency = freq_table_c, Proportion = prop_table_c)
# Renaming the row names (which are the categories)
rownames(freq_prop_table_c) = names(freq_table_c)

view(freq_prop_table_c)



# Calculate frequency table for Drug
freq_table_d = table(dataset$Drug)
prop_table_d = prop.table(freq_table_d)
freq_prop_table_d = data.frame(Frequency = freq_table_d, Proportion = prop_table_d)
# Renaming the row names (which are the categories)
rownames(freq_prop_table_d) = names(freq_table_d)

view(freq_prop_table_d)




# Calculate frequency table for Form
freq_table_f = table(dataset$Form)
prop_table_f = prop.table(freq_table_f)
freq_prop_table_f = data.frame(Frequency = freq_table_f, Proportion = prop_table_f)
# Renaming the row names (which are the categories)
rownames(freq_prop_table_f) = names(freq_table_f)

view(freq_prop_table_f)




# Calculate frequency table for Indication
freq_table_i = table(dataset$Indication)
prop_table_i = prop.table(freq_table_i)
freq_prop_table_i = data.frame(Frequency = freq_table_i, Proportion = prop_table_i)
# Renaming the row names (which are the categories)
rownames(freq_prop_table_i) = names(freq_table_i)

view(freq_prop_table_i)




# Calculate frequency table for Type
freq_table_t = table(dataset$Type)
prop_table_t = prop.table(freq_table_t)
freq_prop_table_t = data.frame(Frequency = freq_table_t, Proportion = prop_table_t)
# Renaming the row names (which are the categories)
rownames(freq_prop_table_t) = names(freq_table_t)

view(freq_prop_table_t)





# Correlation between EaseOfUse','Effective','Price', 'Reviews', 'Satisfaction' 
cols = c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")
heat_map_corr = cor(dataset[,cols], method = "pearson")

# Creating the heatmap with ggplot2
heat_map = ggplot(data = melt(heat_map_corr), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", limits = c(-1,1)) +
  labs(title = "Heatmap for non-object variables", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heat_map)







# Anything interesting

# Histogram of Ease of Use vs Count with an axis line of mean and median
# Creating the histogram with ggplot2
hist_plot = ggplot(data = dataset, aes(x = EaseOfUse)) +
  geom_histogram(fill = "blue", color = "white", bins = 10) +
  labs(title = "Ease of Use vs Count", x = "Ease of Use", y = "Count") +
  theme_minimal()

# Calculating the mean and median
mean_val = mean(dataset$EaseOfUse)
median_val = median(dataset$EaseOfUse)

# Adding vertical lines for mean and median to the histogram
hist_plot = hist_plot + geom_vline(xintercept = mean_val, color = "red") +
  geom_vline(xintercept = median_val, color = "green")

print(hist_plot)






# Histogram of Effective vs Count with an axis line of mean and median
# Creating the histogram with ggplot2
hist_plot2 = ggplot(data = dataset, aes(x = Effective)) +
  geom_histogram(fill = "blue", color = "white", bins = 10) +
  labs(title = "Effective vs Count", x = "Effective", y = "Count") +
  theme_minimal()

# Calculating the mean and median
mean_val2 = mean(dataset$Effective)
median_val2 = median(dataset$Effective)

# Adding vertical lines for mean and median to the histogram
hist_plot2 = hist_plot2 + geom_vline(xintercept = mean_val2, color = "red") +
  geom_vline(xintercept = median_val2, color = "green")

print(hist_plot2)






# Scatter plot of Effective vs Satisfaction
scatter_plot = ggplot(data = dataset, aes(x = Effective, y = Satisfaction, color = "purple")) +
  geom_point() +
  labs(title = "Effectiveness vs Satisfaction", x = "Effective", y = "Satisfaction") +
  theme_minimal()

print(scatter_plot)







# Pair plot of Effective, Ease of Use, Satisfaction, and Price
cols = c("Effective", "EaseOfUse", "Satisfaction", "Price")
pair_plot = ggpairs(data = dataset[,cols], 
                    lower = list(continuous = wrap("points", alpha = 0.5, size = 0.5)),
                    upper = list(continuous = wrap("cor", size = 2)))

print(pair_plot)




# Bar plot of Ease of Use vs Satisfaction
bar_plot = ggplot(data = dataset, aes(x = EaseOfUse, y = Satisfaction)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  labs(title = "Ease of Use vs Satisfaction", x = "Ease of Use", y = "Satisfaction") +
  theme_minimal()

print(bar_plot)






# Subplots of Effectiveness vs Price
bar_plot = ggplot(data = dataset, aes(x = Effective, y = Price)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5, width = 0.5) +
  labs(title = "Effectiveness vs Price", x = "Effective", y = "Price") +
  theme_minimal()

print(bar_plot)
