## Import library
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(corrplot)
library(ClustImpute)
library(DescTools)
library(ggalt)

### Import data and clean the missing values
data <-read_dta("mrc_table8.dta")
summary(data)
data <- data[-4]
PlotMiss(data,main="Missing Data in College dataset")
data <- na.omit(data)
PlotMiss(data,main="Missing Data in College dataset after processing")

### Correlation Analysis
cor_matrix <- round(cor(data),3)
corrplot.mixed(cor_matrix)

# Identifying correlation matrix outliers
#######################################################
#Convert the correlation matrix into a data frame and include the variable names:
cor_df <- cor_matrix %>%
  as.data.frame() %>%
  mutate(Variable1 = row.names(.))

cor_df_long <- cor_df %>%
  gather(key = "Variable2", value = "Correlation", -Variable1)

#Define your criteria for correlation outliers. For example, you can consider correlations with an absolute value greater than 0.75 as outliers:
threshold <- 0.75

outliers <- cor_df_long %>%
  filter(abs(Correlation) > threshold & Variable1 != Variable2)

#Organize the outliers by interest. You can sort them in descending order based on the absolute value of the correlation:
outliers_sorted <- outliers %>%
  arrange(desc(abs(Correlation)))

print(outliers_sorted)


# mice(data)
#dim(data)
#college.tb <- as_tibble(college)
#pacollege <- college.tb %>% filter(state=="PA")
#upenn<- college.tb %>% filter(super_opeid==3378)
#pacollege_comp <- na.omit(pacollege)
#attach(pacollege_comp)
#pacollege_imp <- ClustImpute(pacollege[15:84],nr_cluster = 3)
#pacollege_comp <- pacollege_imp$complete_data
#sum(is.na(pacollege_comp))

#Visualize missing data

#Missing data
#install.packages("mice")
#library(mice)
#imputed_data <- mice(data)
#PlotMiss(imputed_data,main="Missing Data in College dataset")


### Important insights: Mean Kid Income with other factors
library(ggplot2)
library(RColorBrewer)
hist_color1 <- colorRampPalette(c("black","blue","cyan","cadetblue"))(12)
ggplot(data, aes(y=k_mean, fill=factor(cohort))) + geom_histogram() + facet_grid(.~cohort) +
 scale_x_continuous(breaks = NULL)+ labs(title = "Mean Kid Income based on Cohort",
  y="Mean Kid Income", x="Birth Cohort") + theme_classic()+ 
  scale_fill_manual(values=hist_color1) 

hist_color2 <- colorRampPalette(c("black","coral","burlywood","bisque","gray"))(14)
ggplot(data, aes(y=k_mean, fill=factor(tier))) + geom_histogram() + facet_grid(.~tier) +
  scale_x_continuous(breaks = NULL)+ labs(title = "Mean Kid Income based on College Tier",
 y="Mean Kid Income", x="College Tier") + theme_classic() + scale_fill_manual(values=hist_color2)


hist_color3 <- colorRampPalette(c("black","darkgreen","gold","salmon","sienna",
                                         "gray","khaki4"))(21)
ggplot(data, aes(y=k_mean,fill=factor(par_ventile))) + geom_histogram() + facet_grid(.~par_ventile) +
  scale_x_continuous(breaks = NULL)+ labs(title = "Mean Kid Income based on Parent Income",
                      y="Mean Kid Income", x="Parent Income Quintile") + 
  theme_classic()+scale_fill_manual(values = hist_color3)

## Further Insights
data <-read_dta("mrc_table8.dta")
data <- na.omit(data)

num_tiers <- length(unique(data$tier_name))

custom_palette <- colorRampPalette(brewer.pal(9, "Paired"))(num_tiers)
custom_shapes <- c(seq(15, 24), seq(15, 20)) # Repeat some shapes to reach 14

ggplot(data, aes(x = par_mean, y = k_mean, color = tier_name, shape = tier_name)) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_palette) +
  scale_shape_manual(values = custom_shapes) +
  labs(title = "Relationship between Parent Income and Child Income by Tier",
       x = "Mean Parent Income",
       y = "Mean Child Income",
       color = "Tier",
       shape = "Tier") +
  theme_minimal()

#Visualize the distribution of child income across different college tiers and quintiles:
#######################################################
data %>%
  gather(key = "income_group", value = "fraction", k_q1:k_q5) %>%
  ggplot(aes(x = income_group, y = fraction, fill = tier_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = hist_color2) +
  labs(title = "Child Income Distribution Across College Tiers and Quintiles",
       x = "Income Quintile",
       y = "Fraction",
       fill = "College Tier") +
  theme_minimal()


#######################################################
#Visualize the fraction of kids in the top 1%, 5%, and 10% by college tier:
#######################################################
data %>%
  gather(key = "top_pct_group", value = "fraction", k_top1pc:k_top10pc) %>%
  ggplot(aes(x = top_pct_group, y = fraction, fill = tier_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = hist_color2) +
  labs(title = "Fraction of Kids in Top Income Percentiles by College Tier",
       x = "Top Income Percentile Group",
       y = "Fraction",
       fill = "College Tier") +
  theme_minimal()

#######################################################
# K-means cluster graph for relationship between parent income and child income:
#######################################################
#Select the data for clustering:
clustering_data <- data %>%
  select(par_mean, k_mean)

#Remove rows with missing or infinite values:
clustering_data <- na.omit(data %>%
                             select(par_mean, k_mean))


#Run the k-means clustering algorithm. You can choose the number of clusters based on your domain knowledge or by using techniques like the elbow method:
set.seed(123)
num_clusters <- 3
kmeans_result <- kmeans(clustering_data, centers = num_clusters)

#Add the cluster assignments to the original data:
data_filtered <- data %>%
  filter(!is.na(par_mean) & !is.na(k_mean))

data_with_clusters <- data_filtered %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

#Visualize the relationship between parent income and child income with clusters:
ggplot(data_with_clusters, aes(x = par_mean, y = k_mean, color = Cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Relationship between Parent Income and Child Income with Clusters",
       x = "Mean Parent Income",
       y = "Mean Child Income",
       color = "Cluster") +
  theme_minimal()
#######################################################
#improving previous cluster visualization
#######################################################
#Visualize the relationship between parent income and child income with clusters, using different point shapes for each cluster and adding cluster outlines:
ggplot(data_with_clusters, aes(x = par_mean, y = k_mean, color = Cluster, shape = Cluster)) +
  geom_point(size = 2) +
  geom_encircle(aes(fill = Cluster), s_shape = 1.0, expand = 0.05, linetype = "dashed", alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(15, 16, 17)) +
  labs(title = "Relationship between Parent Income and Child Income with Clusters",
       x = "Mean Parent Income",
       y = "Mean Child Income",
       color = "Cluster",
       shape = "Cluster") +
  theme_minimal() +
  guides(fill = "none") # To remove the legend for cluster fill


# If number of cluster = 9
num_clusters <- 9
kmeans_result <- kmeans(clustering_data, centers = num_clusters)
data_filtered <- data %>%
  filter(!is.na(par_mean) & !is.na(k_mean))
data_with_clusters <- data_filtered %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))
ggplot(data_with_clusters, aes(x = par_mean, y = k_mean, color = Cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Relationship between Parent Income and Child Income with Clusters",
       x = "Mean Parent Income",
       y = "Mean Child Income",
       color = "Cluster") +
  theme_minimal()
#######################################################
#improving previous cluster visualizatio (((for 9 clusters)))
#######################################################
#Define a custom color palette for the 9 clusters:
custom_palette <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
                             "#911eb4", "#46f0f0", "#f032e6", "#bcf60c")
                             
#Visualize the relationship between parent income and child income with clusters, using different point shapes for each cluster, adding cluster outlines, and using the custom color palette:
ggplot(data_with_clusters, aes(x = par_mean, y = k_mean, color = Cluster, shape = Cluster)) +
  geom_point(size = 2) +
  geom_encircle(aes(fill = Cluster), s_shape = 1.0, expand = 0.05, linetype = "dashed", alpha = 0.3) +
  scale_color_manual(values = custom_palette) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  labs(title = "Relationship between Parent Income and Child Income with Clusters",
       x = "Mean Parent Income",
       y = "Mean Child Income",
       color = "Cluster",
       shape = "Cluster") +
  theme_minimal() +
  guides(fill = "none") # To remove the legend for cluster fill


