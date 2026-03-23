install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
library(readxl)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
setwd("~/Desktop")
df <- read_excel("360buy_SurveyData (1).xlsx")
cluster_vars <- df[, c("CusAgeYr","CusGen","LevEdn","LevIncome",
                       "CusChoice","ConstUp","ReplacReminder",
                       "ProInsuCov","ProdReturn")]

df_scaled <- scale(cluster_vars)
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2, color = "red") +
  labs(title = "Figure 1: Elbow Plot - Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares")
ggsave("fig1_elbow_plot.png", width = 8, height = 5)
dist_matrix <- dist(df_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

fviz_dend(hc, k = 3,
          cex = 0.5,
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Figure 2: Dendrogram - Ward Linkage, Euclidean Distance")
set.seed(42)
sample_idx <- sample(1:nrow(df_scaled), 200)
df_sample <- df_scaled[sample_idx, ]

dist_matrix <- dist(df_sample, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

fviz_dend(hc, k = 3,
          cex = 0.5,
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Figure 2: Dendrogram (n=200 sample, Ward Linkage)")
set.seed(42)
km <- kmeans(df_scaled, centers = 3, nstart = 25)
df$Cluster <- as.factor(km$cluster)
cluster_means <- df %>%
  group_by(Cluster) %>%
  summarise(
    Age = mean(CusAgeYr),
    Gender = mean(CusGen),
    Education = mean(LevEdn),
    Income = mean(LevIncome),
    PlatformChoice = mean(CusChoice),
    ConstantUpdate = mean(ConstUp),
    ReplacReminder = mean(ReplacReminder),
    ProductInsurance = mean(ProInsuCov),
    ProductReturn = mean(ProdReturn),
    HasAccount = mean(CusAcct)
  )

print(cluster_means)
print(cluster_means, width = Inf)
library(tidyr)

means_long <- cluster_means %>%
  select(Cluster, PlatformChoice, ConstantUpdate, 
         ReplacReminder, ProductInsurance, ProductReturn) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Mean")

ggplot(means_long, aes(x = Variable, y = Mean, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Figure 3: Mean Cluster Characteristics",
       x = "Variable", y = "Mean Score (1-7)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cluster_sizes <- as.data.frame(table(df$Cluster))
colnames(cluster_sizes) <- c("Cluster", "Count")
cluster_sizes$Percentage <- round(cluster_sizes$Count / sum(cluster_sizes$Count) * 100, 1)
cluster_sizes$Label <- paste0("Cluster ", cluster_sizes$Cluster, "\n(", cluster_sizes$Percentage, "%)")

ggplot(cluster_sizes, aes(x = "", y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
  labs(title = "Figure 4: Relative Size of Market Segments") +
  theme_void()
ggplot(df, aes(x = factor(LevIncome), fill = Cluster)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c("1" = "<£50K", "2" = "£51-100K", 
                              "3" = "£101-125K", "4" = "£126-150K", 
                              "5" = ">£150K")) +
  labs(title = "Figure 5: Income Distribution by Cluster",
       x = "Annual Household Income",
       y = "Number of Respondents") +
  theme_minimal()