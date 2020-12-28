# Tutorial at https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/

# install packages
install.packages("factoextra") # for elegant ggplot dataviz
install.packages("cluster") # for computing clustering
install.packages("magrittr") # for piping %>%

#load packages
library("cluster")
library("factoextra")
library("magrittr")

# load a prepare the data
data("USArrests")
my_data <- USArrests %>%
  na.omit() %>% # remove missing values
  scale() # scale variables

# view the first three rows
head(my_data, n = 3)

# compute a distance matrix
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")

# visualise a distance matrix fviz_dist
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# determine optimum number of clusters
fviz_nbclust(my_data, kmeans, method = "gap_stat")

# compute and visualise k-means clustering
set.seed(123)
km.res <- kmeans(my_data, 4, nstart = 25)
fviz_cluster(km.res, data = my_data, 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal())

# k-medoids and pam clustering
# compute PAM
pam.res <- pam(my_data, 3)
# visualise
fviz_cluster(pam.res)

# compute hierarchical clustering
res.hc <- USArrests %>%
  scale() %>% # scale the data
  dist(method = "euclidean") %>% # compoute dissimilarity matrix
  hclust(method = "ward.D2") # compute hierarchical clustering

# visualise using factoextra
# cut in 4 groups and colour by groups
fviz_dend(res.hc, k = 4, # cut in 4 groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # colour labels by groups
          rect = TRUE # add rectangle around groups
          )

# assess clustering tendency
gradient.color <- list(low = "steelblue",  high = "white")
iris[, -5] %>%    # Remove column 5 (Species)
  scale() %>%     # Scale variables
  get_clust_tendency(n = 50, gradient = gradient.color)

# determining the optimal number of clusters
install.packages("NbClust")
# compute
library("NbClust")
res.nbclust <- USArrests %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10,
          method = "complete", index = "all")

# VISUALISE
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

set.seed(123)

# enhanced hierarchical clustering cut in 3 groups
res.hc <- iris[, -5] %>%
  scale() %>%
  eclust("hclust",k = 3, graph = FALSE)

# Visualise with factoextra
fviz_dend(res.hc, palette = "jco", rect = TRUE, show_labels = FALSE)

fviz_silhouette(res.hc)

# silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]

# objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] <0)
sil[neg_sil_index, , drop = FALSE]



