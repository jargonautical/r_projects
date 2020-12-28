
# CLUSTERING NON-COMPLIANT FOOD BUSINESSES

library(tidyverse)
library(cluster)
library(Rtsne)
library(janitor)

# read in the data

data <- readRDS("./outputs/alldata.rds")

# filter only good matches and only non-compliant establishments
# select relevant variables

clusterdata <- data %>%
  filter(grepl("^A", `Match Code`)) %>% 
  filter(risky == "Risky") %>% 
  select(BusinessType, sic07, employeesite2, algsalesband2,
         maxcred2, riskband, riskcat, delband, decile, landusecol) %>% 
  drop_na() %>% 
  mutate(sic07 = as.factor(sic07))

# calculate the gower distance between the observations

gower_dist <- daisy(clusterdata[,5:12])

# express as a matrix

gower_mat <- as.matrix(gower_dist)

# inspect the closest observations

clusterdata[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# inspect the furthest apart observations

clusterdata[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate the silhouette width at different numbers of clusters

sil_width <- c(NA)

for(i in 3:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot silhouette width (higher is better)

plot(sil_width)

# use PAM algorithm to cluster observations

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- clusterdata %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

# results summary

pam_results$the_summary

# most typical example of each cluster

clusterdata[pam_fit$medoids, ]

# bind clusters to original dataset

addclusters <- bind_cols(clusterdata, cluster = pam_fit$clustering)

# example: use this to look at the top business activities in a particular cluster

addclusters %>% 
  filter(cluster == 3) %>% 
  tabyl(sic07) %>% as_tibble() %>% arrange(desc(percent))

# create a t-SNE object and plot it

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

