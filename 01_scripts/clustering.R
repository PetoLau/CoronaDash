# Hierarchical clustering with exact n. of clusters ----
hie_clus <- function(data, k, cols, crit) {
  
  # data normalization (must!)
  data_res_norm <- scale(data.matrix(data[, .SD,
                                          .SDcols = cols
                                          ]),
                         center = T, scale = T)
  
  # Euclidean dist. and Ward.D2 criterion (defaults) in hier. clust.
  hie_res <- hclust(dist(data_res_norm),
                    method = crit)

  # Cut the tree to k clusters
  data_clust <- dendextend::cutree(hie_res,
                                   k = k)

  # Set colors for clusters
  data_clust_colors <- data.table(Cluster = 1:k,
                                  # Color = RColorBrewer::brewer.pal(k, name = "Set2")
                                  Color = colorspace::rainbow_hcl(k, c = 90, l = 50)
                                  )

  # join
  data_clust_merge <- data.table(Cluster = data_clust)
  data_clust_merge[data_clust_colors,
                   on = .(Cluster),
                   Color := i.Color]

  return(list(
    clust_obj = hie_res,
    clustering = data_clust_merge,
    clusters_colors = data_clust_colors
        ))
  
}
