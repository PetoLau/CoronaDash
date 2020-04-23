# Clustering trajectories TS with different lengths using DTW dist. + hierarchical clustering + DTW barycenters ----- 
cluster_trajectories <- function(data, k, normalize = FALSE) {
  
  # transpose data for clustering
  data_trajectories_trans <- t(data[, .SD,
                                    .SDcols = colnames(data)[-1]])
  
  data_trajectories_trans_list <- lapply(1:nrow(data_trajectories_trans), function(i) na.omit(data_trajectories_trans[i,]))
  names(data_trajectories_trans_list) <- colnames(data)[-1]
  
  # TODO - use some time series representations - SMA
  # data_trajectories_trans_list_repr <- lapply(names(data_trajectories_trans_list),
  #                                             function(i) repr_sma(data_trajectories_trans_list[[i]],
  #                                                                  order = order))
  # names(data_trajectories_trans_list_repr) <- names(data_trajectories_trans_list)
  
  n_list <- sapply(1:length(data_trajectories_trans_list), function(i) length(data_trajectories_trans_list[[i]]))
  names(n_list) <- names(data_trajectories_trans_list)
  
  # save(n_list, file = "n_list.RData")
  # load("n_list.RData")
  
  if (length(which(n_list %in% 0:1)) != 0) {
    
    data_trajectories_trans_list <- data_trajectories_trans_list[-which(n_list %in% 0:1)]
    
  }
  
  list_names <- names(data_trajectories_trans_list)

  # normalization
  if (normalize) {
    
    data_trajectories_trans_list <- lapply(names(data_trajectories_trans_list),
                                                function(i) norm_z(data_trajectories_trans_list[[i]])
                                                 )
    names(data_trajectories_trans_list) <- list_names
    
  }

  # save(data_trajectories_trans_list, file = "data_list.RData")
  # load("data_list.RData")
  
  # na.omit(data_trajectories_trans_list[["Latvia"]])
  
  hc_res <- tsclust(data_trajectories_trans_list,
                    type = "hierarchical",
                    k = k,
                    distance = "dtw_basic", # "dtw", "dtw_basic", "dtw2", "sdtw"
                    centroid = dba, # dba, sdtw_cent
                    trace = FALSE,
                    control = hierarchical_control(method = "ward.D2"),
                    args = tsclust_args(dist = list(norm = "L2"))
                    )
  
  return(hc_res)
  
}

# # data read
# data_all_trajectories <- fread("data.csv")
# #
# # save stats of dt
# n_col <- ncol(data_all_trajectories)
# n_row <- nrow(data_all_trajectories)
# 
# n_row_na <- rowSums(data_all_trajectories[, lapply(.SD, is.na)])
# n_col_na <- colSums(data_all_trajectories[, lapply(.SD, is.na)])
# 
# # remove all NA rows and cols
# 
# if (length(which(n_row_na %in% n_col)) != 0) {
# 
#   data_all_trajectories <- copy(data_all_trajectories[-which(n_row_na == n_col)])
# 
# }
# 
# if (length(which(n_col_na %in% n_row)) != 0) {
# 
#   data_all_trajectories <- copy(data_all_trajectories[, -which(n_col_na %in% n_row), with = FALSE])
# 
# }
# # transpose data for clustering
# data_trajectories_trans <- t(data_all_trajectories[, .SD,
#                                                    .SDcols = colnames(data_all_trajectories)[-1]])
# 
# # as.list(data_trajectories_trans)
# # 
# # class(data_trajectories_trans)
# # tail(data_trajectories_trans)
# 
# data_trajectories_trans_list <- lapply(1:nrow(data_trajectories_trans), function(i) na.omit(data_trajectories_trans[i,]))
# names(data_trajectories_trans_list) <- colnames(data_all_trajectories)[-1]
# 
# # some_stat <- lapply(1:length(data_trajectories_trans_list), function(i) length(data_trajectories_trans_list[[i]]))
# 
# library(dtwclust)
# hc_res <- tsclust(data_trajectories_trans_list,
#                   type = "hierarchical",
#                   k = 12L,
#                   distance = "dtw_basic", # "dtw", "dtw_basic", "dtw2", "sdtw"
#                   centroid = dba, # dba, sdtw_cent
#                   trace = TRUE,
#                   control = hierarchical_control(method = "ward.D2"),
#                   args = tsclust_args(dist = list(norm = "L2"))
#                   )
# 
# hc_res@centroids
# 
# data_clust_id <- data.table(Cluster = hc_res@cluster,
#                             Country = names(hc_res@cluster))
# 
# data_plot <- melt(data_all_trajectories,
#                   id.vars = c("Days_since_first_100_case"),
#                   variable.name = "Country",
#                   variable.factor = FALSE,
#                   value.name = "Active_cases",
#                   value.factor = F
#                   )
# 
# data_plot[data_clust_id, on = .(Country), Cluster := i.Cluster]
# 
# # prepare centroids
# centers <- as.data.table(reshape2::melt(hc_res@centroids))
# setnames(centers, "L1", "Cluster")
# centers[, ("Days_since_first_100_case") := 1:.N, by = .(Cluster)]
# setnames(centers, "value", "Active_cases")
# centers[, Country := as.character(Cluster)]
# 
# # Set colors for clusters
# data_clust_colors <- data.table(Cluster = 1:12L,
#                                 # Color = RColorBrewer::brewer.pal(k, name = "Set2")
#                                 Color = colorspace::rainbow_hcl(12L, c = 90, l = 50)
#                                 )
# 
# theme_my <- theme(panel.border = element_rect(fill = NA,
#                                               colour = "grey10"),
#                   panel.background = element_blank(),
#                   panel.grid.minor = element_line(colour = "grey85"),
#                   panel.grid.major = element_line(colour = "grey85"),
#                   panel.grid.major.x = element_line(colour = "grey85"),
#                   axis.text = element_text(size = 13, face = "bold"),
#                   axis.title = element_text(size = 14, face = "bold"),
#                   plot.title = element_text(size = 16, face = "bold"),
#                   strip.text = element_text(size = 16, face = "bold"),
#                   strip.background = element_rect(colour = "black"),
#                   legend.text = element_text(size = 15),
#                   legend.title = element_text(size = 16, face = "bold"),
#                   legend.background = element_rect(fill = "white"),
#                   legend.key = element_rect(fill = "white"),
#                   legend.position="bottom")
# 
# # plot the results
# ggplot(data_plot, aes(Days_since_first_100_case, Active_cases, group = Country)) +
#   facet_wrap(~Cluster, ncol = ceiling(data_plot[, sqrt(uniqueN(Cluster))]),
#              scales = "free") +
#   geom_line(color = "grey10", alpha = 0.75, size = 0.6) +
#   geom_line(data = centers, aes(Days_since_first_100_case, Active_cases,
#                                 color = as.factor(Cluster)),
#             # color = "firebrick1",
#             alpha = 0.95, size = 1.4, linetype = "longdash") +
#   scale_color_manual(values = data_clust_colors$Color) +
#   labs(x = "Days_since_first_100_case", y = "Active_cases") +
#   guides(color = FALSE) +
#   theme_my
# 
# k <- 3
# gg_clust <- ggplot(data_plot[Cluster == k], aes(Days_since_first_100_case, Active_cases, group = Country)) +
#   geom_line(data = centers[Cluster == k], aes(Days_since_first_100_case, Active_cases),
#             linetype = "longdash", color = data_clust_colors[Cluster == k, Color],
#             alpha = 0.95, size = 1.2) +
#   geom_line(color = "grey10", alpha = 0.75, size = 0.5) +
#   labs(title = paste0("Cluster: ", k)) +
#   theme_my
# 
# library(plotly)
# ggplotly(gg_clust)
# # distmat ot MDS ----
# hc_res@distmat
# 
# mds_classical <- cmdscale(hc_res@distmat, eig = FALSE, k = 2) # very slow, be aware!
# # ds_nonmetric <- isoMDS(d, k = 2)$points
# 
# data_plot <- data.table(mds_classical,
#                         Country = row.names(mds_classical))
#                         # Cluster = data_res$Cluster)
# 
# gg_scatter <- ggplot(data_plot, aes(x = get("V1"),
#                                     y = get("V2"),
#                                     label = Country
#                                     # color = as.factor(Cluster)
# )
# ) +
#   geom_label_repel(
#     alpha = 0.95,
#     segment.alpha = 0.35,
#     label.r = 0.1,
#     box.padding = 0.25,
#     label.padding = 0.3,
#     label.size = 0.35,
#     max.iter = 2500) +
#   # scale_color_manual(values = clust_res$clusters_colors$Color) +
#   labs(x = NULL,
#        y = NULL,
#        color = NULL) +
#   guides(color = FALSE) +
#   theme_bw()
# 
# gg_scatter
# plot(hc_res)
# # plot(hc_res, type = "c")
# # plot(hc_res, type = "series")
# plot(hc_res, type = "sc", linetype = "longdash", size = 1.2) + theme_bw()
# hc_res
# 
# dend <- as.dendrogram(hc)
# 
# dend <- dend %>%
#   color_branches(k = 12) %>%
#   color_labels(k = 12) %>%
#   set("branches_lwd", 1) %>%
#   # set("labels", dt_order[, Country]) %>%
#   set("labels_cex", 0.9)
# 
# # Set colors for clusters
# data_clust_colors <- data.table(Cluster = 1:12,
#                                 # Color = RColorBrewer::brewer.pal(k, name = "Set2")
#                                 Color = colorspace::rainbow_hcl(12, c = 90, l = 50))
# 
# lolo <- cutree(hc, k = 12)
# # join
# data_clust_merge <- data.table(Cluster = lolo,
#                                Country = names(lolo))
# data_clust_merge[data_clust_colors,
#                  on = .(Cluster),
#                  Color := i.Color]
# 
# ord <- order.dendrogram(dend)
# kx <- sort_levels_values(lolo[ord])
# kx <- kx[match(seq_along(ord), ord)]
# 
# dt_order <- data.table(Old_order = ord,
#                        New_order = 1:length(ord))
# 
# data_clust_merge[, Old_order := 1:.N]
# data_clust_merge[dt_order, on = .(Old_order), New_order := i.New_order]
# setorder(data_clust_merge, New_order)
# 
# par(mar = c(10,2,1,1))
# plot(dend)
# colored_bars(colors = data_clust_merge[, Color], dend = dend)
# always show top countries ot the top of dendogram
# ggd1 <- as.ggdend(dend)
# 
# gg_dendo <- ggplot(ggd1,
#                    horiz = T)
# 
# gg_dendo
# pc <- tsclust(data_trajectories_trans_list,
#               type = "partitional",
#               k = 10L, 
#               distance = "dtw_basic",
#               centroid = "pam", 
#               # seed = 3247L,
#               trace = TRUE,
#               args = tsclust_args(dist = list(window.size = 5L))
#               )
# 
# pc@cluster