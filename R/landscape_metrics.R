library(landscapemetrics)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

france <- rast("data/raster_france.tif")
netherlands <- rast("data/raster_netherlands.tif")
sweden <- rast("data/raster_sweden.tif")

check_landscape(france)

# Deriving landscape metrics
list_lsm()
list_lsm(type="area and edge metric", level="class")

df_p_shape <- lsm_p_shape(france)
df_c_area <- lsm_c_area_mn(france)
df_l_lpi <- lsm_l_lpi(france)

result_combined <- rbind(df_p_shape, df_c_area, df_l_lpi)
result_range <- group_by(result_combined, metric) |> 
  summarise(min = min(value), max = max(value))
result_range

df_area <- calculate_lsm(france,
                         what=c("lsm_p_area", "lsm_c_area_mn", "lsm_l_ta"))
df_aggr_lsm <- calculate_lsm(france,
                             level="landscape", type="aggregation metric")

df_np_queen <- lsm_c_np(france, directions=8)
df_np_rook <- lsm_c_np(france, directions=4)
df_comparison <- full_join(x=df_np_queen, y=df_np_rook, 
                           by=c("layer", "level", "class", "id", "metric"), 
                           suffix=c(".queen", ".rook")) |> 
  mutate(value.diff=abs(value.queen - value.rook))
df_comparison

df_perim_core <- list(nl=netherlands, fr=france) |>
  calculate_lsm(what=c("lsm_p_perim", "lsm_p_area")) |>
  filter(class %in% 3) |>
  mutate(layer=case_when(layer==1~"Netherlands",
                         layer==2~"France")) |>
  pivot_wider(names_from=metric, values_from=value) |>
  filter(area<=quantile(area, probs=0.95) &
         perim<=quantile(perim, probs=0.95))
ggplot(data=df_perim_core, aes(x=area, y=perim, color=layer)) +
  geom_point(alpha=0.1) +
  geom_smooth(se=FALSE, method="lm", formula="y ~ x") +
  scale_color_manual(name="Country",
                     values=c(Netherlands="#F79400",
                              France="#001E96")) +
  labs(x="Patch area [m2]", y="Patch perimeter [m]") +
  theme_classic() + theme(legend.position = c(0.9, 0.1))

# Visualization of landscape metrics
show_patches(france, class=3)

list_gg_area <- list(fr=france, nl=netherlands) |>
  show_lsm(class=3, what="lsm_p_area")
cowplot::plot_grid(plotlist=list_gg_area, ncol=2, labels=c("NL", "FR"))

class_metrics <- calculate_lsm(netherlands, level="class",
                               type="aggregation metric")
show_correlation(class_metrics)

# Additional features
set.seed(2023-06-28)
samplepoints <- spatSample(france, size=10, as.points=TRUE) |> 
  st_as_sf()

df_extract <- extract_lsm(france, samplepoints,
                          what=c("lsm_p_area", "lsm_p_perim"))
df_extract

df_sample <- sample_lsm(france, samplepoints,
                        what=c("lsm_c_pland", "lsm_l_ta"), 
                        size=10000, shape="circle")
filter(df_sample, percentage_inside>=75)

list_shape <- spatialize_lsm(netherlands,
                             what=c("lsm_p_shape", "lsm_p_frac"))
class(list_shape)
str(list_shape)
values(list_shape$layer_1$lsm_p_frac, mat=FALSE) |> density()

## mat_window <- matrix(1, nrow=501, ncol=501)
## window_lsm(netherlands, window=mat_window, what="lsm_l_pr")

# Utility functions
list_patches <- get_patches(france)
ras_urban <- list_patches$layer_1[1:5] |> 
  rast() |> 
  sum(na.rm=TRUE)
ras_urban

ras_boundaries_urban <- get_boundaries(list_patches$layer_1[1:5])
lapply(ras_boundaries_urban, freq, wide=TRUE)

vec_unique <- get_unique_values(sweden)[[1]]
mat_adjacencies <- get_adjacencies(sweden)[[1]]
dim(mat_adjacencies)
length(vec_unique)

sweden_cl1 <- get_patches(sweden, class=1)
df_neighbour <- sweden_cl1$layer_1$class_1 |>
  get_nearestneighbour(return_id=TRUE) |> 
  arrange(dist)

df_circle <- get_circumscribingcircle(netherlands)
df_centroids <- get_centroids(netherlands) |>
  dplyr::filter(class==4)
as.data.frame(netherlands, xy=TRUE) |>
  ggplot(aes(x=x, y=y)) +
  geom_raster(aes(fill=as.factor(cover))) +
  geom_point(data=df_centroids, aes(x=x, y=y), shape=3, size=2.5) +
  scale_fill_manual(values=c(urban="#C86058", agriculture="#FCE569", vegetation="#44A321",
                             marshes="#A3A6FF", water="#00CFFD", nodata="#666666")) +
  coord_equal() + theme_void() + theme(legend.position="none")
