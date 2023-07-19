library(motif)
library(terra)
library(sf)
france <- rast("data/raster_france.tif")
plot(france)

# Spatial signatures
france_comp <- lsp_signature(france, type="composition")
france_comp

france_comp$signature[[1]]

france_cove <- lsp_signature(france, type="cove")
france_cove

france_cove2 <- lsp_signature(france, type="cove", window=50)
france_cove2

france_cove2_sf <- lsp_add_sf(france_cove2)
plot(france, ext=ext(france_cove2_sf))
plot(st_geometry(france_cove2_sf), add=TRUE)

sample_points <- read_sf("data/france_sample_points.gpkg")
sample_polys <- st_buffer(sample_points, dist=300)
france_cove3 <- lsp_signature(france, type="cove", window=sample_polys)
france_cove3

# Landscape patterns’ comparison
france2000 <- rast("data/raster_france2000.tif")

lc_change1 <- lsp_compare(france2000, france,
                          type="cove", dist_fun="jensen-shannon", 
                          output="terra")
lc_change1["dist"]

lc_change <- lsp_compare(france2000, france,
                         type="cove", dist_fun="jensen-shannon", 
                         output="terra", window=50)

library(tmap)
tm1 <- tm_shape(france2000, bbox=st_bbox(france2000)) +
  tm_raster(legend.show=FALSE) +
  tm_layout(main.title="2001")
tm2 <- tm_shape(france, bbox=st_bbox(france2000)) +
  tm_raster(legend.show=FALSE) +
  tm_layout(main.title="2019")
tm3 <- tm_shape(lc_change[["dist"]], bbox=st_bbox(france2000)) +
  tm_raster(style="headtails", palette="-YlGnBu", title = "") +
  tm_layout(main.title="Dissimilarity", legend.bg.color = "white", legend.text.size = 0.4)
tmap_arrange(tm1, tm2, tm3, nrow=1)

lc_change_df <- as.data.frame(lc_change)
subset(lc_change_df, dist>0.05)
compare_1 <- lsp_extract(c(france2000, france), window=50, id=2107)
plot(compare_1, legend = FALSE)

# Landscape patterns’ search
study_area <- read_sf("data/france_study_area.gpkg")
france_study_area <- crop(france, study_area, mask=TRUE)

nlcd_search <- lsp_search(france_study_area, france,
                         type="cove", dist_fun="jensen-shannon",
                         output="terra", window=50)

plot(nlcd_search[["dist"]])

nlcd_search_df <- as.data.frame(nlcd_search)
subset(nlcd_search_df, dist<0.001)

search_1 <- lsp_extract(france, window=50, id=1303)
plot(search_1)

# Landscape patterns’ regionalization, clustering, and machine learning
france_cove2 <- lsp_signature(france, type="cove", window=10)
france_cove_terra <- lsp_add_terra(france_cove2, metadata=FALSE)
france_cove_terra

library(supercells)
france_sc <- supercells(france_cove_terra, k=200, compactness=0.6,
                        dist_fun="jensen-shannon", metadata=FALSE)

plot(france)
plot(st_geometry(france_sc), add=TRUE, border="red")

set.seed(2023-04-26)
france_sc_df <- st_drop_geometry(france_sc) 
france_sc_dist <- philentropy::distance(france_sc_df, 
                                        method="jensen-shannon",
                                        as.dist.obj=TRUE)
hc <- hclust(france_sc_dist)
france_sc$k <- cutree(hc, 4)

france_sc2 <- aggregate(france_sc, by=list(france_sc$k), FUN=mean)

tmsc1 <- tm_shape(france) +
  tm_raster(legend.show=FALSE) +
  tm_shape(france_sc) +
  tm_borders(col="red") +
  tm_layout(main.title="A")
tmsc2 <- tm_shape(france) +
  tm_raster(legend.show=FALSE) +
  tm_shape(france_sc2) +
  tm_borders(col="red") +
  tm_layout(main.title="C")
tmsc3 <- tm_shape(france) +
  tm_raster(legend.show=FALSE) +
  tm_shape(france_sc2) +
  tm_polygons("k", style="cat", palette="Set3", title="") +
  tm_layout(main.title="B", legend.bg.color="white", legend.text.size = 0.4,
            legend.position=c("LEFT", "TOP"))
tmap_arrange(tmsc1, tmsc3, tmsc2, nrow=1)


