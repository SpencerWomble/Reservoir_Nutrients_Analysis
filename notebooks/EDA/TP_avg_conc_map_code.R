

# ============================================================================
# Mapping mean tp concentration by sampling station
# ----------------------------------------------------------------------------
# One dot per station, placed at its coordinates, with dot SIZE scaled to the
# station's average tp across the full monitoring record. Bigger dot = higher
# average tp.
#
# Spatial stack:
#   sf        -> stores points as a spatial object and tracks the coordinate
#                reference system (CRS), so projection is handled correctly.
#   ggplot2   -> draws the map (sf integrates directly via geom_sf).
#   ggspatial -> adds an online basemap tile, a scale bar, and a north arrow.
# ============================================================================


# ---- 1. Packages -----------------------------------------------------------
# install.packages(c("sf", "ggspatial", "prettymapr", "rosm")) if needed.
# prettymapr + rosm are what ggspatial uses under the hood to fetch tiles.
library(tidyverse)
library(sf)
library(ggspatial)
library(prettymapr)
library(raster)
library(maptiles)
library(tidyterra)

library(sf)
library(nhdplusTools)
library(ggplot2)
library(purrr)


# ---- 2. Get observation-level data -----------------------------------------
# You need, at minimum, per observation:
#   station id, longitude, latitude, and the tp value.
#

data<- read.csv("data/cleaned_data_long.csv")

str(data)

tp_record<- data |>
  dplyr::mutate(characteristic = if_else(
    characteristic =="Phosphorus", "tp", characteristic)) |> 
  dplyr::filter(characteristic == "tp" & sample_fraction == "Total") |> 
  dplyr::select(site, id, longitude, latitude, measure_value) |> 
  dplyr::rename(tp = measure_value)

head(tp_record)

# ---- 3. Collapse to one mean per station -----------------------------------
# Average tp over the whole record, carrying the coordinates through.
# (Coordinates are constant within a station, so first() is just "keep it".)
#
# NOTE on censoring: this is a plain arithmetic mean. With left-censored tp
# (values below MDL), a naive mean is biased. For a visual it is usually fine,
# but if you want the dots to reflect censoring-aware summaries you can swap
# this line for a station-level estimate from NADA::cenfit() or pull the
# posterior station means out of your brms model and join them in here. The
# rest of the script does not care how mean_tp was produced.
station_means <- tp_record |>
  dplyr::group_by(id, site) |>
  dplyr::summarise(
    mean_tp = mean(tp, na.rm = TRUE),
    lon      = dplyr::first(longitude),
    lat      = dplyr::first(latitude),
    n_obs    = dplyr::n(),
    .groups  = "drop"
  )

station_means


# ---- 4. Turn the table into a spatial (sf) object --------------------------
# st_as_sf() converts the lon/lat columns into a geometry column.
#   coords = c("lon", "lat") -> names of the x then y columns, IN THAT ORDER.
#   crs = 4326               -> EPSG:4326 is plain WGS84 longitude/latitude,
#                               which is what GPS / most raw coordinates are.
# Getting the CRS right is the one step people miss; without it ggspatial does
# not know how to line your points up with the basemap.
stations_sf <- sf::st_as_sf(
  station_means,
  coords = c("lon", "lat"),
  crs    = 4326
)


# ---- 5. The map ------------------------------------------------------------
# Reading order of the layers (bottom to top):
#   annotation_map_tile -> basemap underneath everything.
#   geom_sf             -> your station dots, size mapped to mean_tp.
#   scale_size_area     -> makes dot AREA (not radius) proportional to value,
#                          which is how people actually read bubble size, so
#                          the encoding is honest. max_size tunes the largest.
#   annotation_scale / north_arrow -> cartographic furniture.
#   coord_sf            -> ties the whole thing to a CRS for display.


make_tp_map <- function(site_id, stations, buffer_m = 5000) {

  sites <- subset(stations, site == site_id)

  aoi <- sites |>
    st_transform(5070) |>
    st_union() |>
    st_buffer(buffer_m) |>
    st_transform(4326)

  waterbodies <- get_waterbodies(AOI = aoi)
  flowlines   <- get_nhdplus(AOI = aoi, realization = "flowline")
  flowlines_main <- subset(flowlines, streamorde >= 2)

  pretty_name <- tools::toTitleCase(gsub("_", " ", site_id))

  ggplot() +
    geom_sf(data = waterbodies, fill = "#cfe6ea", colour = NA) +
    geom_sf(data = flowlines_main, colour = "#9ec9d1", linewidth = 0.3) +
    geom_sf(
      data   = sites,
      aes(size = mean_tp),
      shape  = 21,
      fill   = "#1f6f78",
      colour = "white",
      stroke = 0.4,
      alpha  = 0.85
    ) +
    scale_size_area(max_size = 14, name = expression(Mean~TP~(mg~L^-1))) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_fancy_orienteering()
    ) +
    coord_sf(crs = 4326) +
    labs(
      title    = paste0("Average TP concentration by sampling station: ", pretty_name),
      subtitle = "Dot area proportional to record-long mean",
      caption  = "Hydrography: USGS NHDPlus"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_line(colour = grey(0.92))
    )
}

# Build all three, named so you can pull any one out
site_ids <- unique(tp_record$site)
tp_maps <- map(set_names(site_ids), make_tp_map, stations = stations_sf)

tp_maps$center_hill
tp_maps$dale_hollow
tp_maps$percy_priest




# ---- 6. Save ---------------------------------------------------------------
# Vector PDF for manuscripts; PNG for slides/quick looks.
#ggsave("tp_station_map.pdf", tp_map, width = 8, height = 6.5)
#ggsave("tp_station_map.png", tp_map, width = 8, height = 6.5, dpi = 300)


# ============================================================================
# Optional tweaks
# ----------------------------------------------------------------------------
# Redundant colour encoding (area + colour both from mean_tp). Some find this
# easier to read; drop it in by adding aes(fill = mean_tp) and replacing the
# fixed fill with a scale:
#
#   geom_sf(aes(size = mean_tp, fill = mean_tp), shape = 21,
#           colour = "white", stroke = 0.4, alpha = 0.9) +
#   scale_fill_viridis_c(option = "mako", direction = -1,
#                        name = expression(Mean~NO[x]~(mg~L^-1))) +
#   guides(fill = guide_legend(), size = guide_legend())  # merge legends
#
# Label the stations: add library(ggrepel) and
#   ggrepel::geom_text_repel(aes(label = station, geometry = geometry),
#                            stat = "sf_coordinates", size = 3)
#
# No internet / tiles failing? Delete the annotation_map_tile() line and the
# map still draws as bare dots; or add a county/state outline from
# tigris::counties("TN") (also an sf object) as a static backdrop instead.
# ============================================================================