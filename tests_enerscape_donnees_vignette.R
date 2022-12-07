## Test 'enerscape' avec les données de la vignette
## https://emilio-berti.github.io/enerscape.html

setwd("D:/00_M2_Stats/MagotsBarrieres/tests_enerscape")

## Introduction
## https://emilio-berti.github.io/enerscape.html#introduction
## librairies
library(tidyverse)
library(raster)
library(sf)
library(enerscape)
library(gdistance)

## données
attach(pontzer)
#pontzer_data <- pontzer

#' Function to plot the areas
#' 
#' @param x is a raster
#' @param poly is a shapefile polygon
plot_area <- function(x, poly, mask = NULL, col = NULL, void = FALSE) {
  if (is.null(col)) {
    plot(x, axes = !void, box = !void)
  } else {
    plot(x, col = col, axes = !void, box = !void)
  }
  if (!is.null(mask)) {
    plot(mask, add = T, legend = FALSE,
         col = adjustcolor("white", alpha.f = 0.5), 
         axes = !void, 
         box = !void)
  }
  plot(poly$geometry, add = TRUE)
}


#' ARC model predictions
#'
#' @param mass body mass of the animal (kg)
#' @param slope incline of the terrain (degree)
ARC <- function(mass, slope) {
  E_ar <- 8 * mass^(-0.34)
  E_mec <- 50 * (1 + sin((2 * slope - 74) / 180 * pi)) * mass^(-0.12)
  E <- E_ar + E_mec
  return(E)
}

pontzer %>%  ## on utilise les données pontzer du package Enerscape
  as_tibble %>%  ## on les transforme en un objet R de type 'tibble' --> dans quel but?
  mutate(ARC = ARC(Mass, Incline)) %>% 
  ## on ajoute une colonne ARC qui contient le résultat de la fonction ARC appliqué à Mass et Incline
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(aes(ARC, Cost.of.Transport, col = Incline)) +
  scale_color_gradient(low = "steelblue", high = "tomato") +
  scale_x_log10(n.breaks = 11) +
  scale_y_log10(n.breaks = 11) +
  xlab("ARC predicitons") +
  ylab(expression("E"["COT"]*" (J kg"^"-1"*"m"^"-1"*")")) +
  theme_bw()

dem <- raster("data/dem-vignette.tif")
## où trouver le .tif? ok réponse de l'auteur
res(dem) # function res, package raster: get the resolution
crs(dem) # crs== Coordinate Reference System
en <- enerscape(dem, 50)

extr <- en_extrapolation(en, plot = FALSE)
extr

plot(dem)
plot(extr$`Slope extrapolation`, 
     add = TRUE, 
     col = adjustcolor("blue", alpha.f = 0.3),
     legend = FALSE)
arrows(888523, 4674275, 878791, 4671549)

lcp <- en_lcp(en, simulate_random_points = TRUE)

## Landscape connectivity for Marsican bear in the Sirentee-Velino Park
# dem <- raster("data/dem-vignette.tif")
park <- st_read("data/SVRP-polygon.shp")
m <- mask(dem, park, inverse = TRUE)
plot_area(dem, park, m, void = TRUE)
title("Elevation (m)")
scalebar(10000, 
         type = "bar", 
         divs = 2, 
         label = as.character(c(0, 5, 10)),
         below = "kilometers")
## Calculating the energy landscape for a 140 kg bear
en <- enerscape(dem, 140, "kcal")
plot(en$rasters, axes = FALSE, box = FALSE)

## Calculating the cost of travel per cell 
plot_area(en$rasters$Work, park, m, col = topo.colors(100), void = TRUE)
## ?? Erreur pb avec la désignation de l'objet 'en$rasters$Work ??
title("Energy cost of travel (kcal)")



## Choix de 2 points pour calculer le chemin de moindre coût
## en utilisant Circuitscape
p <- data.frame(x = c(877367, 882653),
                y = c(4674192, 4677413))
plot_area(dem, park, m, void = TRUE)
points(p, pch = 20)

lcp <- en_lcp(en, p[1, ], p[2, ])


