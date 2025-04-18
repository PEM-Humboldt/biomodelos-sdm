# FilterHabitatByPresence
# Version 2 - 10/04/2025
# Author: E. A. Noguera-Urbano
# Description:
# This function filters a binary habitat raster to retain only patches 
# that are spatially associated with at least one species presence point.
# Useful for post-processing species distribution models or habitat suitability maps
# to reduce overprediction by limiting outputs to areas with confirmed presence.
# Dependencies: terra 1.8-29

```        
library(terra)
      
```

#' Filter Habitat Patches by Presence
#'
#' This function takes a binary raster representing suitable habitat (1 = habitat, 0 = non-habitat or NA)
#' and a set of species presence points. It identifies habitat patches and retains only those 
#' intersected by the presence points. The result is saved as a new raster and returned.
#'
#' @param map A SpatRaster object. Must contain binary values (1 = habitat, 0 or NA = non-habitat).
#' @param sp.points A set of spatial points (e.g., from a data.frame or SpatVector) representing species presences.
#' @param namras Character. Filename prefix for the output raster (default is "output").
#'
#' @return A SpatRaster with only habitat patches where presence points occur. All other cells are set to NA.
#' @export
#'

FilterHabitatByPresence <- function(map, sp.points, namras = "output") {
  # Ensure NA values are treated as absence (set to 0)
  map[is.na(map)] <- 0
  
  # Create binary habitat mask (1 = habitat, NA = non-habitat)
  habitat_bin <- classify(map, cbind(0, NA), others = 1)
  
  # Identify contiguous habitat patches (8-directional connectivity)
  map_patch <- patches(habitat_bin, directions = 8)
  
  # Extract patch IDs where species presence points fall
  patch_extract <- extract(map_patch, sp.points)
  patch_ids <- unique(patch_extract[, 2])
  patch_ids <- patch_ids[!is.na(patch_ids)]
  
  # If no presence points fall on valid patches
  if (length(patch_ids) == 0) {
    warning("⚠️ No presence points fall within habitat")
    map[] <- NA
    return(map)
  }
  
  # Retain only patches with the presence evidence
  mask <- map_patch
  mask[!(map_patch[] %in% patch_ids)] <- NA
  mask[mask > 0] <- 1
  
  # Multiply by original map to keep original values (in case values differ from 1)
  result <- mask * map
  
  # Write result to file
  writeRaster(result, filename = paste0(namras, ".tif"), overwrite = TRUE)
  
  return(result)
}


### 🧪 Example: Simulating a Binary Raster with Intersecting Presence Points

The following example demonstrates how to simulate a binary habitat raster and test the `FilterHabitatByPresence` function by selecting only the patch intersecting with species presence points.

```r
# Set seed for reproducibility
set.seed(42)

# Simulate a binary raster (1 = habitat, 0 = non-habitat)
r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
values(r) <- sample(c(0, 1), ncell(r), replace = TRUE, prob = c(0.7, 0.3))

# Plot the original raster
plot(r, main = "Original Raster (habitat = 1)")

# Extract coordinates of habitat cells (value == 1)
hab_cells <- which(values(r) == 1)
hab_coords <- xyFromCell(r, hab_cells)

# Randomly select some coordinates to simulate presence points
pres_coords <- hab_coords[sample(1:nrow(hab_coords), 3), ]
points(pres_coords, col = "red", pch = 16)

# Convert selected points to SpatVector
presencias <- vect(pres_coords, type = "points", crs = crs(r))

# Apply the FilterHabitatByPresence function to retain only patches with presence
r_cut <- FilterHabitatByPresence(r, presencias)

# Plot the resulting raster with only relevant patches
plot(r_cut, main = "Result: Patches with Presence")
points(pres_coords, col = "red", pch = 16)
