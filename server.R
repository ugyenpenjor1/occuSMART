################################################################################
# SMART Data Analysis & Visualisation Dashboard
# Author: Ugyen Penjor
# Update: January 2026
################################################################################

library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(sf)
library(leaflet)
library(terra)
library(corrplot)
library(unmarked)
library(ggplot2)
library(plotly)
library(manipulateWidget)
library(ROCR)
library(pROC)
library(progress)
library(shinyjs)
library(shinymaterial)
library(ggcorrplot)
library(MuMIn)
library(osmdata)
library(spatstat.geom)
library(elevatr)
library(RCurl)
library(stringr)

# Helper function - environmental data processing
process_environmental_data <- function(
    aoi,
    mask = FALSE,
    osm_resolution = 512,
    dem_terrain_metrics = TRUE,
    gfc_target_year = 2024,
    skip_on_error = TRUE,
    show_progress = TRUE,
    process_osm = TRUE,
    process_chm = TRUE,
    process_dem = TRUE,
    process_gfc = TRUE,
    process_hii = TRUE
) {

  # Package dependency checks (need to update with all packages)
  required_packages <- c(
    "sf", "terra", "osmdata", "spatstat.geom",
    "elevatr", "RCurl", "stringr"
  )
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste0(
      "ERROR: Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      "\nPlease install with: install.packages(c('",
      paste(missing_packages, collapse = "', '"), "'))"
    ))
  }
  
  # Progress bar
  create_custom_progress <- function(total_steps, label) {
    if (!show_progress) {
      return(NULL)
    }
    list(
      total = total_steps,
      current = 0,
      label = label,
      start_time = Sys.time()
    )
  }
  
  update_custom_progress <- function(pb, message = NULL) {
    if (is.null(pb)) {
      return(invisible(NULL))
    }
    
    pb$current <- pb$current + 1
    percent <- round(100 * pb$current / pb$total)
    
    # Create progress bar with custom characters
    bar_width <- 40
    filled <- floor(bar_width * pb$current / pb$total)
    bar <- paste0(
      strrep("=", filled),
      ifelse(filled < bar_width, ">", ""),
      strrep("-", max(0, bar_width - filled - 1))
    )
    
    # Calculate elapsed time
    elapsed <- difftime(Sys.time(), pb$start_time, units = "secs")
    
    # Print progress
    cat("\r", sprintf(
      "[%s] %s |%s| %3d%%",
      pb$label,
      ifelse(is.null(message), "", message),
      bar,
      percent
    ))
    
    if (pb$current >= pb$total) {
      cat(sprintf(" ✓ (%.1fs)\n", elapsed))
    }
    
    flush.console()
    invisible(pb)
  }
  
  # Auto UTM transform
  auto_utm_transform <- function(sf_object) {
    if (is.na(sf::st_crs(sf_object))) {
      stop("Input sf object has no CRS defined.")
    }
    sf_wgs84 <- sf::st_transform(sf_object, crs = 4326)
    centroid <- sf::st_centroid(sf::st_union(sf_wgs84))
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]
    lat <- coords[1, "Y"]
    utm_zone <- floor((lon + 180) / 6) + 1
    epsg_code <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)
    sf::st_transform(sf_object, crs = epsg_code)
  }
  
  # DATASET 1: OSM Data (Buildings, Highways, Waterways)
  process_osm_section <- function(aoi_sf, aoi_utm, mask_flag) {
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  DATASET 1/5: OpenStreetMap Data (buildings, highways, waterways)\n")
    cat(strrep("═", 70), "\n\n")
    
    pb <- create_custom_progress(3, "OSM")
    
    tryCatch(
      {
        # Prepare
        aoi_wgs <- sf::st_transform(aoi_sf, 4326)
        bbox <- sf::st_bbox(aoi_wgs)
        
        # Multiple Overpass servers
        overpass_servers <- c(
          "https://overpass.kumi.systems/api/interpreter",
          "https://overpass-api.de/api/interpreter",
          "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
        )
        
        # Helper function for individual features with smart retry
        process_osm_feature <- function(feature_name, feature_key, feature_value = NULL, geometry_type) {
          max_retries <- 3
          retry_delay <- 5 # seconds
          
          for (attempt in 1:max_retries) {
            # Try different servers on different attempts
            server_idx <- ((attempt - 1) %% length(overpass_servers)) + 1
            current_server <- overpass_servers[server_idx]
            
            if (attempt > 1) {
              cat(sprintf(
                "\n  Retry %d/%d for %s (using server %d)...",
                attempt - 1, max_retries - 1, feature_name, server_idx
              ))
              Sys.sleep(retry_delay)
            }
            
            # Set current server
            tryCatch(
              {
                osmdata::set_overpass_url(current_server)
              },
              error = function(e) {
                # If setting server fails, try next one
                next
              }
            )
            
            # Build query
            osm_query <- tryCatch(
              {
                if (!is.null(feature_value)) {
                  osmdata::opq(bbox = bbox, timeout = 500) |>
                    osmdata::add_osm_feature(key = feature_key, value = feature_value)
                } else {
                  osmdata::opq(bbox = bbox, timeout = 500) |>
                    osmdata::add_osm_feature(key = feature_key)
                }
              },
              error = function(e) {
                return(NULL)
              }
            )
            
            if (is.null(osm_query)) next
            
            # Execute query
            osm_data <- tryCatch(
              {
                osmdata::osmdata_sf(osm_query)
              },
              error = function(e) {
                # Log specific error types
                if (grepl("502|503|504", e$message)) {
                  message(sprintf("  Server error (502/503/504) for %s", feature_name))
                } else if (grepl("doc", e$message, ignore.case = TRUE)) {
                  message(sprintf("  Malformed response for %s", feature_name))
                } else {
                  message(sprintf("  Error: %s", e$message))
                }
                return(NULL)
              }
            )
            
            # If we got data, break out of retry loop
            if (!is.null(osm_data)) {
              if (attempt > 1) {
                cat(sprintf(" ✓ Success on retry %d\n", attempt - 1))
              }
              break
            }
          }
          
          if (is.null(osm_data)) {
            warning(sprintf("Failed to query %s after %d attempts", feature_name, max_retries))
            return(NULL)
          }
          
          # Extract geometry
          feature_sf <- if (geometry_type == "point") {
            if (is.null(osm_data$osm_points) || nrow(osm_data$osm_points) == 0) {
              message(sprintf("  No %s points found in AOI", feature_name))
              return(NULL)
            }
            sf::st_make_valid(osm_data$osm_points)
          } else {
            if (is.null(osm_data$osm_lines) || nrow(osm_data$osm_lines) == 0) {
              message(sprintf("  No %s lines found in AOI", feature_name))
              return(NULL)
            }
            sf::st_make_valid(osm_data$osm_lines)
          }
          
          cat(sprintf("  Found %d %s feature(s)\n", nrow(feature_sf), feature_name))
          
          # Transform and crop with error handling
          feature_utm <- tryCatch(
            {
              auto_utm_transform(feature_sf)
            },
            error = function(e) {
              warning(sprintf("Failed to transform %s: %s", feature_name, e$message))
              return(NULL)
            }
          )
          
          if (is.null(feature_utm)) {
            return(NULL)
          }
          
          feature_crop <- tryCatch(
            {
              cropped <- sf::st_crop(feature_utm, sf::st_bbox(aoi_utm))
              if (nrow(cropped) == 0) {
                message(sprintf("  No %s features within AOI bounds", feature_name))
                return(NULL)
              }
              cropped
            },
            error = function(e) {
              warning(sprintf("Failed to crop %s: %s", feature_name, e$message))
              return(NULL)
            }
          )
          
          if (is.null(feature_crop)) {
            return(NULL)
          }
          
          attr(sf::st_geometry(feature_crop), "bbox") <-
            sf::st_bbox(sf::st_as_sfc(sf::st_bbox(aoi_utm)))
          
          # Distance raster with error handling
          feature_ras <- tryCatch(
            {
              feature_spatial <- if (geometry_type == "point") {
                spatstat.geom::distmap(spatstat.geom::as.ppp(sf::st_geometry(feature_crop)), dimyx = osm_resolution)
              } else {
                spatstat.geom::distmap(spatstat.geom::as.psp(sf::st_geometry(feature_crop)), dimyx = osm_resolution)
              }
              
              ras <- terra::rast(feature_spatial)
              names(ras) <- paste0("Dist_", feature_name)
              terra::crs(ras) <- paste0("EPSG:", sf::st_crs(feature_crop)$epsg)
              
              if (mask_flag) {
                ras <- terra::mask(ras, terra::vect(aoi_utm))
              }
              
              ras
            },
            error = function(e) {
              warning(sprintf("Failed to create distance raster for %s: %s", feature_name, e$message))
              return(NULL)
            }
          )
          
          return(feature_ras)
        }
        
        # Buildings
        update_custom_progress(pb, "Downloading buildings")
        building_ras <- process_osm_feature("building", "building", "yes", "point")
        Sys.sleep(2)
        
        # Highways
        update_custom_progress(pb, "Downloading highways")
        highway_ras <- process_osm_feature("highway", "highway", NULL, "line")
        Sys.sleep(2)
        
        # Waterways
        update_custom_progress(pb, "Downloading waterways")
        waterway_ras <- process_osm_feature("waterway", "waterway", NULL, "line")
        
        # Collect results
        osm_list <- list()
        if (!is.null(building_ras)) osm_list <- c(osm_list, list(building_ras))
        if (!is.null(highway_ras)) osm_list <- c(osm_list, list(highway_ras))
        if (!is.null(waterway_ras)) osm_list <- c(osm_list, list(waterway_ras))
        
        if (length(osm_list) == 0) {
          warning("No OSM features found")
          return(NULL)
        }
        
        cat("\n  ✓ Successfully processed", length(osm_list), "OSM layer(s)\n")
        return(osm_list)
      },
      error = function(e) {
        warning(paste0("OSM processing failed: ", e$message))
        if (!skip_on_error) stop(e)
        return(NULL)
      }
    )
  }
  
  # DATASET 2: CHM (Canopy Height Model)
  process_chm_section <- function(aoi_vect, mask_flag) {
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  DATASET 2/5: Canopy Height Model (CHM)\n")
    cat(strrep("═", 70), "\n\n")
    
    pb <- create_custom_progress(4, "CHM")
    
    tryCatch(
      {
        # Ensure WGS84
        update_custom_progress(pb, "Preparing AOI")
        if (terra::crs(aoi_vect) != "EPSG:4326") {
          aoi_vect <- terra::project(aoi_vect, "EPSG:4326")
        }
        
        # Tile code generator
        tile_code <- function(lat, lon) {
          ns <- ifelse(lat >= 0, "N", "S")
          ew <- ifelse(lon >= 0, "E", "W")
          sprintf("%s%02d%s%03d", ns, abs(lat), ew, abs(lon))
        }
        
        # Find tiles
        update_custom_progress(pb, "Identifying tiles")
        bb <- terra::ext(aoi_vect)
        lats <- c(terra::ymin(bb), terra::ymax(bb))
        lons <- c(terra::xmin(bb), terra::xmax(bb))
        
        lat_seq <- seq(floor(min(lats) / 3) * 3, floor(max(lats) / 3) * 3, by = 3)
        lon_seq <- seq(floor(min(lons) / 3) * 3, floor(max(lons) / 3) * 3, by = 3)
        
        tile_list <- unlist(lapply(lat_seq, function(la) {
          sapply(lon_seq, function(lo) tile_code(la, lo))
        }))
        
        base_url <- "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files="
        tile_urls <- paste0(base_url, "ETH_GlobalCanopyHeight_10m_2020_", tile_list, "_Map.tif")
        
        # Read tiles
        update_custom_progress(pb, "Downloading tiles")
        rasters <- list()
        for (u in tile_urls) {
          r <- try(terra::rast(paste0("/vsicurl/", u)), silent = TRUE)
          if (!inherits(r, "try-error")) {
            rasters <- c(rasters, list(r))
          }
        }
        
        if (length(rasters) == 0) {
          warning("No CHM tiles found")
          return(NULL)
        }
        
        # Mosaic if needed
        update_custom_progress(pb, "Processing tiles")
        merged <- if (length(rasters) == 1) rasters[[1]] else do.call(terra::mosaic, rasters)
        
        # Crop and mask
        cropped <- terra::crop(merged, terra::ext(aoi_vect))
        result <- if (mask_flag) terra::mask(cropped, aoi_vect) else cropped
        names(result) <- "CHM"
        
        cat("\n  ✓ Successfully processed CHM\n")
        return(list(result))
      },
      error = function(e) {
        warning(paste0("CHM processing failed: ", e$message))
        if (!skip_on_error) stop(e)
        return(NULL)
      }
    )
  }
  
  # DATASET 3: DEM and Terrain Metrics
  process_dem_section <- function(aoi_sf, mask_flag, compute_terrain) {
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  DATASET 3/5: Digital Elevation Model (DEM) + Terrain Metrics\n")
    cat(strrep("═", 70), "\n\n")
    
    total_steps <- if (compute_terrain) 5 else 3
    pb <- create_custom_progress(total_steps, "DEM")
    
    tryCatch(
      {
        # Ensure WGS84
        update_custom_progress(pb, "Preparing AOI")
        if (sf::st_crs(aoi_sf)$epsg != 4326) {
          aoi_sf <- sf::st_transform(aoi_sf, crs = 4326)
        }
        
        # Download DEM
        update_custom_progress(pb, "Downloading DEM")
        dem_global <- elevatr::get_elev_raster(
          locations = aoi_sf,
          prj = sf::st_crs(aoi_sf)$proj4string,
          z = 12,
          clip = "tile"
        )
        
        dem_global <- terra::rast(dem_global)
        
        # Crop and mask
        update_custom_progress(pb, "Cropping DEM")
        aoi_vect <- terra::vect(aoi_sf)
        dem_crop <- terra::crop(dem_global, terra::ext(aoi_vect))
        
        if (mask_flag) {
          dem_crop <- terra::mask(dem_crop, aoi_vect)
        }
        
        names(dem_crop) <- "DEM"
        dem_list <- list(dem_crop)
        
        # Terrain metrics
        if (compute_terrain) {
          update_custom_progress(pb, "Computing slope")
          slope <- terra::terrain(dem_crop, v = "slope", unit = "degrees")
          names(slope) <- "Slope"
          
          update_custom_progress(pb, "Computing TRI & roughness")
          tri <- terra::terrain(dem_crop, v = "TRI", unit = "degrees")
          names(tri) <- "TRI"
          
          rough <- terra::terrain(dem_crop, v = "roughness", unit = "degrees")
          names(rough) <- "Roughness"
          
          dem_list <- c(dem_list, list(slope, tri, rough))
        }
        
        cat("\n  ✓ Successfully processed", length(dem_list), "DEM layer(s)\n")
        return(dem_list)
      },
      error = function(e) {
        warning(paste0("DEM processing failed: ", e$message))
        if (!skip_on_error) stop(e)
        return(NULL)
      }
    )
  }
  
  # DATASET 4: GFC (Global Forest Change)
  process_gfc_section <- function(aoi_sf, target_year, mask_flag) {
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  DATASET 4/5: Global Forest Change (GFC)\n")
    cat(strrep("═", 70), "\n\n")
    
    pb <- create_custom_progress(5, "GFC")
    
    tryCatch(
      {
        options(timeout = max(1000, getOption("timeout")))
        
        # Helper functions
        verify_download <- function(tile_url, local_path) {
          header <- RCurl::getURL(tile_url, nobody = 1L, header = 1L)
          header <- strsplit(header, "\r\n")[[1]]
          content_length <- header[grepl("Content-Length:", header)]
          remote_size <- as.numeric(stringr::str_extract(content_length, "[0-9]+"))
          local_size <- file.info(local_path)$size
          if (is.na(remote_size) || is.na(local_size) || remote_size != local_size) 3 else 0
        }
        
        download_tile <- function(tile_url, local_path) {
          ret <- download.file(tile_url, local_path, mode = "wb", quiet = TRUE)
          if (ret != 0) {
            return(1)
          }
          if (verify_download(tile_url, local_path) != 0) {
            return(2)
          }
          0
        }
        
        # Prepare AOI
        update_custom_progress(pb, "Preparing AOI")
        aoi_sf <- sf::st_zm(sf::st_transform(aoi_sf, 4326))
        
        # Find tiles
        update_custom_progress(pb, "Finding GFC tiles")
        load(url("https://raw.githubusercontent.com/ugyenpenjor1/open_data/main/gfc_tiles.RData"))
        if (!identical(sf::st_crs(aoi_sf), sf::st_crs(gfc_tiles))) {
          aoi_sf <- sf::st_transform(aoi_sf, sf::st_crs(gfc_tiles))
        }
        idx <- apply(sf::st_intersects(gfc_tiles, sf::st_convex_hull(aoi_sf), sparse = FALSE), 1, any)
        tiles <- gfc_tiles[idx, ]
        
        # Download tiles
        update_custom_progress(pb, "Downloading tiles")
        temp_dir <- tempfile("GFC_tiles_")
        dir.create(temp_dir)
        on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
        
        for (n in seq_len(nrow(tiles))) {
          gfc_tile <- tiles[n, ]
          bb <- sf::st_bbox(gfc_tile)
          
          min_x <- ifelse(bb["xmin"] < 0,
                          paste0(sprintf("%03i", abs(bb["xmin"])), "W"),
                          paste0(sprintf("%03i", bb["xmin"]), "E")
          )
          max_y <- ifelse(bb["ymax"] < 0,
                          paste0(sprintf("%02i", abs(bb["ymax"])), "S"),
                          paste0(sprintf("%02i", bb["ymax"]), "N")
          )
          
          file_root <- "Hansen_GFC-2024-v1.12_"
          file_suffix <- paste0("_", max_y, "_", min_x, ".tif")
          filenames <- paste0(file_root, c("treecover2000", "lossyear", "gain"), file_suffix)
          
          base_url <- "http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC-2024-v1.12/"
          
          for (fn in filenames) {
            url <- paste0(base_url, fn)
            dest <- file.path(temp_dir, fn)
            if (file.exists(dest) && verify_download(url, dest) == 0) next
            download_tile(url, dest)
          }
        }
        
        # Merge tiles
        update_custom_progress(pb, "Merging tiles")
        all_files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)
        file_types <- unique(sub("(_[0-9]+[NEWS]).*", "", basename(all_files)))
        
        for (ft in file_types) {
          files <- list.files(temp_dir, pattern = paste0(ft, ".*\\.tif$"), full.names = TRUE)
          r <- lapply(files, terra::rast)
          merged <- if (length(r) == 1) r[[1]] else do.call(terra::mosaic, r)
          terra::writeRaster(merged, file.path(temp_dir, paste0(ft, "_merged.tif")), overwrite = TRUE)
        }
        
        # Process forest cover
        update_custom_progress(pb, "Computing tree cover")
        merged_files <- list.files(temp_dir, pattern = "_merged\\.tif$", full.names = TRUE)
        
        gain_r <- terra::rast(merged_files[1])
        loss_r <- terra::rast(merged_files[2])
        tree2000_r <- terra::rast(merged_files[3])
        
        loss_mask <- terra::crop(loss_r, aoi_sf) > 0 &
          terra::crop(loss_r, aoi_sf) <= (target_year - 2000)
        
        forest_latest <- terra::crop(tree2000_r, aoi_sf)
        forest_latest[loss_mask] <- 0
        forest_latest[terra::crop(gain_r, aoi_sf) == 1 & !loss_mask] <-
          terra::crop(tree2000_r, aoi_sf)[terra::crop(gain_r, aoi_sf) == 1 & !loss_mask]
        
        # Apply mask if requested
        if (mask_flag) {
          message("  Masking GFC to AOI...")
          aoi_vect <- terra::vect(aoi_sf)
          forest_latest <- terra::mask(forest_latest, aoi_vect)
        }
        
        names(forest_latest) <- paste0("Treecover_", target_year)
        
        cat("\n  ✓ Successfully processed GFC\n")
        return(list(forest_latest))
      },
      error = function(e) {
        warning(paste0("GFC processing failed: ", e$message))
        if (!skip_on_error) stop(e)
        return(NULL)
      }
    )
  }
  
  # DATASET 5: HII (Human Impact Index)
  process_hii_section <- function(aoi_vect, mask_flag) {
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  DATASET 5/5: Human Impact Index (HII)\n")
    cat(strrep("═", 70), "\n\n")
    
    pb <- create_custom_progress(3, "HII")
    
    tryCatch(
      {
        # Ensure WGS84
        update_custom_progress(pb, "Preparing AOI")
        if (terra::crs(aoi_vect) != "EPSG:4326") {
          aoi_vect <- terra::project(aoi_vect, "EPSG:4326")
        }
        
        # Download HII
        update_custom_progress(pb, "Downloading HII")
        hii_global <- terra::rast(
          "/vsicurl/https://storage.googleapis.com/hii-export/2020-01-01/hii_2020-01-01.tif"
        )
        
        # Crop and mask
        update_custom_progress(pb, "Processing HII")
        hii_crop <- terra::crop(hii_global, terra::ext(aoi_vect))
        result <- if (mask_flag) terra::mask(hii_crop, aoi_vect) else hii_crop
        names(result) <- "HII"
        
        cat("\n  ✓ Successfully processed HII\n")
        return(list(result))
      },
      error = function(e) {
        warning(paste0("HII processing failed: ", e$message))
        if (!skip_on_error) stop(e)
        return(NULL)
      }
    )
  }
  
  # PROCESSING WORKFLOW
  cat("\n")
  cat(strrep("<", 70), "\n")
  cat("  ENVIRONMENTAL DATA PROCESSOR\n")
  cat(strrep(">", 70), "\n")
  cat(sprintf("  AOI mask: %s\n", ifelse(mask, "YES", "NO")))
  cat(sprintf("  GFC target year: %d\n", gfc_target_year))
  cat(sprintf("  Skip on error: %s\n", ifelse(skip_on_error, "YES", "NO")))
  cat(strrep("-", 70), "\n")
  
  # Validate and prepare AOI
  if (is.character(aoi) && file.exists(aoi)) {
    cat("\n[SETUP] Reading AOI from file...\n")
    aoi <- sf::st_read(aoi, quiet = TRUE)
  }
  
  if (!inherits(aoi, "sf")) {
    stop("ERROR: AOI must be an sf object or a valid shapefile path.")
  }
  
  if (is.na(sf::st_crs(aoi))) {
    stop("ERROR: AOI has no CRS defined.")
  }
  
  # Prepare different AOI formats
  aoi_sf <- aoi
  aoi_vect <- terra::vect(aoi_sf)
  aoi_utm <- auto_utm_transform(aoi_sf)
  
  cat("[SETUP] AOI prepared successfully\n")
  cat(sprintf("[SETUP] CRS: EPSG:%d\n", sf::st_crs(aoi_sf)$epsg))
  
  # Initialise results list and layer variables
  all_layers <- list()
  osm_layers <- list()
  chm_layers <- list()
  dem_layers <- list()
  gfc_layers <- list()
  hii_layers <- list()
  
  # Process each data type
  if (process_osm) {
    osm_layers <- process_osm_section(aoi_sf, aoi_utm, mask)
    if (!is.null(osm_layers)) all_layers <- c(all_layers, osm_layers)
  } else {
    cat("\n[SKIPPED] OSM data processing disabled\n")
  }
  
  if (process_chm) {
    chm_layers <- process_chm_section(aoi_vect, mask)
    if (!is.null(chm_layers)) all_layers <- c(all_layers, chm_layers)
  } else {
    cat("\n[SKIPPED] CHM data processing disabled\n")
  }
  
  if (process_dem) {
    dem_layers <- process_dem_section(aoi_sf, mask, dem_terrain_metrics)
    if (!is.null(dem_layers)) all_layers <- c(all_layers, dem_layers)
  } else {
    cat("\n[SKIPPED] DEM data processing disabled\n")
  }
  
  if (process_gfc) {
    gfc_layers <- process_gfc_section(aoi_sf, gfc_target_year, mask)
    if (!is.null(gfc_layers)) all_layers <- c(all_layers, gfc_layers)
  } else {
    cat("\n[SKIPPED] GFC data processing disabled\n")
  }
  
  if (process_hii) {
    hii_layers <- process_hii_section(aoi_vect, mask)
    if (!is.null(hii_layers)) all_layers <- c(all_layers, hii_layers)
  } else {
    cat("\n[SKIPPED] HII data processing disabled\n")
  }
  
  # Stacking
  cat("\n")
  cat(strrep("<", 70), "\n")
  cat("  FINAL STACKING\n")
  cat(strrep(">", 70), "\n\n")
  
  if (length(all_layers) == 0) {
    stop("ERROR: No data layers were successfully processed!")
  }
  
  cat(sprintf("Total layers processed: %d\n\n", length(all_layers)))
  
  # Use first valid layer as template
  template <- all_layers[[1]]
  cat("Template raster:\n")
  cat(sprintf("  Resolution: %s\n", paste(terra::res(template), collapse = " x ")))
  cat(sprintf("  Extent: %s\n\n", paste(as.vector(terra::ext(template)), collapse = ", ")))
  
  # Align all rasters to template grid
  cat("Aligning rasters to template grid...\n")
  
  all_layers_aligned <- lapply(all_layers, function(r) {
    # 1. Reproject if needed
    if (!terra::compareGeom(r, template, stopOnError = FALSE, crs = TRUE)) {
      r <- terra::project(r, template)
    }
    
    # 2. Crop to template extent
    r <- terra::crop(r, template)
    
    # 3. Resample to template grid
    r <- terra::resample(r, template, method = "bilinear")
    
    return(r)
  })
  
  # Stack
  cat("Stacking layers...\n")
  final_stack <- terra::rast(all_layers_aligned)
  
  # Print summary
  cat("\n")
  cat(strrep("═", 70), "\n")
  cat("LAYER SUMMARY:\n")
  cat(strrep("═", 70), "\n")
  for (i in 1:terra::nlyr(final_stack)) {
    cat(sprintf(
      "  %2d. %-30s [%s]\n", i, names(final_stack)[i],
      ifelse(i <= length(osm_layers), "OSM",
             ifelse(i <= length(osm_layers) + length(chm_layers), "CHM",
                    ifelse(i <= length(osm_layers) + length(chm_layers) + length(dem_layers), "DEM",
                           ifelse(i <= length(osm_layers) + length(chm_layers) + length(dem_layers) + length(gfc_layers), "GFC", "HII")
                    )
             )
      )
    ))
  }
  cat(strrep("═", 70), "\n")
  cat(sprintf(
    "\nRaster dimensions: %d rows x %d cols\n",
    terra::nrow(final_stack), terra::ncol(final_stack)
  ))
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("   PROCESSING COMPLETE! \n")
  cat(strrep("=", 70), "\n\n")
  
  return(final_stack)
}

################################################################################
# SERVER
################################################################################

# Define server
server <- function(input, output, session) {
  # Reactive object for uploaded CSV
  csv_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update the column selection options
  observe({
    col_options <- names(csv_data())
    updateSelectInput(session, "column_to_convert", choices = col_options)
  })
  
  # Create a second reactive object by adding a new column
  modified_data <- reactiveVal(NULL)
  
  # Process data when button is clicked
  observeEvent(input$process_btn, {
    req(input$file, input$column_to_convert)
    
    modified_df <- csv_data()
    
    # Create a new column 'obs' based on the condition
    modified_df$obs <- ifelse(
      modified_df[[input$column_to_convert]] == "", 0, 1
    )
    modified_df$obs <- as.numeric(modified_df$obs)
    modified_data(modified_df)
  })
  
  # Display the modified CSV data
  output$modified_table <- renderDT({
    req(modified_data())
    datatable(
      modified_data(),
      caption = "",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", # enable selection of a single row
        filter = "top", # include column filters at the bottom
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  observeEvent(input$toggle_table, {
    shinyjs::toggle("table_container")
  })
  
  # Reactive input area of interest
  map <- reactive({
    req(input$filemap)
    
    # shpdf is a data.frame with the name, size, type, and
    # datapath of the uploaded files
    shpdf <- input$filemap
    
    # Name the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    # Read the shapefile
    map <- st_read(
      paste(
        tempdirname,
        shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
        sep = "/"
      )
    )
    return(map)
  })
  
  # Environmental Data Processing
  
  # Helper function: wraps the original process_environmental_data() function
  # and filters/processes only selected datasets
  
  process_environmental_data_selective <- function(
    aoi,
    mask = FALSE,
    osm_resolution = 512,
    process_osm = TRUE,
    process_chm = TRUE,
    process_dem = TRUE,
    process_gfc = TRUE,
    process_hii = TRUE,
    osm_features = NULL,  
    dem_metrics = NULL,   
    gfc_layers = NULL,   
    gfc_target_year = 2024,
    skip_on_error = TRUE,
    show_progress = TRUE
  ) {
    
    # Initialise result list
    all_layers <- list()
    
    # Call original function to get ALL data
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  ENVIRONMENTAL DATA PROCESSING - SELECTIVE MODE\n")
    cat(strrep("═", 70), "\n")
    cat("\nProcessing selected datasets...\n\n")
    
    # Process all data first
    full_result <- process_environmental_data(
      aoi = aoi,
      mask = mask,
      osm_resolution = osm_resolution,
      dem_terrain_metrics = process_dem,
      gfc_target_year = gfc_target_year,
      skip_on_error = skip_on_error,
      show_progress = show_progress,
      process_osm = process_osm,
      process_chm = process_chm,
      process_dem = process_dem,
      process_gfc = process_gfc,
      process_hii = process_hii
    )
    
    if (is.null(full_result) || terra::nlyr(full_result) == 0) {
      warning("No environmental data layers were successfully processed")
      return(NULL)
    }
    
    # Get layer names
    all_names <- names(full_result)
    
    # Filter layers based on selection
    selected_names <- c()
    
    # OSM filtering
    if (process_osm && !is.null(osm_features)) {
      if (osm_features$building) {
        selected_names <- c(selected_names, grep("building", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (osm_features$highway) {
        selected_names <- c(selected_names, grep("highway", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (osm_features$waterway) {
        selected_names <- c(selected_names, grep("waterway", all_names, ignore.case = TRUE, value = TRUE))
      }
    }
    
    # CHM filtering
    if (process_chm) {
      selected_names <- c(selected_names, grep("chm|canopy", all_names, ignore.case = TRUE, value = TRUE))
    }
    
    # DEM filtering
    if (process_dem && !is.null(dem_metrics)) {
      if (dem_metrics$elevation) {
        selected_names <- c(selected_names, grep("^elevation$|^dem$", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (dem_metrics$slope) {
        selected_names <- c(selected_names, grep("slope", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (dem_metrics$aspect) {
        selected_names <- c(selected_names, grep("aspect", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (dem_metrics$tri) {
        selected_names <- c(selected_names, grep("^tri$", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (dem_metrics$tpi) {
        selected_names <- c(selected_names, grep("^tpi$", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (dem_metrics$roughness) {
        selected_names <- c(selected_names, grep("roughness", all_names, ignore.case = TRUE, value = TRUE))
      }
    }
    
    # GFC filtering
    if (process_gfc && !is.null(gfc_layers)) {
      if (gfc_layers$treecover) {
        selected_names <- c(selected_names, grep("treecover|tree_cover", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (gfc_layers$loss) {
        selected_names <- c(selected_names, grep("loss", all_names, ignore.case = TRUE, value = TRUE))
      }
      if (gfc_layers$gain) {
        selected_names <- c(selected_names, grep("gain", all_names, ignore.case = TRUE, value = TRUE))
      }
    }
    
    # HII filtering
    if (process_hii) {
      selected_names <- c(selected_names, grep("hii|human.*impact", all_names, ignore.case = TRUE, value = TRUE))
    }
    
    # Remove duplicates and select layers
    selected_names <- unique(selected_names)
    
    if (length(selected_names) == 0) {
      warning("No layers matched the selection criteria")
      return(NULL)
    }
    
    # Subset the raster stack
    result <- full_result[[selected_names]]
    
    # Print summary
    cat("\n")
    cat(strrep("═", 70), "\n")
    cat("  PROCESSING COMPLETE\n")
    cat(strrep("═", 70), "\n")
    cat(sprintf("\nSelected %d out of %d available layers:\n", length(selected_names), length(all_names)))
    for (i in seq_along(selected_names)) {
      cat(sprintf("  %2d. %s\n", i, selected_names[i]))
    }
    cat("\n")
    
    return(result)
  }
  
  # Store environmental data
  env_data <- reactiveVal(NULL)
  
  # Select All button
  observeEvent(input$env_select_all, {
    # Main datasets
    updateCheckboxInput(session, "env_osm", value = TRUE)
    updateCheckboxInput(session, "env_chm", value = TRUE)
    updateCheckboxInput(session, "env_dem", value = TRUE)
    updateCheckboxInput(session, "env_gfc", value = TRUE)
    updateCheckboxInput(session, "env_hii", value = TRUE)
    
    # OSM sub-components
    updateCheckboxInput(session, "env_osm_building", value = TRUE)
    updateCheckboxInput(session, "env_osm_highway", value = TRUE)
    updateCheckboxInput(session, "env_osm_waterway", value = TRUE)
    
    # DEM sub-components
    updateCheckboxInput(session, "env_dem_elevation", value = TRUE)
    updateCheckboxInput(session, "env_dem_slope", value = TRUE)
    updateCheckboxInput(session, "env_dem_aspect", value = TRUE)
    updateCheckboxInput(session, "env_dem_tri", value = TRUE)
    updateCheckboxInput(session, "env_dem_tpi", value = TRUE)
    updateCheckboxInput(session, "env_dem_roughness", value = TRUE)
    
    # GFC sub-components
    updateCheckboxInput(session, "env_gfc_treecover", value = TRUE)
    updateCheckboxInput(session, "env_gfc_loss", value = TRUE)
    updateCheckboxInput(session, "env_gfc_gain", value = TRUE)
  })
  
  # Deselect All button
  observeEvent(input$env_deselect_all, {
    # Main datasets
    updateCheckboxInput(session, "env_osm", value = FALSE)
    updateCheckboxInput(session, "env_chm", value = FALSE)
    updateCheckboxInput(session, "env_dem", value = FALSE)
    updateCheckboxInput(session, "env_gfc", value = FALSE)
    updateCheckboxInput(session, "env_hii", value = FALSE)
    
    # OSM sub-components
    updateCheckboxInput(session, "env_osm_building", value = FALSE)
    updateCheckboxInput(session, "env_osm_highway", value = FALSE)
    updateCheckboxInput(session, "env_osm_waterway", value = FALSE)
    
    # DEM sub-components
    updateCheckboxInput(session, "env_dem_elevation", value = FALSE)
    updateCheckboxInput(session, "env_dem_slope", value = FALSE)
    updateCheckboxInput(session, "env_dem_aspect", value = FALSE)
    updateCheckboxInput(session, "env_dem_tri", value = FALSE)
    updateCheckboxInput(session, "env_dem_tpi", value = FALSE)
    updateCheckboxInput(session, "env_dem_roughness", value = FALSE)
    
    # GFC sub-components
    updateCheckboxInput(session, "env_gfc_treecover", value = FALSE)
    updateCheckboxInput(session, "env_gfc_loss", value = FALSE)
    updateCheckboxInput(session, "env_gfc_gain", value = FALSE)
  })
  
  # Display selection summary
  output$env_selection_summary <- renderPrint({
    selected <- c()
    
    if (input$env_osm) {
      osm_sub <- c()
      if (input$env_osm_building) osm_sub <- c(osm_sub, "Buildings")
      if (input$env_osm_highway) osm_sub <- c(osm_sub, "Highways")
      if (input$env_osm_waterway) osm_sub <- c(osm_sub, "Waterways")
      
      if (length(osm_sub) > 0) {
        selected <- c(selected, paste0("OSM: ", paste(osm_sub, collapse = ", ")))
      }
    }
    
    if (input$env_chm) {
      selected <- c(selected, "Canopy Height")
    }
    
    if (input$env_dem) {
      dem_sub <- c()
      if (input$env_dem_elevation) dem_sub <- c(dem_sub, "Elevation")
      if (input$env_dem_slope) dem_sub <- c(dem_sub, "Slope")
      if (input$env_dem_aspect) dem_sub <- c(dem_sub, "Aspect")
      if (input$env_dem_tri) dem_sub <- c(dem_sub, "TRI")
      if (input$env_dem_tpi) dem_sub <- c(dem_sub, "TPI")
      if (input$env_dem_roughness) dem_sub <- c(dem_sub, "Roughness")
      
      if (length(dem_sub) > 0) {
        selected <- c(selected, paste0("DEM: ", paste(dem_sub, collapse = ", ")))
      }
    }
    
    if (input$env_gfc) {
      gfc_sub <- c()
      if (input$env_gfc_treecover) gfc_sub <- c(gfc_sub, "TreeCover")
      if (input$env_gfc_loss) gfc_sub <- c(gfc_sub, "Loss")
      if (input$env_gfc_gain) gfc_sub <- c(gfc_sub, "Gain")
      
      if (length(gfc_sub) > 0) {
        selected <- c(selected, paste0("GFC: ", paste(gfc_sub, collapse = ", ")))
      }
    }
    
    if (input$env_hii) {
      selected <- c(selected, "Human Impact Index")
    }
    
    if (length(selected) == 0) {
      cat("No layers selected.\n")
      cat("Please select at least one dataset to process.")
    } else {
      cat("Selected layers:\n\n")
      for (i in seq_along(selected)) {
        cat(sprintf("%d. %s\n", i, selected[i]))
      }
      cat(sprintf("\nTotal: %d dataset(s)", length(selected)))
    }
  })
  
  # Process environmental data when button is clicked
  observeEvent(input$process_env_btn, {
    req(map())
    
    # Check if at least one dataset is selected
    if (!input$env_osm && !input$env_chm && !input$env_dem && 
        !input$env_gfc && !input$env_hii) {
      showNotification(
        "Please select at least one dataset to process!",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    # Collect selected options
    selected_osm <- NULL
    if (input$env_osm) {
      selected_osm <- list(
        building = input$env_osm_building,
        highway = input$env_osm_highway,
        waterway = input$env_osm_waterway
      )
    }
    
    selected_dem <- NULL
    if (input$env_dem) {
      selected_dem <- list(
        elevation = input$env_dem_elevation,
        slope = input$env_dem_slope,
        aspect = input$env_dem_aspect,
        tri = input$env_dem_tri,
        tpi = input$env_dem_tpi,
        roughness = input$env_dem_roughness
      )
    }
    
    selected_gfc <- NULL
    if (input$env_gfc) {
      selected_gfc <- list(
        treecover = input$env_gfc_treecover,
        loss = input$env_gfc_loss,
        gain = input$env_gfc_gain
      )
    }
    
    # Show modal with progress
    showModal(modalDialog(
      title = "Processing environmental data",
      "This may take several minutes. Please wait...",
      div(
        style = "text-align: center; padding: 20px;",
        tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 48px; color: #3c8dbc;")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Process in background
    result <- tryCatch(
      {
        process_environmental_data_selective(
          aoi = map(),
          mask = input$env_mask,
          osm_resolution = 512,
          # Main dataset toggles
          process_osm = input$env_osm,
          process_chm = input$env_chm,
          process_dem = input$env_dem,
          process_gfc = input$env_gfc,
          process_hii = input$env_hii,
          # Sub-selections
          osm_features = selected_osm,
          dem_metrics = selected_dem,
          gfc_layers = selected_gfc,
          gfc_target_year = 2024,
          skip_on_error = TRUE,
          show_progress = TRUE
        )
      },
      error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste("Error processing environmental data:", e$message),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }
    )
    
    if (!is.null(result)) {
      env_data(result)
      removeModal()
      showModal(modalDialog(
        title = "Success!",
        paste0("Successfully processed ", terra::nlyr(result), " environmental layer(s)!"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })
  
  # Render the environmental data plots
  output$env_data_plot <- renderPlot({
    req(env_data())
    
    # Plot all layers
    terra::plot(env_data(),
                main = names(env_data()),
                col = terrain.colors(100, rev = TRUE)
    )
  })
  
  # Download handler
  output$download_env_data <- downloadHandler(
    filename = function() {
      paste0("environmental_data_", Sys.Date(), ".tif")
    },
    content = function(file) {
      req(env_data())
      terra::writeRaster(env_data(), file, overwrite = TRUE)
    }
  )
  
  # Tell Shiny when environmental data is ready
  output$env_ready <- reactive({
    !is.null(env_data())
  })
  
  # Make it available to the browser
  outputOptions(output, "env_ready", suspendWhenHidden = FALSE)
  
  # Cell size conditional on the area of the polygon
  # Currently offers square grids or hexagons based on user choice
  shp_grid_id <- reactive({
    req(map(), input$grid_shape)
    
    current_crs <- sf::st_crs(map())$proj4string
    is_square <- as.logical(input$grid_shape)
    
    area_km2 <- units::set_units(sf::st_area(map()), "km^2") |> as.numeric()
    
    cell_size <- if (area_km2 < 1000) {
      if (identical(current_crs, "+proj=longlat +datum=WGS84 +no_defs")) {
        0.01
      } else {
        1000
      }
    } else {
      if (identical(current_crs, "+proj=longlat +datum=WGS84 +no_defs")) {
        0.02
      } else {
        2000
      }
    }
    
    # Adjust cell size for hexagons
    cell_size <- if (!is_square) {
      cell_size * 1.1547
    } else {
      cell_size
    }
    
    t_grid <- map() |>
      sf::st_make_grid(
        what     = "polygons",   # always "polygons" — shape is controlled by square arg
        cellsize = cell_size,
        square   = is_square,    # TRUE = rectangles, FALSE = hexagons
        crs      = current_crs
      ) |>
      sf::st_intersection(map())
    
    t_grid_id <- sf::st_sf(
      idcell = seq_along(t_grid),
      geom   = t_grid
    ) |>
      sf::st_cast("POLYGON")
    
    return(t_grid_id)
  })
  
  # Clip out grids outside the study area
  shp_grid_clip <- reactive({
    req(shp_grid_id(), map())
    
    # Check CRS
    if (identical(sf::st_crs(shp_grid_id()), sf::st_crs(map()))) {
      # CRS = same, no need for transformation
      sf::st_intersection(x = shp_grid_id(), y = map())
    } else {
      # CRS != same, transform
      shp_grid_transformed <- sf::st_transform(
        shp_grid_id(),
        crs = sf::st_crs(map())
      )
      map_transformed <- sf::st_transform(
        map(),
        crs = sf::st_crs(shp_grid_id())
      )
      
      sf::st_intersection(
        x = shp_grid_transformed,
        y = map_transformed
      )
    }
  })
  
  # Transform to WGS84
  grid_wgs <- reactive({
    req(shp_grid_clip())
    
    current_crs <- sf::st_crs(shp_grid_clip())$proj4string
    
    # Check if CRS is WGS84
    if (identical(current_crs, 4326)) {
      # CRS is already WGS84, no need for transformation
      return(shp_grid_clip())
    } else {
      # CRS is not WGS84, transform to WGS84
      return(sf::st_transform(shp_grid_clip(), crs = 4326))
    }
  })
  
  # Reactive for cleaned (outlier-removel) data
  cleaned_data <- reactiveVal(NULL)
  
  observeEvent(input$process_outlier_btn, {
    req(modified_data())
    
    data <- modified_data()
    
    # Check if X and Y columns exist
    if (!all(c("X", "Y") %in% colnames(data))) {
      showNotification("X and Y columns are missing from data!", type = "error")
      return(NULL)
    }
    
    # Step 1: Compute IQR
    Q1_X <- quantile(data$X, 0.25, na.rm = TRUE)
    Q3_X <- quantile(data$X, 0.75, na.rm = TRUE)
    IQR_X <- Q3_X - Q1_X
    
    Q1_Y <- quantile(data$Y, 0.25, na.rm = TRUE)
    Q3_Y <- quantile(data$Y, 0.75, na.rm = TRUE)
    IQR_Y <- Q3_Y - Q1_Y
    
    # Step 2: Identify outliers
    x_outliers <- (data$X < (Q1_X - 1.5 * IQR_X)) | (data$X > (Q3_X + 1.5 * IQR_X))
    y_outliers <- (data$Y < (Q1_Y - 1.5 * IQR_Y)) | (data$Y > (Q3_Y + 1.5 * IQR_Y))
    
    x_outliers[is.na(x_outliers)] <- FALSE
    y_outliers[is.na(y_outliers)] <- FALSE
    
    outlier_index <- x_outliers | y_outliers
    
    # Step 3: Remove outliers
    clean_df <- data[!outlier_index, ]
    
    if (nrow(clean_df) == 0) {
      showNotification("No data left after removing outliers!", type = "error")
      cleaned_data(NULL)
    } else {
      cleaned_data(clean_df)
    }
  })
  
  # Check coordinates and transform if necessary
  cleaned_data_utm <- reactive({
    req(cleaned_data(), map())
    
    # Check if X and Y columns exist in cleaned_data()
    if (all(c("X", "Y") %in% colnames(cleaned_data()))) {
      # Check if X & Y values are within the typical range for lat-long
      lat_long_check1 <- all(cleaned_data()$X >= -180 &
                               cleaned_data()$X <= 180) &&
        all(cleaned_data()$Y >= -90 &
              cleaned_data()$Y <= 90)
      
      # Convert to UTM using the projection of map()
      # if X & Y in lat-long format
      if (lat_long_check1 && all(sf::st_crs(map())$proj4string %in%
                                 c("+proj=longlat", "+proj=latlong"))) {
        return(sf::st_transform(
          cleaned_data(),
          crs = sf::st_crs(map())$proj4string
        ))
      } else {
        # Coordinates are not in lat-long format, return modified_data() as is
        return(cleaned_data())
      }
    } else {
      # X and Y columns are not present, return cleaned_data() as is
      return(cleaned_data())
    }
  })
  
  # Convert cleaned SMART data into spatial object
  clean_dat_sf <- reactive({
    req(cleaned_data_utm(), map())
    
    sf::st_as_sf(
      cleaned_data_utm(),
      coords = c("X", "Y"),
      crs = sf::st_crs(map())$proj4string
    ) |>
      dplyr::mutate(
        Detection = ifelse(obs == 0, "No", "Yes")
      )
  })
  
  # Cleaned Obs reactive
  clean_obs_sf <- reactive({
    req(clean_dat_sf())
    sf::st_make_valid(clean_dat_sf())
  })
  
  # Render 'clean_dat_sf' table
  output$clean_dat_sf_table <- renderDT({
    datatable(clean_dat_sf())
  })
  
  # Grid reactive
  grid_sf <- reactive({
    req(shp_grid_clip())
    sf::st_make_valid(shp_grid_clip())
  })
  
  # Overlay
  clean_overlay_sf <- reactive({
    req(grid_sf(), clean_obs_sf())
    sf::st_intersection(
      x = grid_sf(),
      y = clean_obs_sf()
    )
  })
  
  # Render table for "raw_overlay_sf"
  output$clean_overlay_sf_table <- renderDT({
    req(clean_overlay_sf())
    datatable(
      clean_overlay_sf(),
      caption = "clean_overlay_sf",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  clean_overlay <- reactive({
    req(clean_overlay_sf(), grid_wgs())
    sf::st_transform(
      x = clean_overlay_sf(),
      crs = sf::st_crs(grid_wgs())$proj4string
    )
  })
  
  # Render 'clean_overlay' table
  output$clean_overlay_table <- renderDT({
    datatable(clean_overlay())
  })
  
  # Data frame of raw_overlay
  clean_raw_obs_df <- reactive({
    req(clean_overlay())
    result <- clean_overlay() %>%
      group_by(idcell) %>%
      summarise(obs = sum(obs)) %>%
      st_drop_geometry()
    return(result)
  })
  
  # Render 'clean_raw_obs_df' table
  output$clean_raw_obs_df_table <- renderDT({
    datatable(clean_raw_obs_df())
  })
  
  # Create paths from SMART data points
  clean_id_mapping <- reactive({
    req(cleaned_data())
    data.frame(
      Patrol.ID = unique(cleaned_data()$Patrol.ID),
      new_id = paste0(
        "TID",
        seq_along(unique(cleaned_data()$Patrol.ID))
      )
    )
  })
  
  # merge the mapping with the original data
  clean_dat_id <- reactive({
    req(cleaned_data(), clean_id_mapping())
    cleaned_data() |>
      dplyr::left_join(
        x = clean_id_mapping(),
        by = "Patrol.ID"
      )
  })
  
  # Create a second sf object
  clean_dat_sf2 <- reactive({
    req(clean_dat_id(), map())
    sf::st_as_sf(
      x = clean_dat_id(),
      coords = c("X", "Y"),
      crs = sf::st_crs(map())$proj4string # projected crs
    )
  })
  
  # Convert points to polylines by transect id
  clean_patrol_paths <- reactive({
    req(clean_dat_sf2())
    p_paths <- clean_dat_sf2() |>
      dplyr::group_by(new_id) |>
      dplyr::summarise(
        geometry = sf::st_sfc(
          sf::st_cast(
            sf::st_make_valid(sf::st_union(geometry)), "LINESTRING"
          )
        )
      )
    
    # Check & handle invalid geometries if necessary
    p_paths <- p_paths[sf::st_is_valid(p_paths), ]
    
    return(p_paths)
  })
  
  # Reactive for raw vs cleaned choice
  final_data <- reactive({
    if (isTRUE(input$remove_outliers) && !is.null(cleaned_data())) {
      return(cleaned_data()) 
    } else {
      return(modified_data()) 
    }
  })
  
  # Data reactive to check coordinates & transform if necessary
  raw_modified_data_utm <- reactive({
    req(modified_data(), map())
    
    # Check if X & Y columns exist in modified_data()
    if (all(c("X", "Y") %in% colnames(modified_data()))) {
      # Check if X & Y values are within the typical range for lat-long
      lat_long_check <- all(modified_data()$X >= -180 &
                              modified_data()$X <= 180) &&
        all(modified_data()$Y >= -90 &
              modified_data()$Y <= 90)
      
      # Convert to UTM using the projection of map()
      # if X & Y in lat-long format
      if (lat_long_check && all(sf::st_crs(map())$proj4string %in%
                                c("+proj=longlat", "+proj=latlong"))) {
        return(sf::st_transform(
          modified_data(),
          crs = sf::st_crs(map())$proj4string
        ))
      } else {
        # Coordinates are not in lat-long format, return modified_data() as is
        return(modified_data())
      }
    } else {
      # X and Y columns are not present, return modified_data() as is
      return(modified_data())
    }
  })

  # To use final_data(), we can modify modified_data_utm() as
  modified_data_utm <- reactive({
    req(final_data(), map())
    data <- final_data()
    
    if (all(c("X", "Y") %in% colnames(data))) {
      lat_long_check <- all(data$X >= -180 & data$X <= 180) &&
        all(data$Y >= -90 & data$Y <= 90)
      
      if (lat_long_check && all(sf::st_crs(map())$proj4string %in%
                                c("+proj=longlat", "+proj=latlong"))) {
        return(sf::st_transform(
          data,
          crs = sf::st_crs(map())$proj4string
        ))
      } else {
        return(data)
      }
    } else {
      return(data)
    }
  })
  
  # Convert SMART data into spatial object
  dat_sf <- reactive({
    req(modified_data_utm(), map())
    
    sf::st_as_sf(
      modified_data_utm(),
      coords = c("X", "Y"),
      crs = sf::st_crs(map())$proj4string
    ) |>
      dplyr::mutate(
        Detection = ifelse(obs == 0, "No", "Yes") # obs column created above
      )
  })
  
  # Obs reactive
  obs_sf <- reactive({
    req(dat_sf())
    sf::st_make_valid(dat_sf())
  })
  
  # Render 'dat_sf' table
  output$dat_sf_table <- renderDT({
    datatable(dat_sf())
  })
  
  # Grid reactive
  grid_sf <- reactive({
    req(shp_grid_clip())
    sf::st_make_valid(shp_grid_clip())
  })
  
  # Overlay operation
  overlay_sf <- reactive({
    req(grid_sf(), obs_sf())
    sf::st_intersection(
      x = grid_sf(),
      y = obs_sf()
    )
  })
  
  # render table for "overlay_sf"
  output$overlay_sf_table <- renderDT({
    req(overlay_sf())
    datatable(
      overlay_sf(),
      caption = "overlay_sf",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top", 
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  overlay <- reactive({
    req(overlay_sf(), grid_wgs())
    sf::st_transform(
      x = overlay_sf(),
      crs = sf::st_crs(grid_wgs())$proj4string
    )
  })
  
  # Render 'overlay' table
  output$overlay_table <- renderDT({
    datatable(overlay())
  })
  
  # Data frame of overlay
  raw_obs_df <- reactive({
    req(overlay())
    result <- overlay() %>%
      group_by(idcell) %>%
      summarise(obs = sum(obs)) %>%
      st_drop_geometry()
    return(result)
  })
  
  # Render 'raw_obs_df' table
  output$raw_obs_df_table <- renderDT({
    datatable(raw_obs_df())
  })
  
  # Convert single column observation to detection history matrix
  result_dataframe_raw <- reactive({
    req(grid_sf(), overlay_sf())
    
    # Create a data frame to store results
    result_df <- as.data.frame(x = grid_sf())
    
    # Create a list to store information for each grid
    result_list <- vector(mode = "list", length = nrow(result_df))
    names(result_list) <- result_df$idcell
    
    # Loop through each grid and extract information of points within it
    for (i in seq_along(result_df$idcell)) {
      grid_id <- result_df$idcell[i]
      points_in_grid <- overlay_sf()[overlay_sf()$idcell == grid_id, ]
      # Check if points_in_grid is not empty
      if (nrow(points_in_grid) > 0) {
        result_list[[as.character(grid_id)]] <- points_in_grid$obs # obs
      } else {
        # If no points in the grid, fill with NA or any default value
        result_list[[as.character(grid_id)]] <- NA
      }
    }
    
    # Convert to data frame
    unique_grids <- unique(overlay_sf()$id)
    
    # Initialise a matrix to store the information
    result_matrix <- matrix(
      NA,
      nrow = length(unique_grids),
      ncol = max(sapply(result_list, length))
    )
    
    # Loop through each grid and fill in the matrix
    for (i in seq_along(unique_grids)) {
      grid_id <- unique_grids[i]
      points_in_grid <- overlay_sf()[overlay_sf()$idcell == grid_id, ]
      # Check if points_in_grid is not empty
      if (nrow(points_in_grid) > 0) {
        result_matrix[i, seq_along(result_list[[as.character(grid_id)]])] <-
          result_list[[as.character(grid_id)]]
      }
    }
    
    # Create a data frame from the matrix
    result_df_sparse_part1 <- as.data.frame(result_matrix)
    
    # Add grid ID column
    result_df_sparse_part1$grid_ID <- as.character(unique_grids)
    
    return(result_df_sparse_part1)
  })
  
  # Create final detection-nondetection matrix
  modified_df <- reactive({
    req(result_dataframe_raw())
    
    # Move grid ids to the first position
    res_df_spar <- result_dataframe_raw() |>
      dplyr::select(
        grid_ID, everything()
      )
    
    # Move NAs to the end within each row
    res_df_spar[, -1] <- t(
      apply(
        res_df_spar[, -1], 1,
        function(row) {
          c(row[!is.na(row)], rep(NA, sum(is.na(row))))
        }
      )
    )
    return(res_df_spar)
  })
  
  # Render 'modified_df' table
  output$modified_df_table <- renderDT({
    datatable(
      modified_df(),
      caption = "Detection History Matrix (original)",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single",
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  output$modified_df_structure <- renderPrint({
    str(modified_df())
  })
  
  output$modified_df_class <- renderPrint({
    class(modified_df())
  })
  

  # Collapse columns (occasions)
  
  # Function to collapse columns
  collapse_columns <- function(df, num_columns) {
    df <- df[, -1] # exclude 'grid_ID'
    
    num_rows <- nrow(df)
    num_new_columns <- ceiling(ncol(df) / num_columns)
    
    collapsed_df <- matrix(NA, nrow = num_rows, ncol = num_new_columns)
    
    for (i in seq_len(num_new_columns)) {
      start_col <- (i - 1) * num_columns + 1
      end_col <- min(i * num_columns, ncol(df))
      
      collapsed_column <- apply(df[, start_col:end_col], 1, function(row) {
        if (any(!is.na(row)) && sum(row, na.rm = TRUE) > 0) {
          return(1)
        } else if (all(is.na(row))) {
          return(NA)
        } else {
          return(0)
        }
      })
      
      collapsed_df[, i] <- collapsed_column
    }
    
    colnames(collapsed_df) <- paste("V", seq_len(num_new_columns), sep = "")
    return(data.frame(collapsed_df))
  }
  
  # Reactive object for collapsed data with grid_ID
  collapsed_data_with_grid_ID <- reactive({
    num_columns <- as.numeric(input$num_columns)
    
    if (num_columns == 0) {
      # Return original data (no collapse)
      return(modified_df())
    }
    
    collapsed_df <- collapse_columns(df = modified_df(), num_columns = num_columns)
    collapsed_df_with_grid_ID <- cbind(modified_df()$grid_ID, collapsed_df)
    colnames(collapsed_df_with_grid_ID)[1] <- "grid_ID"
    return(collapsed_df_with_grid_ID)
  })
  
  # Collapsed table output
  output$collapsed_table <- renderDT({
    req(collapsed_data_with_grid_ID())
    datatable(
      collapsed_data_with_grid_ID(),
      caption = "Detection History Matrix (collapsed occasions)",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", ## enable selection of a single row
        filter = "top", ## include column filters at the bottom
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Create paths from SMART data points
  id_mapping <- reactive({
    req(final_data())
    data.frame(
      Patrol.ID = unique(final_data()$Patrol.ID),
      new_id = paste0(
        "TID",
        seq_along(unique(final_data()$Patrol.ID))
      )
    )
  })
  
  dat_id <- reactive({
    req(final_data(), id_mapping())
    final_data() |>
      dplyr::left_join(
        x = id_mapping(),
        by = "Patrol.ID"
      )
  })
  
  # Create a second sf object
  dat_sf2 <- reactive({
    req(dat_id(), map())
    sf::st_as_sf(
      x = dat_id(),
      coords = c("X", "Y"),
      crs = sf::st_crs(map())$proj4string # projected crs
    )
  })
  
  # Convert points to polylines by transect id
  patrol_paths <- reactive({
    req(dat_sf2())
    p_paths <- dat_sf2() |>
      dplyr::group_by(new_id) |>
      dplyr::summarise(
        geometry = sf::st_sfc(
          sf::st_cast(
            sf::st_make_valid(sf::st_union(geometry)), "LINESTRING"
          )
        )
      )
    
    # Check & handle invalid geometries if necessary
    p_paths <- p_paths[sf::st_is_valid(p_paths), ]
    
    return(p_paths)
  })
  
  # Calculate length of transects within each grid (detection covariate)
  patrol_paths_tr <- reactive({
    req(patrol_paths(), map())
    sf::st_transform(
      x = patrol_paths(),
      crs = sf::st_crs(map())$proj4string
    )
  })
  
  # Join grid and polylines
  joined_sf <- reactive({
    req(patrol_paths_tr(), shp_grid_clip())
    sf::st_intersection(
      x = patrol_paths_tr(),
      y = shp_grid_clip()
    )
  })
  
  # Calculate length of patrol paths in each grid cell
  lengths <- reactive({
    req(joined_sf())
    sf::st_length(x = joined_sf())
  })
  
  # Summarise the lengths by idcell
  lengths_summary <- reactive({
    req(lengths(), joined_sf())
    aggregate(
      x = lengths(),
      by = list(idcell = joined_sf()$idcell),
      FUN = sum
    )
  })
  
  # Render 'lengths_summary' table
  output$lengths_summary_table <- renderDT({
    req(lengths_summary())
    datatable(
      lengths_summary(),
      caption = "Transect length",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  grid_sf2 <- reactive({
    req(lengths_summary(), shp_grid_clip())
    grid_sf3 <- merge(
      x = shp_grid_clip(),
      y = lengths_summary(),
      by = "idcell",
      all.x = TRUE
    )
    
    # Convert transect length to numeric class (new column)
    grid_sf3$transect_length <- as.numeric(x = grid_sf3$x)
    
    return(grid_sf3)
  })
  
  # Render 'grid_sf2' table
  output$grid_sf2_table <- renderDT({
    req(grid_sf2())
    datatable(
      grid_sf2(),
      caption = "grid_sf2",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Convert transect length per each grid cell into data frame
  grid_line_df <- reactive({
    req(grid_sf2())
    grid_sf2()[, c("idcell", "transect_length")] |>
      sf::st_drop_geometry()
  })
  
  # Render 'grid_line_df' table
  output$grid_line_df_table <- renderDT({
    req(grid_line_df())
    datatable(
      grid_line_df(),
      caption = "grid_line_df",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single",
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  overlay_date <- reactive({
    req(overlay_sf())
    
    overlay1 <- overlay_sf()
    
    overlay1$Date <- lubridate::dmy(
      overlay_sf()$Waypoint.Date,
      tz = "UTC"
    )
    overlay1$date_numeric <- as.numeric(overlay1$Date)
    
    return(overlay1)
  })
  
  # Render 'overlay_date' table
  output$overlay_date_table <- renderDT({
    req(overlay_date())
    datatable(
      overlay_date(),
      caption = "overlay_date",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Join overlay with numeric date with grid_sf2
  join_date_overlay <- reactive({
    req(overlay_date(), grid_sf2())
    
    sf::st_join(
      overlay_date(),
      grid_sf2(),
      by = "idcell"
    )
  })
  
  # Render 'join_date_overlay' table
  output$join_date_overlay_table <- renderDT({
    req(join_date_overlay())
    datatable(
      join_date_overlay(),
      caption = "join_date_overlay",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Summarise the counts of unique dates for each grid
  count_date <- reactive({
    req(join_date_overlay())
    
    result <- join_date_overlay() |>
      dplyr::group_by(idcell.x) |>
      dplyr::summarise(
        patrol_days = dplyr::n_distinct(date_numeric)
      )
    # create a column called idcell
    result$idcell <- result$idcell.x
    return(result)
  })
  
  # Render 'count_date' table
  output$count_date_table <- renderDT({
    req(count_date())
    datatable(
      count_date(),
      caption = "count_date",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top", 
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Merge summarised date nack to the shp_grid_clip
  grid_patrol_days_sf <- reactive({
    req(shp_grid_clip(), count_date())
    
    sf::st_join(
      x = shp_grid_clip(),
      y = count_date(),
      by = "idcell",
      all.x = TRUE
    )
  })
  
  # Render 'count_date' table
  output$grid_patrol_days_sf_table <- renderDT({
    req(grid_patrol_days_sf())
    datatable(
      grid_patrol_days_sf(),
      caption = "grid_patrol_days_sf",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Convert patrol days to data frame
  grid_patrol_days_df <- reactive({
    req(grid_patrol_days_sf())
    
    grid_patrol_days_sf() |>
      sf::st_drop_geometry()
  })
  
  # Render 'count_date' table
  output$grid_patrol_days_df_table <- renderDT({
    req(grid_patrol_days_df())
    datatable(
      grid_patrol_days_df(),
      caption = "grid_patrol_days_df",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Create centre coordinates of grids
  grid_c <- reactive({
    req(shp_grid_clip())
    shp_grid_clip() |>
      sf::st_centroid() |>
      sf::st_sf()
  })
  
  grid_centre <- reactive({
    req(grid_c(), shp_grid_clip())
    grid_c() |>
      sf::st_join(
        shp_grid_clip(),
        join = sf::st_within
      )
  })
  
  # Extract coordinates of grid centre & combine with original data
  data_df <- reactive({
    req(grid_centre())
    
    # Extract coordinates
    coordinate_gc <- sf::st_coordinates(grid_centre())
    
    # combine with original data
    data_df0 <- cbind(
      sf::st_drop_geometry(x = grid_centre()),
      coordinate_gc
    )
    return(data_df0)
  })
  
  # Data prep conditional on whether the occasions are collapsed or not
  # Define a reactive value to track whether columns are collapsed or not
  collapsed <- reactiveVal(FALSE)
  
  # When the user selects an option other than "No Collapse",
  # set collapsed to TRUE
  observeEvent(input$num_columns, {
    if (input$num_columns != 0) {
      collapsed(TRUE)
    }
  })
  
  # Convert grid_ID integer
  grids_df <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(collapsed_data_with_grid_ID())
      result_df_sparse <- collapsed_data_with_grid_ID()
    } else {
      req(modified_df())
      result_df_sparse <- modified_df()
    }
    result_df_sparse$grid_ID <- as.integer(
      x = result_df_sparse$grid_ID
    )
    return(result_df_sparse)
  })
  
  # subset data_df
  sub_data_df <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(data_df(), collapsed_data_with_grid_ID())
      data_df()[data_df()$idcell.x %in% collapsed_data_with_grid_ID()$grid_ID, ]
    } else {
      req(data_df(), modified_df())
      data_df()[data_df()$idcell.x %in% modified_df()$grid_ID, ]
    }
  })
  
  # subset grid_line_df
  sub_grid_line_df <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(grid_line_df(), collapsed_data_with_grid_ID())
      grid_line_df()[grid_line_df()$idcell %in% collapsed_data_with_grid_ID()$grid_ID, ]
    } else {
      req(grid_line_df(), modified_df())
      grid_line_df()[grid_line_df()$idcell %in% modified_df()$grid_ID, ]
    }
  })
  
  # Render 'sub_grid_line_df' table
  output$sub_grid_line_df_table <- renderDT({
    req(sub_grid_line_df())
    datatable(
      sub_grid_line_df(),
      caption = "sub_grid_line_df",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single",
        filter = "top", 
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # reorder the subset
  sub_data_df_ord <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(sub_data_df(), collapsed_data_with_grid_ID())
      sub_data_df()[order(match(
        sub_data_df()$idcell.x,
        collapsed_data_with_grid_ID()$grid_ID
      )), ]
    } else {
      req(sub_data_df(), modified_df())
      sub_data_df()[order(match(
        sub_data_df()$idcell.x,
        modified_df()$grid_ID
      )), ]
    }
  })
  
  # Render 'sub_data_df_ord' table
  output$sub_data_df_ord_table <- renderDT({
    req(sub_data_df_ord())
    datatable(
      sub_data_df_ord(),
      caption = "sub_data_df_ord",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top", 
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Patrol effort - transect length
  transect_length <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(sub_grid_line_df(), collapsed_data_with_grid_ID())
      
      # Order & subset the data, retain the second column
      second_column <- sub_grid_line_df()[order(match(
        sub_grid_line_df()$idcell,
        collapsed_data_with_grid_ID()$grid_ID
      )), ][, "transect_length", drop = FALSE]
    } else {
      req(sub_grid_line_df(), modified_df())
      
      # Order & subset the data, retain the second column
      second_column <- sub_grid_line_df()[order(match(
        sub_grid_line_df()$idcell,
        modified_df()$grid_ID
      )), ][, "transect_length", drop = FALSE]
    }
    
    return(data.frame(transect_length = second_column))
  })
  
  # Render 'transect_length' table
  output$transect_length_table <- renderDT({
    req(transect_length())
    datatable(
      transect_length(),
      caption = "transect_length",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # subset grid_line_df 
  sub_grid_patrol_days_df <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(grid_patrol_days_df(), collapsed_data_with_grid_ID())
      grid_patrol_days_df()[grid_patrol_days_df()$idcell.x %in% collapsed_data_with_grid_ID()$grid_ID, ]
    } else {
      req(grid_patrol_days_df(), modified_df())
      grid_patrol_days_df()[grid_patrol_days_df()$idcell.x %in% modified_df()$grid_ID, ]
    }
  })
  
  # reorder patrol days
  patrol_days <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(sub_grid_patrol_days_df(), collapsed_data_with_grid_ID())
      
      # Order & subset the data, retain the second column
      fifth_column <- sub_grid_patrol_days_df()[order(match(
        sub_grid_patrol_days_df()$idcell.x,
        collapsed_data_with_grid_ID()$grid_ID
      )), ][, "patrol_days", drop = FALSE]
    } else {
      req(sub_grid_patrol_days_df(), modified_df())
      
      # Order & subset the data, retain the second column
      fifth_column <- sub_grid_patrol_days_df()[order(match(
        sub_grid_patrol_days_df()$idcell.x,
        modified_df()$grid_ID
      )), ][, "patrol_days", drop = FALSE] # need to make this dynamic (!!!)
    }
    
    return(data.frame(patrol_days = fifth_column))
  })
  
  # Final det-nondet data
  final_df <- reactive({
    if (collapsed() && input$num_columns != 0) {
      req(collapsed_data_with_grid_ID(), sub_data_df_ord(), transect_length(), patrol_days())
      cbind(
        collapsed_data_with_grid_ID(),
        sub_data_df_ord()[, c("X", "Y")],
        transect_length(),
        patrol_days()
      )
    } else {
      req(modified_df(), sub_data_df_ord(), transect_length(), patrol_days())
      cbind(
        modified_df(),
        sub_data_df_ord()[, c("X", "Y")],
        transect_length(),
        patrol_days()
      )
    }
  })
  
  output$final_df_table <- renderDT({
    req(final_df())
    datatable(
      final_df(),
      caption = "Detection History with site information",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Map of cleaned patrol tracks & threat detections
  clean_dat1_df_wgs <- reactive({
    req(dat_sf2(), grid_wgs())
    clean_dat1_sf_wgs <- sf::st_transform(
      clean_dat_sf2(),
      crs = sf::st_crs(grid_wgs())
    )
    clean_dat1_df <- clean_dat1_sf_wgs |>
      sf::st_coordinates() |>
      as.data.frame()
    clean_dat1_df_c <- cbind(clean_dat_sf2()$new_id, clean_dat1_df)
    colnames(clean_dat1_df_c) <- c("ID", "X", "Y")
    
    return(clean_dat1_df_c)
  })
  
  clean_overlay_1_wgs <- reactive({
    req(clean_overlay())
    clean_overlay_1 <- clean_overlay()[clean_overlay()$obs == 1, ]
    clean_overlay_1_df <- clean_overlay_1 |>
      sf::st_coordinates() |>
      as.data.frame()
    
    return(clean_overlay_1_df)
  })
  
  # Raw data
  raw_id_mapping <- reactive({
    req(modified_data())
    data.frame(
      Patrol.ID = unique(modified_data()$Patrol.ID),
      new_id = paste0(
        "TID",
        seq_along(unique(modified_data()$Patrol.ID))
      )
    )
  })
  
  raw_dat_id <- reactive({
    req(modified_data(), raw_id_mapping())
    modified_data() |>
      dplyr::left_join(
        x = raw_id_mapping(),
        by = "Patrol.ID"
      )
  })
  
  # Create a second sf object
  raw_dat_sf2 <- reactive({
    req(raw_dat_id(), map())
    sf::st_as_sf(
      x = raw_dat_id(),
      coords = c("X", "Y"),
      crs = sf::st_crs(map())$proj4string # projected crs
    )
  })
  
  raw_dat1_df_wgs <- reactive({
    req(raw_dat_sf2(), grid_wgs())
    raw_dat1_sf_wgs <- sf::st_transform(
      raw_dat_sf2(),
      crs = sf::st_crs(grid_wgs())
    )
    raw_dat1_df <- raw_dat1_sf_wgs |>
      sf::st_coordinates() |>
      as.data.frame()
    raw_dat1_df_c <- cbind(raw_dat_sf2()$new_id, raw_dat1_df)
    colnames(raw_dat1_df_c) <- c("ID", "X", "Y")
    
    return(raw_dat1_df_c)
  })
  
  raw_dat_sf <- reactive({
    req(raw_modified_data_utm(), map())
    
    sf::st_as_sf(
      raw_modified_data_utm(),
      coords = c("X", "Y"),
      crs = sf::st_crs(map())$proj4string
    ) |>
      dplyr::mutate(
        Detection = ifelse(obs == 0, "No", "Yes") # obs column created above
      )
  })
  
  raw_obs_sf <- reactive({
    req(raw_dat_sf())
    sf::st_make_valid(raw_dat_sf())
  })
  
  raw_overlay_sf <- reactive({
    req(grid_sf(), raw_obs_sf())
    sf::st_intersection(
      x = grid_sf(),
      y = raw_obs_sf()
    )
  })
  
  raw_overlay <- reactive({
    req(raw_overlay_sf(), grid_wgs())
    sf::st_transform(
      x = raw_overlay_sf(),
      crs = sf::st_crs(grid_wgs())$proj4string
    )
  })
  
  raw_overlay_1_wgs <- reactive({
    req(raw_overlay())
    raw_overlay_1 <- raw_overlay()[raw_overlay()$obs == 1, ]
    raw_overlay_1_df <- raw_overlay_1 |>
      sf::st_coordinates() |>
      as.data.frame()
    
    return(raw_overlay_1_df)
  })
  

  # Plot raw cleaned & uncleaned maps together
  output$track_det_threat_clean_unclean <- renderCombineWidgets({
    req(
      grid_wgs(), clean_dat1_df_wgs(), clean_overlay_1_wgs(), clean_raw_obs_df(),
      raw_dat1_df_wgs(), raw_overlay_1_wgs(), raw_dat_sf()
    )
    
    base_grid0 <- grid_wgs()
    clean_patrol_tracks <- clean_dat1_df_wgs()
    clean_threat_det <- clean_overlay_1_wgs()
    
    pt_map0 <- leaflet(base_grid0) |>
      addTiles() |>
      addPolygons(color = "grey", fillOpacity = 0, weight = 1)
    
    n0 <- length(unique(clean_patrol_tracks$ID))
    npal0 <- randomcoloR::distinctColorPalette(n0)
    
    clean_patrol_tracks_grp <- clean_patrol_tracks |>
      dplyr::mutate(colour = npal0[match(ID, unique(ID))])
    
    for (ID in levels(factor(clean_patrol_tracks_grp$ID))) {
      pt_map0 <- addPolylines(
        pt_map0,
        lng = ~X,
        lat = ~Y,
        data = clean_patrol_tracks_grp[clean_patrol_tracks_grp$ID == ID, ],
        color = ~colour,
        weight = 1
      )
    }
    
    pt_map0 <- pt_map0 |>
      addCircleMarkers(
        data = clean_threat_det,
        lng = ~X,
        lat = ~Y,
        radius = 3,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.5
      ) |>
      addControl(
        html = '<div id="map_title">Patrol tracks with threat (cleaned)</div>',
        position = "topright"
      )
    
    # Unclean map
    base_grid00 <- grid_wgs()
    patrol_tracks00 <- raw_dat1_df_wgs()
    threat_det00 <- raw_overlay_1_wgs()
    
    pt_map00 <- leaflet(base_grid00) |>
      addTiles() |>
      addPolygons(color = "grey", fillOpacity = 0, weight = 1)
    
    n00 <- length(unique(patrol_tracks00$ID))
    npal00 <- randomcoloR::distinctColorPalette(n00)
    
    patrol_tracks_grp00 <- patrol_tracks00 |>
      dplyr::mutate(colour = npal00[match(ID, unique(ID))])
    
    for (ID in levels(factor(patrol_tracks_grp00$ID))) {
      pt_map00 <- addPolylines(
        pt_map00,
        lng = ~X,
        lat = ~Y,
        data = patrol_tracks_grp00[patrol_tracks_grp00$ID == ID, ],
        color = ~colour,
        weight = 1
      )
    }
    
    pt_map00 <- pt_map00 |>
      addCircleMarkers(
        data = threat_det00,
        lng = ~X,
        lat = ~Y,
        radius = 3,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.5
      ) |>
      addControl(
        html = '<div id="map_title">Patrol tracks with threat (raw)</div>',
        position = "topright"
      )
    
    combineWidgets(pt_map0, pt_map00, ncol = 2)
  })
  
  # Map of patrol tracks & threat detections
  dat1_df_wgs <- reactive({
    req(dat_sf2(), grid_wgs())
    dat1_sf_wgs <- sf::st_transform(
      dat_sf2(),
      crs = sf::st_crs(grid_wgs())
    )
    dat1_df <- dat1_sf_wgs |>
      sf::st_coordinates() |>
      as.data.frame()
    dat1_df_c <- cbind(dat_sf2()$new_id, dat1_df)
    colnames(dat1_df_c) <- c("ID", "X", "Y")
    
    return(dat1_df_c)
  })
  
  overlay_1_wgs <- reactive({
    req(overlay())
    overlay_1 <- overlay()[overlay()$obs == 1, ]
    overlay_1_df <- overlay_1 |>
      sf::st_coordinates() |>
      as.data.frame()
    
    return(overlay_1_df)
  })
  
  # All maps
  output$track_det_threat_count <- renderCombineWidgets({
    req(grid_wgs(), dat1_df_wgs(), overlay_1_wgs(), raw_obs_df())
    
    base_grid <- grid_wgs()
    patrol_tracks <- dat1_df_wgs()
    threat_det <- overlay_1_wgs()
    
    pt_map <- leaflet(base_grid) |>
      addTiles() |>
      addPolygons(color = "grey", fillOpacity = 0, weight = 1)
    
    n <- length(unique(patrol_tracks$ID))
    npal <- randomcoloR::distinctColorPalette(n)
    
    patrol_tracks_grp <- patrol_tracks |>
      dplyr::mutate(colour = npal[match(ID, unique(ID))])
    
    for (ID in levels(factor(patrol_tracks_grp$ID))) {
      pt_map <- addPolylines(
        pt_map,
        lng = ~X,
        lat = ~Y,
        data = patrol_tracks_grp[patrol_tracks_grp$ID == ID, ],
        color = ~colour,
        weight = 1
      )
    }
    
    pt_map <- pt_map |>
      addPolygons(
        data = base_grid,
        fillColor = "transparent",
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = paste("Grid", base_grid$idcell),
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addCircleMarkers(
        data = threat_det,
        lng = ~X,
        lat = ~Y,
        radius = 3,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.5
      ) |>
      addControl(
        html = '<div id="map_title">Patrol tracks with threat</div>',
        position = "topright"
      )
    
    obs_data <- raw_obs_df()
    
    # assign observation data to grid
    raw_obs <- sp::merge(
      base_grid,
      obs_data,
      by.x = "idcell",
      by.y = "idcell"
    )
    
    pal1 <- colorNumeric(
      palette = "viridis",
      domain = raw_obs$obs
    )
    
    # Labels
    map_labels1 <- paste(
      "Threat count in grid",
      raw_obs$idcell,
      "=",
      round(raw_obs$obs, 0)
    )
    
    map1 <- leaflet() |>
      addTiles() |>
      addPolygons(
        data = base_grid,
        fillColor = "transparent",
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = paste("Grid", base_grid$idcell),
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addPolygons(
        data = raw_obs$geom,
        fillColor = pal1(raw_obs$obs),
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = map_labels1,
        labelOptions = labelOptions(textsize = "12px")
      ) |>
      addLegend(
        pal = pal1,
        values = raw_obs$obs,
        position = "bottomleft",
        title = "Count"
      ) |>
      addControl(
        html = '<div id="map_title">Threat count</div>',
        position = "topright"
      )
    
    combineWidgets(pt_map, map1, ncol = 2)
  })
  
  output$transect_length_patrol_days <- renderCombineWidgets({
    req(grid_line_df(), grid_wgs(), grid_patrol_days_df())
    
    tl_df <- grid_line_df()
    base_grid <- grid_wgs()
    
    # Assign length per grid to original grids
    tl_g <- sp::merge(
      base_grid,
      tl_df,
      by.x = "idcell",
      by.y = "idcell"
    )
    
    pal2 <- colorNumeric(
      palette = "viridis",
      na.color = "#FF000000",
      domain = tl_g$transect_length
    )
    
    map_labels2 <- paste(
      "Transect length (m)",
      tl_g$idcell,
      "=",
      round(tl_g$transect_length, 0)
    )
    
    map2 <- leaflet() |>
      addTiles() |>
      addPolygons(
        data = base_grid,
        fillColor = "transparent",
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = paste("Grid", base_grid$idcell),
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addPolygons(
        data = tl_g$geom,
        fillColor = pal2(tl_g$transect_length),
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = map_labels2,
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal2,
        values = tl_g$transect_length,
        position = "bottomleft",
        title = "Length (m)"
      ) |>
      addControl(
        html = '<div id="map_title">Transect length</div>',
        position = "topright"
      )
    
    pat_days <- grid_patrol_days_df()
    
    days <- sp::merge(
      base_grid,
      pat_days,
      by.x = "idcell",
      by.y = "idcell.x"
    )
    
    pal_pdays <- colorNumeric(
      palette = "viridis",
      na.color = "#ff000000",
      domain = days$patrol_days
    )
    
    map_labels_pdays <- paste(
      "Patrol days in grid",
      days$idcell,
      "=",
      days$patrol_days
    )
    
    map_pdays <- leaflet() |>
      addTiles() |>
      addPolygons(
        data = base_grid,
        fillColor = "transparent",
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = paste("Grid", base_grid$idcell),
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addPolygons(
        data = days$geom,
        fillColor = pal_pdays(days$patrol_days),
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = map_labels_pdays,
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal_pdays,
        values = days$patrol_days,
        position = "bottomleft",
        title = "Days"
      ) |>
      addControl(
        html = '<div id="map_title">Patrol days</div>',
        position = "topright"
      )
    
    combineWidgets(map2, map_pdays, ncol = 2)
  })
  
  # Raster input option
  # Allow users to upload raster file
  raster_data <- reactiveValues(rasters = list(), stacked_raster = NULL)
  
  # Store multiple models for comparison
  model_storage <- reactiveValues(
    models = list(),
    model_names = character(),
    counter = 0
  )
  
  # Initialise model list display
  output$model_list_display <- renderPrint({
    if (length(model_storage$model_names) == 0) {
      cat("No models in storage.\n")
    } else {
      cat("Models in storage:\n")
      cat(paste(seq_along(model_storage$model_names),
                model_storage$model_names,
                sep = ". ",
                collapse = "\n"
      ))
    }
  })
  
  # Raster upload
  # Track raster names & their indices
  raster_names <- reactiveVal(character(0))
  
  # Counter to track file uploads
  upload_counter <- reactiveVal(0)
  
  # Upload & process rasters
  observeEvent(input$raster_files, {
    req(input$raster_files)
    
    upload_counter(upload_counter() + 1)
    
    tryCatch({
      reference_raster <- NULL
      if (length(raster_data$rasters) > 0) {
        reference_raster <- raster_data$rasters[[1]]
      }
      
      # Process each newly uploaded file
      new_rasters <- list()
      for (i in seq_len(nrow(input$raster_files))) {
        file <- input$raster_files[i, ]
        raster <- terra::rast(file$datapath)
        
        # Check if raster has CRS, if not, assign WGS 84
        if (is.na(terra::crs(raster)) || is.null(terra::crs(raster))) {
          terra::crs(raster) <- "+proj=longlat +datum=WGS84"
        }
        
        # First raster uploaded, use it as reference
        if (is.null(reference_raster) && i == 1) {
          reference_raster <- raster
        } else {
          # Reproject to match the CRS of the reference raster
          if (!is.null(reference_raster)) {
            raster <- terra::project(raster, terra::crs(reference_raster))
          }
          
          # Extent matching
          if (!is.null(reference_raster)) {
            raster <- terra::resample(raster, reference_raster)
          }
        }
        
        raster_name <- tools::file_path_sans_ext(basename(file$name))
        names(raster) <- raster_name
        
        new_rasters[[i]] <- raster
      }
      
      # Add new rasters to existing ones
      raster_data$rasters <- c(raster_data$rasters, new_rasters)
      
      # Update raster names
      new_names <- sapply(new_rasters, function(r) names(r)[1])
      raster_names(c(raster_names(), new_names))
      
      # Force update of stacked_raster
      if (length(raster_data$rasters) > 0) {
        raster_data$stacked_raster <- do.call(c, raster_data$rasters)
      }
      
      showNotification(
        paste("Successfully uploaded", length(new_rasters), "raster(s)!"),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error uploading rasters:", e$message),
        type = "error",
        duration = 5
      )
    })
  }, ignoreInit = TRUE)
  
  # Render the list of uploaded rasters with delete buttons
  output$uploaded_rasters_list <- renderUI({
  
    upload_counter()
    
    if (length(raster_data$rasters) == 0) {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px; color: #6c757d;",
        icon("info-circle"),
        " No rasters uploaded yet. Use the browse button above to select raster files."
      )
    } else {
      raster_items <- lapply(seq_along(raster_data$rasters), function(i) {
        raster_name <- names(raster_data$rasters[[i]])[1]
        
        div(
          style = "display: flex; align-items: center; justify-content: space-between; 
                   padding: 8px; margin: 4px 0; background-color: #f0f0f0; 
                   border-radius: 4px; border-left: 4px solid #3c8dbc;",
          div(
            style = "flex-grow: 1;",
            icon("layer-group"),
            " ",
            strong(raster_name)
          ),
          actionButton(
            paste0("delete_raster_", i),
            label = NULL,
            icon = icon("trash"),
            class = "btn-danger btn-sm",
            style = "padding: 4px 8px;"
          )
        )
      })
      
      div(
        style = "margin-top: 10px;",
        tags$b(paste("Uploaded Rasters:", length(raster_data$rasters))),
        br(), br(),
        do.call(tagList, raster_items)
      )
    }
  })
  
  # Observe delete button
  lapply(1:100, function(i) {
    observeEvent(input[[paste0("delete_raster_", i)]], {
      # Only process if this index is valid
      if (i <= length(raster_data$rasters) && length(raster_data$rasters) > 0) {
        deleted_name <- names(raster_data$rasters[[i]])[1]
        
        # Create new list without the i-th element
        new_rasters <- raster_data$rasters[-i]
        raster_data$rasters <- new_rasters
        
        # Update names
        current_names <- raster_names()
        if (i <= length(current_names)) {
          raster_names(current_names[-i])
        }
        
        # Force update of stacked_raster
        if (length(raster_data$rasters) > 0) {
          raster_data$stacked_raster <- do.call(c, raster_data$rasters)
        } else {
          raster_data$stacked_raster <- NULL
        }
        
        # Increment upload counter
        upload_counter(upload_counter() + 1)
        
        showNotification(
          paste("Deleted raster:", deleted_name),
          type = "warning",
          duration = 3
        )
      }
    }, ignoreInit = TRUE)
  })
  
  # Create reactive object to stack all rasters
  observe({
    upload_counter()
    
    # Update stacked raster whenever rasters change
    if (length(raster_data$rasters) > 0) {
      raster_data$stacked_raster <- do.call(c, raster_data$rasters)
    } else {
      # When all rasters deleted, set to NULL
      raster_data$stacked_raster <- NULL
    }
  })
  
  observeEvent(input$harmonise_btn, {
    # Require both datasets
    req(env_data())
    req(raster_data$rasters)
    
    if (length(raster_data$rasters) == 0) {
      showModal(modalDialog(
        title = "No Uploaded Rasters",
        "Please upload raster data before harmonising.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    showModal(modalDialog(
      title = "Harmonising Spatial Data",
      "Aligning CRS, extent, and resolution. Please wait...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Reference environmental raster
    env <- env_data()
    
    # Build stack
    uploaded_stack <- do.call(c, raster_data$rasters)
    
    # Reproject to match CRS
    if (!terra::same.crs(uploaded_stack, env)) {
      uploaded_stack <- terra::project(uploaded_stack, terra::crs(env))
    }
    
    # match resolution & extent
    uploaded_stack <- terra::resample(uploaded_stack, env[[1]], method = "bilinear")
    
    # Crop & mask to template
    uploaded_stack <- terra::crop(uploaded_stack, env[[1]])
    uploaded_stack <- terra::mask(uploaded_stack, env[[1]])
    
    # Combine stacks
    combined_stack <- c(env, uploaded_stack)
    names(combined_stack) <- make.unique(names(combined_stack))
    
    # Store final harmonised stack
    raster_data$stacked_raster <- combined_stack
    
    removeModal()
    
    showModal(modalDialog(
      title = "Success",
      paste0("Final stack contains ", terra::nlyr(combined_stack), " layers."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Plot of stacked raster
  output$stacked_plot <- renderPlot({
    if (!is.null(raster_data$stacked_raster)) {
      plot(raster_data$stacked_raster)
    }
  })
  
  # Convert xy coordinates to spatial object
  xy <- reactive({
    req(final_df(), map())
    data.frame(
      lon = final_df()$X,
      lat = final_df()$Y
    ) |>
      terra::vect(
        crs = sf::st_crs(x = map())$proj4string
      )
  })
  
  rast_val <- reactive({
    # determine if we have ANY raster data
    has_env_data <- !is.null(env_data())
    has_uploaded_rasters <- length(raster_data$rasters) > 0
    has_harmonized <- !is.null(raster_data$stacked_raster)
    
    # If no raster data at all, return NULL early
    if (!has_env_data && !has_uploaded_rasters && !has_harmonized) {
      cat("No raster data available. Using detection history only.\n")
      return(NULL)
    }
    
    # Only require xy() if we actually have raster data to extract
    req(xy())
    
    # Determine which raster to use
    raster_to_use <- NULL
    
    # Harmonised stack (if exists)
    if (has_harmonized) {
      raster_to_use <- raster_data$stacked_raster
      cat("Using harmonised stack (", terra::nlyr(raster_to_use), " layers)\n")
    }
    # Environmental data only (if exists & no uploaded rasters)
    else if (has_env_data && !has_uploaded_rasters) {
      raster_to_use <- env_data()
      cat("Using environmental data only (", terra::nlyr(raster_to_use), " layers)\n")
    }
    # Uploaded rasters only (if exists)
    else if (has_uploaded_rasters) {
      raster_to_use <- do.call(c, raster_data$rasters)
      cat("Using uploaded rasters only (", terra::nlyr(raster_to_use), " layers)\n")
    }
    
    # Double-check
    if (is.null(raster_to_use)) {
      return(NULL)
    }
    
    # Extract raster values
    extracted_values <- terra::extract(
      x = raster_to_use,
      y = xy(),
      method = "bilinear"
    )
    
    extracted_values <- extracted_values[, -1, drop = FALSE]
    
    # Replace NAs with raster mean
    r_mean <- terra::global(raster_to_use, mean, na.rm = TRUE)
    extracted_values[is.na(extracted_values)] <- r_mean[1, 1]
    
    return(extracted_values)
  })
  
  
  # Finally bind with all other info
  comb_fin <- reactive({
    req(final_df())
    
    raster_values <- rast_val()
    
    # If raster data exists, combine it with detection history
    if (!is.null(raster_values) && ncol(raster_values) > 0) {
      cat("Combining detection history with", ncol(raster_values), "covariate layer(s)\n")
      combined_data <- cbind(final_df(), raster_values)
      cat("Total columns:", ncol(combined_data), "\n")
      return(combined_data)
    }
    # Otherwise, just use detection history
    else {
      cat("No covariate data available. Using detection history only.\n")
      cat("You can still create occupancy models with detection covariates (e.g., patrol effort).\n")
      return(final_df())
    }
  })
  
  # Final detection history matrix with all the relevant data
  output$occupancy_data <- renderDT({
    req(comb_fin())
    datatable(
      comb_fin(),
      caption = "Input data for occupancy model",
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Triggered when the user clicks the download button
  output$download_btn1 <- downloadHandler(
    filename = "SMART_input_data_for_occupancy_model.csv",
    content = function(file) {
      write.csv(comb_fin(), row.names = FALSE, file)
    }
  )
  
  # Modelling
  
  # Reactive object for 'detn'
  detn <- reactive({
    req(input$col_range_detn)
    comb_fin()[, input$col_range_detn, drop = FALSE]
  })
  
  # Render 'detn' table
  output$detn_table <- renderDT({
    datatable(detn())
  })
  
  # Reactive object for 'site_cov'
  site_cov <- reactive({
    # Return NULL if no site covariates selected
    if (is.null(input$col_range_site_cov) || length(input$col_range_site_cov) == 0) {
      return(NULL)
    }
    comb_fin()[, input$col_range_site_cov, drop = FALSE]
  })
  
  # Render 'site_cov' table
  output$site_cov_table <- renderDT({
    datatable(site_cov())
  })
  
  # Standardise each column of 'site_cov'
  site_cov_standardized <- reactive({
    site_cov_data <- site_cov()
    # Return NULL if no site covariates
    if (is.null(site_cov_data) || ncol(site_cov_data) == 0) {
      return(NULL)
    }
    standardized_data <- lapply(site_cov_data, function(col) {
      (col - mean(col, na.rm = TRUE)) / sd(col, na.rm = TRUE)
    })
    as.data.frame(standardized_data)
  })
  
  # Render 'site_cov_standardized' table
  output$site_cov_standardized_table <- renderDT({
    datatable(
      site_cov_standardized(),
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Reactive object for 'det_cov'
  det_cov <- reactive({
    # Return NULL if no detection covariates selected
    if (is.null(input$col_range_det_cov) || length(input$col_range_det_cov) == 0) {
      return(NULL)
    }
    comb_fin()[, input$col_range_det_cov, drop = FALSE]
  })
  
  # Render 'det_cov' table
  output$det_cov_table <- renderDT({
    datatable(
      det_cov(),
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top", 
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Standardise each column of 'det_cov'
  det_cov_standardized <- reactive({
    det_cov_data <- det_cov()
    # Return NULL if no detection covariates
    if (is.null(det_cov_data) || ncol(det_cov_data) == 0) {
      return(NULL)
    }
    standardized_data1 <- lapply(det_cov_data, function(col) {
      (col - mean(col, na.rm = TRUE)) / sd(col, na.rm = TRUE)
    })
    as.data.frame(standardized_data1)
  })
  
  # Render 'det_cov_standardized' table
  output$det_cov_standardized_table <- renderDT({
    datatable(
      det_cov_standardized(),
      options = list(
        initComplete = JS(
          "function(settings, json)
          {", "$(this.api().table().header()).css({'background-color': 'teal',
          'color': 'black'});", "}"
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 5,
        extensions = "Buttons",
        selection = "single", 
        filter = "top",
        columnDefs = list(
          list(targets = 2, width = "100px")
        )
      )
    )
  })
  
  # Dynamically generate column choices
  observe({
    req(comb_fin())
    col_names <- names(comb_fin())
    
    # Automatically identify detection columns
    det_cols <- col_names[grepl("^V[0-9]+$", col_names)]
    
    # Identify non-detection columns
    non_det_cols <- col_names[!col_names %in% c("grid_ID", "X", "Y", det_cols)]
    
    # Update detection columns
    updateSelectizeInput(
      session,
      "col_range_detn",
      choices = det_cols,
      selected = det_cols,
      server = TRUE
    )
    
    # Update site & detection covariate choices
    updateSelectizeInput(
      session,
      "col_range_site_cov",
      choices = non_det_cols,
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      "col_range_det_cov",
      choices = non_det_cols,
      server = TRUE
    )
    
    # Update choices for global model builder
    updateSelectizeInput(
      session,
      "global_psi_covs",
      choices = non_det_cols,
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      "global_p_covs",
      choices = non_det_cols,
      server = TRUE
    )
    
    # Update choices for custom model builder
    updateSelectizeInput(
      session,
      "first_part",
      choices = c("1", non_det_cols),
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      "second_part",
      choices = c("1", non_det_cols),
      server = TRUE
    )
  })
  
  # Create unmarkedFrameOccu object
  umf <- reactive({
    req(detn())
    
    # Check if we have valid data
    if (ncol(detn()) == 0) {
      showNotification("Please select detection columns", type = "error")
      return(NULL)
    }
    
    # Check if we have covariate data
    has_site_cov <- !is.null(site_cov_standardized()) && ncol(site_cov_standardized()) > 0
    has_det_cov <- !is.null(det_cov_standardized()) && ncol(det_cov_standardized()) > 0
    
    # Build siteCovs
    if (has_site_cov && has_det_cov) {
      all_covs <- cbind(site_cov_standardized(), det_cov_standardized())
    } else if (has_site_cov) {
      all_covs <- site_cov_standardized()
    } else if (has_det_cov) {
      all_covs <- det_cov_standardized()
    } else {
      all_covs <- NULL
    }
    
    # Only create umf if we have data
    if (nrow(detn()) > 0) {
      tryCatch(
        {
          unmarkedFrameOccu(
            y = detn(),
            siteCovs = all_covs
          )
        },
        error = function(e) {
          showNotification(paste("UMF creation error:", e$message), type = "error")
          return(NULL)
        }
      )
    } else {
      NULL
    }
  })
  
  # Calculate naive occupancy
  naive_occ <- reactive({
    detn_data <- detn()
    round(
      sum(ifelse(rowSums(detn_data, na.rm = TRUE) > 0, 1, 0)) / nrow(detn_data),
      2
    )
  })
  
  # Display naive occuapancy value
  output$naive_occ_text <- renderUI({
    withMathJax(
      helpText("\\(\\textit{Na\u00EFve}\\ \\psi\\) =", naive_occ())
    )
  })
  
  # Create correlation plot
  output$correlation_plot1 <- renderPlotly({
    cor_plot <- ggcorrplot::ggcorrplot(
      corr = round(cor(site_cov_standardized(), use = "complete.obs"), 2),
      hc.order = TRUE,
      type = "lower",
      lab = TRUE,
      lab_size = 3
    ) +
      ggplot2::ggtitle("Pearson's Correlation") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::coord_fixed()
    
    ggplotly(cor_plot) %>%
      plotly::layout(
        margin = list(b = 150, l = 150),
        xaxis = list(tickangle = 45, tickfont = list(size = 10)),
        yaxis = list(tickfont = list(size = 10))
      )
  })
  
  # Update tables & plots on button click
  observeEvent(input$update_btn, {
    # Check if comb_fin() data exists
    if (is.null(comb_fin())) {
      showNotification(
        "Data not ready. Please ensure you have: (1) Uploaded CSV and shapefile, (2) Processed observations. Raster data is optional.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    # Check if detection columns are selected
    if (is.null(input$col_range_detn) || length(input$col_range_detn) == 0) {
      showNotification(
        "Detection columns not found. Please ensure data is processed correctly.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    # Render detection table
    output$detn_table <- renderDT({
      datatable(detn()[, input$col_range_detn, drop = FALSE])
    })
    
    # Display umf structure
    output$umf_structure <- renderPrint({
      umf_obj <- umf()
      if (!is.null(umf_obj)) {
        str(umf_obj)
      } else {
        cat("UMF object could not be created. Please check your data selections.")
      }
    })
    
    # Show success notification if UMF was created
    if (!is.null(umf())) {
      # Count how many covariates are available
      n_cols <- ncol(comb_fin())
      n_detn <- length(input$col_range_detn)
      n_covariates <- n_cols - n_detn - 4
      
      success_msg <- paste0(
        "Data structure checked successfully! ",
        "UMF created with ", n_detn, " detection occasions",
        if (n_covariates > 0) paste0(" and ", n_covariates, " site covariates") else ""
      )
      
      showNotification(
        success_msg,
        type = "message",
        duration = 3
      )
    }
  })
  
  # Model run
 
  # Create reactive values for column names from comb_fin()
  detn_col_names <- reactiveVal(NULL)
  site_cov_col_names <- reactiveVal(NULL)
  det_cov_col_names <- reactiveVal(NULL)
  
  # Create reactiveValues to store formula, model, & GoF results
  reactive_values <- reactiveValues(
    formula = NULL,
    model = NULL,
    mod_gof = NULL
  )
  
  # Reactive formula for the model
  observe({
    if (!is.null(input$first_part) && !is.null(input$second_part)) {
      formula_str <- paste(
        "~",
        paste(input$first_part, collapse = "+"),
        "~",
        paste(input$second_part, collapse = "+")
      )
      reactive_values$formula <- as.formula(formula_str)
    }
  })
  
  # Create a reactive expression for the model
  reactive_model <- reactive({
    if (!is.null(reactive_values$formula)) {
      occu(
        reactive_values$formula,
        data = umf()
      )
    }
  })
  
  # To access psi prediction from within an observeEvent object
  psi_all_val <- reactive({
    if (!is.null(reactive_model())) {
      predict(reactive_model(), type = "state")
    } else {
      NULL
    }
  })
  
  # Reactive storage for models & GoF results
  model_storage <- reactiveValues(
    counter = 0,
    models = list(),
    model_names = character(),
    global_model = NULL,
    global_umf = NULL,
    global_formula = NULL,
    global_gof = NULL
  )
  
  # Build Global Model
  observeEvent(input$build_global_btn, {
    if (is.null(umf())) {
      showNotification("Please click 'Check data structure' first to create UMF!",
                       type = "error", duration = 5
      )
      return()
    }
    
    if (length(input$global_psi_covs) == 0 && length(input$global_p_covs) == 0) {
      showNotification("Please select at least one covariate for occupancy or detection!",
                       type = "warning", duration = 5
      )
      return()
    }
    
    # Build formula
    psi_part <- if (length(input$global_psi_covs) == 0) {
      "1"
    } else {
      paste(input$global_psi_covs, collapse = " + ")
    }
    
    p_part <- if (length(input$global_p_covs) == 0) {
      "1"
    } else {
      paste(input$global_p_covs, collapse = " + ")
    }
    
    formula_str <- paste("~", p_part, "~", psi_part)
    global_formula <- as.formula(formula_str)
    
    # Extract UMF data
    umf_data <- umf()
    
    # Fit model
    withProgress(message = "Building global model...", value = 0, {
      incProgress(0.5, detail = "Fitting model...")
      
      global_model <- tryCatch(
        {
          occu(global_formula, data = umf_data)
        },
        error = function(e) {
          showNotification(paste("Model fitting error:", e$message),
                           type = "error", duration = 10
          )
          return(NULL)
        }
      )
      
      incProgress(1)
    })
    
    if (is.null(global_model)) {
      return()
    }
    
    # Create short model name
    psi_vars <- if (length(input$global_psi_covs) > 0) {
      paste(substr(input$global_psi_covs, 1, 3), collapse = "+")
    } else {
      "."
    }
    
    p_vars <- if (length(input$global_p_covs) > 0) {
      paste(substr(input$global_p_covs, 1, 3), collapse = "+")
    } else {
      "."
    }
    
    model_name <- paste0("psi(", psi_vars, ")p(", p_vars, ")")
    
    # Store global model first before adding to general storage
    model_storage$global_model <- global_model
    model_storage$global_umf <- umf_data
    model_storage$global_formula <- global_formula
    
    # Then add to general storage
    model_storage$counter <- model_storage$counter + 1
    model_id <- paste0("global_", model_storage$counter)
    model_storage$models[[model_id]] <- global_model
    model_storage$model_names <- c(model_storage$model_names, model_name)
    names(model_storage$models)[length(model_storage$models)] <- model_name
    
    # Update reactive values
    reactive_values$formula <- global_formula
    reactive_values$model <- global_model
    
    showNotification("Global model built successfully! You can now run Dredge.",
                     type = "message", duration = 3
    )
    
    output$model_list_display <- renderPrint({
      cat("Models in storage:\n")
      cat(paste(seq_along(model_storage$model_names),
                model_storage$model_names,
                sep = ". ",
                collapse = "\n"
      ))
    })
    
    # Update dropdown selector for viewing models
    updateSelectInput(session, "selected_model_name",
                      choices = model_storage$model_names,
                      selected = model_name
    )
    
    # Update AUC model selector
    updateSelectInput(session, "auc_model_select",
                      choices = c("", model_storage$model_names),
                      selected = ""
    )
  })
  
  # Add custom model to list
  observeEvent(input$add_model_btn, {
    req(reactive_values$model)
    
    # Create model name from formula
    formula_parts <- as.character(reactive_values$formula)
    
    # Extract variable names from formula
    p_vars_raw <- formula_parts[2]
    if (p_vars_raw == "1") {
      p_part <- "."
    } else {
      p_vars <- strsplit(gsub(" ", "", p_vars_raw), "\\+")[[1]]
      p_part <- paste(substr(p_vars, 1, 3), collapse = "+")
    }
    
    # Occupancy 
    psi_vars_raw <- formula_parts[3]
    if (psi_vars_raw == "1") {
      psi_part <- "."
    } else {
      psi_vars <- strsplit(gsub(" ", "", psi_vars_raw), "\\+")[[1]]
      psi_part <- paste(substr(psi_vars, 1, 3), collapse = "+")
    }
    
    model_name <- paste0("psi(", psi_part, ")p(", p_part, ")")
    
    # Check if model already exists
    if (model_name %in% model_storage$model_names) {
      showNotification("This model already exists in the list!", type = "warning", duration = 3)
      return()
    }
    
    # Rebuild model with explicit UMF data to ensure data is properly embedded
    umf_data <- umf()
    rebuilt_model <- occu(reactive_values$formula, data = umf_data)
    
    # Store the rebuilt model
    model_storage$counter <- model_storage$counter + 1
    model_id <- paste0("custom_model_", model_storage$counter)
    model_storage$models[[model_id]] <- rebuilt_model
    model_storage$model_names <- c(model_storage$model_names, model_name)
    names(model_storage$models)[length(model_storage$models)] <- model_name
    
    showNotification(paste("Model added:", model_name), type = "message", duration = 3)
    
    # Update display
    output$model_list_display <- renderPrint({
      cat("Models in storage:\n")
      cat(paste(seq_along(model_storage$model_names), model_storage$model_names, sep = ". ", collapse = "\n"))
    })
    
    # Update dropdown selector for viewing models
    updateSelectInput(session, "selected_model_name",
                      choices = model_storage$model_names,
                      selected = model_name
    )
    
    # Update AUC model selector
    updateSelectInput(session, "auc_model_select",
                      choices = c("", model_storage$model_names),
                      selected = ""
    )
  })
  
  # Add null model
  observeEvent(input$add_null_model_btn, {
    null_model <- occu(~1 ~ 1, data = umf())
    
    model_storage$models[["null_model"]] <- null_model
    model_storage$model_names <- c(model_storage$model_names, "psi(.)p(.)")
    names(model_storage$models)[length(model_storage$models)] <- "psi(.)p(.)"
    
    showNotification("Null model added!", type = "message", duration = 3)
    
    output$model_list_display <- renderPrint({
      cat("Models in storage:\n")
      cat(paste(seq_along(model_storage$model_names), model_storage$model_names, sep = ". ", collapse = "\n"))
    })
    
    # Update dropdown selector for viewing models
    updateSelectInput(session, "selected_model_name",
                      choices = model_storage$model_names,
                      selected = "psi(.)p(.)"
    )
    
    # Update AUC model selector
    updateSelectInput(session, "auc_model_select",
                      choices = c("", model_storage$model_names),
                      selected = ""
    )
  })
  
  # Model selection & ranking
  
  # Model Selection
  observeEvent(input$rank_models_btn, {
    req(length(model_storage$models) >= 2)
    
    mod_list <- do.call(fitList, model_storage$models)
    ms <- modSel(mod_list)
    model_storage$model_selection <- ms
    
    output$model_selection_table <- renderDT({
      ms_df <- as.data.frame(ms@Full)
      datatable(
        ms_df,
        caption = "Model Selection Results (ranked by AIC)",
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          pageLength = 10
        ),
        rownames = TRUE
      ) %>%
        formatRound(columns = c("AIC", "delta", "AICwt", "cumltvWt"), digits = 3)
    })
    
    showNotification("Models ranked successfully!", type = "message", duration = 3)
  })
  
  # Clear all models
  observeEvent(input$clear_models_btn, {
    model_storage$models <- list()
    model_storage$model_names <- character()
    model_storage$counter <- 0
    
    output$model_list_display <- renderPrint({
      cat("No models in storage.\n")
    })
    
    # Clear dropdown selector
    updateSelectInput(session, "selected_model_name", choices = NULL)
    
    # Clear AUC model selector
    updateSelectInput(session, "auc_model_select", choices = c(""), selected = "")
    
    # Clear model displays
    output$model_name_output <- renderUI({
      NULL
    })
    output$model_summary <- renderPrint({
      cat("No model selected.\n")
    })
    
    showNotification("All models cleared!", type = "warning", duration = 3)
  })
  
  # View selected model in detail
  observeEvent(input$view_model_btn, {
    req(input$selected_model_name)
    
    selected_model <- model_storage$models[[input$selected_model_name]]
    
    if (is.null(selected_model)) {
      showNotification("Selected model not found in storage!",
                       type = "error", duration = 3
      )
      return()
    }
    
    # Extract formula from the model
    model_formula <- formula(selected_model)
    formula_parts <- as.character(model_formula)
    
    # Detection part
    p_part <- formula_parts[2]
    if (p_part == "1") {
      p_display <- "."
    } else {
      p_display <- p_part
    }
    
    # Occupancy part
    psi_part <- formula_parts[3]
    if (psi_part == "1") {
      psi_display <- "."
    } else {
      psi_display <- psi_part
    }
    
    # Render the formula
    output$model_name_output <- renderUI({
      withMathJax(
        helpText(
          paste(
            "$$p(\\text{",
            p_display,
            "}) \\\\ \\psi(\\text{",
            psi_display,
            "})$$",
            sep = ""
          )
        )
      )
    })
    
    # Render the model summary
    output$model_summary <- renderPrint({
      selected_model
    })
    
    showNotification(
      paste("Displaying:", input$selected_model_name),
      type = "message",
      duration = 2
    )
  })
  

  # Custom model-averaging function
  avg_unmarked_custom <- function(ms_obj, delta_threshold = 2) {
    df_full <- ms_obj@Full
    df <- df_full[df_full$delta <= delta_threshold, ]
    
    if (nrow(df) == 0) stop("No models found within that Delta AIC threshold!")
    
    raw_w <- exp(-0.5 * df$delta)
    new_weights <- raw_w / sum(raw_w)
    
    all_cols <- names(df)
    coef_cols <- all_cols[grepl("^(psi|p)\\(", all_cols)]
    
    results_list <- lapply(coef_cols, function(col_name) {
      se_col_name <- paste0("SE", col_name)
      
      is_present <- !is.na(df[[col_name]])
      rel_importance <- sum(new_weights[is_present])
      
      betas <- df[[col_name]]
      betas[is.na(betas)] <- 0
      
      ses <- df[[se_col_name]]
      ses[is.na(ses)] <- 0
      vars <- ses^2
      
      # Unconditional weighted average
      avg_beta <- sum(betas * new_weights)
      
      # Unconditional variance (Burnham & Anderson 2002)
      uncond_var <- sum(new_weights * (vars + (betas - avg_beta)^2))
      uncond_se <- sqrt(uncond_var)
      
      comp <- ifelse(grepl("^psi", col_name), "state", "det")
      
      data.frame(
        Component  = comp,
        Parameter  = col_name,
        Importance = round(rel_importance, 3),
        Estimate   = round(avg_beta, 4),
        Uncond_SE  = round(uncond_se, 4),
        Lower_CI   = round(avg_beta - (1.96 * uncond_se), 4),
        Upper_CI   = round(avg_beta + (1.96 * uncond_se), 4)
      )
    })
    
    final_table <- do.call(rbind, results_list)
    final_table <- final_table[order(final_table$Component, -final_table$Importance), ]
    
    list(
      table = final_table,
      n_models_used = nrow(df),
      n_models_total = nrow(df_full),
      model_names = rownames(df)
    )
  }
  
  # Model Averaging
  observeEvent(input$model_avg_btn, {
    req(model_storage$model_selection)
    
    delta_threshold <- input$delta_aic_threshold
    
    # Get models within delta AIC threshold
    ms_df <- as.data.frame(model_storage$model_selection@Full)
    top_models_idx <- which(ms_df$delta <= delta_threshold)
    
    if (length(top_models_idx) < 2) {
      showNotification(
        paste(
          "Only", length(top_models_idx), "model within delta AIC", delta_threshold,
          ". Model averaging requires at least 2 models."
        ),
        type = "warning",
        duration = 5
      )
      return()
    }
    
    # Model averaging using custom function
    avg_result <- tryCatch(
      {
        avg_unmarked_custom(model_storage$model_selection, delta_threshold = delta_threshold)
      },
      error = function(e) {
        showNotification(
          paste("Model averaging error:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
    )
    
    if (is.null(avg_result)) {
      return()
    }
    
    model_storage$averaged_model <- avg_result
    
    output$model_avg_coef <- renderDT({
      datatable(
        avg_result$table,
        caption = paste("Model-averaged coefficients (Delta (Δ) AIC ≤", delta_threshold, ")"),
        options = list(scrollX = TRUE, pageLength = 20),
        rownames = FALSE
      ) %>%
        formatRound(
          columns = c("Importance", "Estimate", "Uncond_SE", "Lower_CI", "Upper_CI"),
          digits = 4
        )
    })
    
    output$model_avg_summary <- renderPrint({
      cat("--- Model Averaging Summary ---\n")
      cat(paste(
        "Subset uses top", avg_result$n_models_used,
        "out of", avg_result$n_models_total, "models.\n"
      ))
      cat("-------------------------------\n\n")
      cat("Models used in averaging:\n")
      cat(paste("  -", avg_result$model_names, collapse = "\n"))
      cat("\n\nModel averaging complete!")
    })
    
    showNotification(
      paste("Model averaging complete using", avg_result$n_models_used, "models!"),
      type = "message",
      duration = 3
    )
  })
  
  # Dredge Global Model (not relevant)
  
  # Custom dredge function
  dredge_occu_psi <- function(umf,
                              p_covs,
                              psi_covs) {
    # Detection part
    p_part <- if (length(p_covs) == 0) "1" else paste(p_covs, collapse = " + ")
    
    # All psi subsets (including null)
    psi_forms <- unlist(
      lapply(0:length(psi_covs), function(k) {
        if (k == 0) {
          "1"
        } else {
          combn(psi_covs, k, FUN = function(x) paste(x, collapse = " + "))
        }
      })
    )
    
    # Fit models
    models <- lapply(psi_forms, function(psi) {
      form <- as.formula(paste("~", p_part, "~", psi))
      occu(form, data = umf)
    })
    
    model_names <- vapply(psi_forms, function(psi) {
      psi_lab <- if (psi == "1") "." else psi
      p_lab <- if (p_part == "1") "." else p_part
      paste0("psi(", psi_lab, ")p(", p_lab, ")")
    }, character(1))
    
    names(models) <- model_names
    
    fitList(fits = models)
  }
  
  observeEvent(input$dredge_btn, {
    if (is.null(model_storage$global_model)) {
      showNotification(
        "Please build a Global Model first using 'Build Global Model'!",
        type = "error", duration = 5
      )
      return()
    }
    
    umf_data <- model_storage$global_umf
    psi_covs <- input$global_psi_covs
    p_covs <- input$global_p_covs
    
    if (length(psi_covs) == 0) {
      showNotification(
        "Select at least one psi covariate for dredge.",
        type = "warning", duration = 5
      )
      return()
    }
    
    withProgress(message = "Running dredge (psi only)...", value = 0, {
      incProgress(0.2, detail = "Generating candidate models")
      
      fitlist <- tryCatch(
        {
          dredge_occu_psi(
            umf = umf_data,
            p_covs = p_covs,
            psi_covs = psi_covs
          )
        },
        error = function(e) {
          showNotification(e$message, type = "error", duration = 10)
          return(NULL)
        }
      )
      
      if (is.null(fitlist)) {
        return()
      }
      
      incProgress(0.5, detail = "Checking convergence")
      
      conv <- sapply(fitlist@fits, function(m) m@opt$convergence)
      fitlist <- fitList(fits = fitlist@fits[conv == 0])
      
      incProgress(0.7, detail = "Model selection")
      
      dredge_modsel <- modSel(fitlist)
      
      incProgress(1)
      
      # Store
      model_storage$dredge_fitlist <- fitlist
      model_storage$dredge_modsel <- dredge_modsel
     
      dredge_df <- as.data.frame(dredge_modsel@Full)[
        ,
        c("model", "negLogLike", "nPars", "n", "AIC", "delta", "AICwt", "cumltvWt")
      ]
      dredge_df <- dredge_df[order(dredge_df$delta), ] # sort by delta
      
      model_storage$dredge_table_df <- dredge_df
      
      # Filter top models by delta threshold
      delta_threshold <- input$dredge_delta_threshold
      model_storage$dredge_top_models <- dredge_df[dredge_df$delta <= delta_threshold, ]
      
      output$dredge_table <- renderDT({
        datatable(
          dredge_df,
          caption = "Dredge Results (psi-only, p fixed)",
          options = list(scrollX = TRUE, pageLength = 20),
          rownames = FALSE
        ) %>% formatRound(columns = which(sapply(dredge_df, is.numeric)), digits = 3)
      })
      
      output$dredge_summary <- renderPrint({
        cat("Dredge Summary\n")
        cat("==============\n")
        cat(
          "Detection model (fixed): ",
          if (length(p_covs) == 0) "1" else paste(p_covs, collapse = " + "), "\n"
        )
        cat("Psi covariates considered: ", paste(psi_covs, collapse = ", "), "\n")
        cat("Models evaluated: ", nrow(dredge_df), "\n")
        cat("Models within Δ ≤", delta_threshold, ": ", nrow(model_storage$dredge_top_models), "\n")
      })
      
      showNotification(
        paste("Dredge complete!", nrow(dredge_df), "models evaluated."),
        type = "message", duration = 5
      )
    })
  })
  
  # Model average dredge estimates (not relevant)
  
  # Custom model averaging
  average_dredge_modSel <- function(modsel, delta_threshold = 2) {
 
    df_full <- modsel@Full
    df <- df_full[df_full$delta <= delta_threshold, ]
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    raw_w <- exp(-0.5 * df$delta)
    new_weights <- raw_w / sum(raw_w)
    
    all_cols <- names(df)
    coef_cols <- all_cols[grepl("^(psi|p)\\(", all_cols)]
    
    results_list <- lapply(coef_cols, function(col_name) {
      se_col_name <- paste0("SE", col_name)
      
      is_present <- !is.na(df[[col_name]])
      rel_importance <- sum(new_weights[is_present])
      
      betas <- df[[col_name]]
      betas[is.na(betas)] <- 0
      
      ses <- df[[se_col_name]]
      ses[is.na(ses)] <- 0
      vars <- ses^2
      
      # Unconditional weighted average
      avg_beta <- sum(betas * new_weights)
      
      # Unconditional variance (Burnham & Anderson 2002)
      uncond_var <- sum(new_weights * (vars + (betas - avg_beta)^2))
      uncond_se <- sqrt(uncond_var)
      
      comp <- ifelse(grepl("^psi", col_name), "state", "det")
      
      data.frame(
        Component = comp,
        Parameter = col_name,
        Estimate = round(avg_beta, 4),
        Uncond_SE = round(uncond_se, 4),
        Lower_CI = round(avg_beta - (1.96 * uncond_se), 4),
        Upper_CI = round(avg_beta + (1.96 * uncond_se), 4),
        stringsAsFactors = FALSE
      )
    })
    
    final_table <- do.call(rbind, results_list)
    
    final_table <- final_table[order(final_table$Component, final_table$Parameter), ]
    
    return(final_table)
  }
  
  # Model-averaging from dredge - not relevant!!!
  observeEvent(input$dredge_avg_btn, {
    if (is.null(model_storage$dredge_modsel)) {
      showNotification(
        "Please run dredge first!",
        type = "error", duration = 5
      )
      return()
    }
    
    # Delta threshold for model averaging
    delta_threshold <- input$dredge_delta_threshold
    
    # Model-averaging using custom function
    avg_df <- average_dredge_modSel(
      modsel = model_storage$dredge_modsel,
      delta_threshold = delta_threshold
    )
    
    if (is.null(avg_df)) {
      showNotification(
        "No models within the Δ threshold for averaging.",
        type = "warning", duration = 5
      )
      return()
    }
    
    model_storage$dredge_averaged <- avg_df
    
    output$dredge_avg_coef <- renderDT({
      datatable(
        avg_df,
        caption = paste0("Model-averaged coefficients (Δ ≤ ", delta_threshold, ")"),
        options = list(scrollX = TRUE, pageLength = 20),
        rownames = FALSE
      ) %>% formatRound(
        columns = c("Estimate", "Uncond_SE", "Lower_CI", "Upper_CI"),
        digits = 4
      )
    })
    
    showNotification(
      "Model averaging complete!",
      type = "message", duration = 5
    )
  })
  

  # Goodness of fit
  # Custom GoF function (adapted from AICmodavg package with a minimal change)
  
  ## for single-season occupancy models of class unmarkedFitOccu
  mb.chisq.unmarkedFitOccu <- function(mod, print.table = TRUE, ...) {
    stopifnot(inherits(mod, "unmarkedFitOccu"))
    
    ## step 1:
    ## extract detection histories
    y.raw <- mod@data@y
    
    ## if some rows are all NA and sites are discarded, adjust sample size accordingly
    N.raw <- nrow(y.raw)
    # if(all NA) {N - number of rows with all NA}
    ## identify sites without data
    na.raw <- apply(X = y.raw, MARGIN = 1, FUN = function(i) all(is.na(i)))
    
    ## remove sites without data
    y.data <- y.raw[!na.raw, ]
    
    N <- N.raw - sum(na.raw)
    
    # N is required for computations in the end
    Ts <- ncol(y.data)
    
    det.hist <- apply(X = y.data, MARGIN = 1, FUN = function(i) paste(i, collapse = ""))
    
    ## compute predicted values of occupancy
    preds.psi <- predict(mod, type = "state")$Predicted
    
    ## compute predicted values of p
    preds.p <- matrix(
      data = predict(mod, type = "det")$Predicted,
      ncol = Ts, byrow = TRUE
    )
    
    ## assemble in data.frame
    out.hist <- data.frame(det.hist, preds.psi, stringsAsFactors = TRUE)
    
    ## identify unique histories
    un.hist <- unique(det.hist)
    n.un.hist <- length(un.hist)
    
    ## identify if missing values occur
    na.vals <- length(grep(pattern = "NA", x = un.hist)) > 0
    
    if (na.vals) {
      ## identify each history with NA
      id.na <- grep(pattern = "NA", x = un.hist)
      id.det.hist.na <- grep(pattern = "NA", x = det.hist)
      
      ## cohorts with NA
      cohort.na <- sort(un.hist[id.na])
      n.cohort.na <- length(cohort.na)
      ## determine cohorts that will be grouped together (same missing value)
      unique.na <- gsub(pattern = "NA", replacement = "N", x = cohort.na)
      ## determine which visit has missing value
      na.visits <- sapply(strsplit(x = unique.na, split = ""), FUN = function(i) paste(ifelse(i == "N", 1, 0), collapse = ""))
      ## add cohort labels for histories
      names(cohort.na) <- na.visits
      ## number of histories in each cohort
      n.hist.missing.cohorts <- table(na.visits)
      ## number of missing cohorts
      n.missing.cohorts <- length(n.hist.missing.cohorts)
      out.hist.na <- out.hist[id.det.hist.na, ]
      out.hist.na$det.hist <- droplevels(out.hist.na$det.hist)
      
      ## groupings in out.hist.na
      just.na <- sapply(X = out.hist.na$det.hist, FUN = function(i) gsub(pattern = "1", replacement = "0", x = i))
      out.hist.na$coh <- sapply(X = just.na, FUN = function(i) gsub(pattern = "NA", replacement = "1", x = i))
      ## number of sites in each missing cohort
      freqs.missing.cohorts <- table(out.hist.na$coh)
      
      ## number of sites with each history
      na.freqs <- table(det.hist[id.det.hist.na])
      preds.p.na <- preds.p[id.det.hist.na, ]
      
      ## cohorts without NA
      cohort.not.na <- sort(un.hist[-id.na])
      out.hist.not.na <- out.hist[-id.det.hist.na, , drop = FALSE]
      out.hist.not.na$det.hist <- droplevels(out.hist.not.na$det.hist)
      n.cohort.not.na <- length(cohort.not.na)
      n.sites.not.na <- length(det.hist) - length(id.det.hist.na)
      preds.p.not.na <- preds.p[-id.det.hist.na, ]
    } else {
      cohort.not.na <- sort(un.hist)
      out.hist.not.na <- out.hist
      preds.p.not.na <- preds.p
      n.cohort.not.na <- length(cohort.not.na)
      n.sites.not.na <- length(det.hist)
    }
    
    ## for each missing data cohort, determine number of sites for each
    
    ## iterate over each site for each unique history
    if (n.cohort.not.na > 0) { ## expected frequencies for non-missing data
      exp.freqs <- rep(NA, n.cohort.not.na)
      names(exp.freqs) <- cohort.not.na
      
      # SORT ENCOUNTER HISTORIES CHECK THAT ORDER IS IDENTICAL TO OBSERVED FREQS
      
      ## iterate over detection histories
      for (i in 1:n.cohort.not.na) {
        eq.solved <- rep(NA, n.sites.not.na)
        select.hist <- cohort.not.na[i]
        ## strip all values
        strip.hist <- unlist(strsplit(select.hist, split = ""))
        
        ## translate each visit in probability statement
        hist.mat <- matrix(NA, nrow = n.sites.not.na, ncol = Ts)
        
        ## iterate over sites
        for (j in 1:n.sites.not.na) {
          ## in extreme cases where only a single cohort occurs without missing values
          if (n.sites.not.na == 1) {
            hist.mat[j, ] <- ifelse(strip.hist == "1", preds.p.not.na,
                                    ifelse(strip.hist == "0", 1 - preds.p.not.na,
                                           0
                                    )
            )
          } else {
            hist.mat[j, ] <- ifelse(strip.hist == "1", preds.p.not.na[j, ],
                                    ifelse(strip.hist == "0", 1 - preds.p.not.na[j, ],
                                           0
                                    )
            )
          }
          
          ## combine into equation
          combo.p <- paste(hist.mat[j, ], collapse = "*")
          ## for history without detection
          if (sum(as.numeric(strip.hist)) == 0) {
            combo.first <- paste(c(out.hist.not.na[j, "preds.psi"], combo.p), collapse = "*")
            combo.psi.p <- paste((1 - out.hist.not.na[j, "preds.psi"]), "+", combo.first)
          } else {
            combo.psi.p <- paste(c(out.hist.not.na[j, "preds.psi"], combo.p), collapse = "*")
          }
          eq.solved[j] <- eval(parse(text = as.expression(combo.psi.p)))
        }
        
        exp.freqs[i] <- sum(eq.solved, na.rm = TRUE)
      }
      
      ## for each detection history, compute observed frequencies
      freqs <- table(out.hist.not.na$det.hist)
      
      out.freqs <- matrix(NA, nrow = n.cohort.not.na, ncol = 4)
      colnames(out.freqs) <- c("Cohort", "Observed", "Expected", "Chi-square")
      rownames(out.freqs) <- names(freqs)
      ## cohort
      out.freqs[, 1] <- 0
      ## observed
      out.freqs[, 2] <- freqs
      ## expected
      out.freqs[, 3] <- exp.freqs
      ## chi-square
      out.freqs[, 4] <- ((out.freqs[, "Observed"] - out.freqs[, "Expected"])^2) / out.freqs[, "Expected"]
    }
    
    ## if missing values
    if (na.vals) {
      ## create list to store the chisquare for each cohort
      missing.cohorts <- list()
      ## check if preds.p.na has only 1 row and change to matrix
      if (!is.matrix(preds.p.na)) {
        preds.p.na <- matrix(data = preds.p.na, nrow = 1)
      }
      
      for (m in 1:n.missing.cohorts) {
        ## select cohort
        select.cohort <- out.hist.na[which(out.hist.na$coh == names(freqs.missing.cohorts)[m]), ]
        select.preds.p.na <- preds.p.na[which(out.hist.na$coh == names(freqs.missing.cohorts)[m]), ]
        ## replace NA's with 1 to remove from likelihood
        if (!is.matrix(select.preds.p.na)) {
          select.preds.p.na <- matrix(data = select.preds.p.na, nrow = 1)
        }
        select.preds.p.na[, gregexpr(pattern = "N", text = gsub(pattern = "NA", replacement = "N", x = select.cohort$det.hist[1]))[[1]]] <- 1
        n.total.sites <- nrow(select.cohort)
        freqs.na <- table(droplevels(select.cohort$det.hist))
        cohort.na.un <- sort(unique(select.cohort$det.hist))
        n.hist.na <- length(freqs.na)
        exp.na <- rep(NA, n.hist.na)
        names(exp.na) <- cohort.na.un
        
        for (i in 1:n.hist.na) {
          ## number of sites in given history
          n.sites.hist <- freqs.na[i] ## this should be number of sites for each history
          eq.solved <- rep(NA, n.total.sites)
          ## replace NA's with N
          select.hist <- gsub(pattern = "NA", replacement = "N", x = cohort.na.un[i])
          ## strip all values
          strip.hist <- unlist(strsplit(select.hist, split = ""))
          ## translate each visit in probability statement
          hist.mat <- matrix(NA, nrow = n.total.sites, ncol = Ts)
          ## iterate over sites
          for (j in 1:n.total.sites) {
            ## modified
            if (Ts == 1) {
              hist.mat[j, ] <- ifelse(strip.hist == "1", select.preds.p.na[j],
                                      ifelse(strip.hist == "0", 1 - select.preds.p.na[j], 1)
              )
            } else {
              hist.mat[j, ] <- ifelse(strip.hist == "1", select.preds.p.na[j, ],
                                      ifelse(strip.hist == "0", 1 - select.preds.p.na[j, ], 1)
              )
            }
            ## modified
            ## replace NA by 1 (missing visit is removed from likelihood)
            ## for missing value, remove occasion
            ## combine into equation
            combo.p <- paste(hist.mat[j, ], collapse = "*")
            ## for history without detection
            if (sum(as.numeric(gsub(pattern = "N", replacement = "0", x = strip.hist))) == 0) {
              combo.first <- paste(c(select.cohort[j, "preds.psi"], combo.p), collapse = "*")
              combo.psi.p <- paste((1 - select.cohort[j, "preds.psi"]), "+", combo.first)
            } else {
              combo.psi.p <- paste(c(select.cohort[j, "preds.psi"], combo.p), collapse = "*")
            }
            
            eq.solved[j] <- eval(parse(text = as.expression(combo.psi.p)))
          }
          
          exp.na[i] <- sum(eq.solved, na.rm = TRUE)
        }
        
        ## compute chisq for missing data cohorts
        
        ## for each detection history, compute observed frequencies
        out.freqs.na <- matrix(NA, nrow = n.hist.na, ncol = 4)
        colnames(out.freqs.na) <- c("Cohort", "Observed", "Expected", "Chi-square")
        rownames(out.freqs.na) <- cohort.na.un
        ## cohort
        out.freqs.na[, 1] <- m
        ## observed
        out.freqs.na[, 2] <- freqs.na
        ## expected
        out.freqs.na[, 3] <- exp.na
        ## chi-square
        out.freqs.na[, 4] <- ((out.freqs.na[, "Observed"] - out.freqs.na[, "Expected"])^2) / out.freqs.na[, "Expected"]
        
        missing.cohorts[[m]] <- list(out.freqs.na = out.freqs.na)
      }
    }
    ## test statistic is chi-square for all possible detection histories
    ## for observed detection histories, chisq = sum((obs - exp)^2)/exp = Y
    
    ## to avoid computing all possible detection histories, it is possible to obtain it by subtraction:
    # for unobserved detection histories, chisq = sum((obs - exp)^2)/exp = sum(0 - exp)^2/exp = sum(exp) = X
    ## X = N.sites - sum(exp values from observed detection histories)
    ## Thus, test statistic = Y + X = Y + N - sum(exp values from observed detection histories)
    
    ## compute partial chi-square for observed detection histories (Y)
    # chisq.obs.det <- sum(((out.freqs[, "observed"] - out.freqs[, "expected"])^2)/out.freqs[, "expected"])
    
    ## compute partial chi-square for unobserved detection histories (X)
    if (na.vals) {
      chisq.missing <- do.call("rbind", lapply(missing.cohorts, FUN = function(i) i$out.freqs.na))
      if (n.cohort.not.na > 0) {
        chisq.unobs.det <- N - sum(out.freqs[, "Expected"]) - sum(chisq.missing[, "Expected"])
        chisq.table <- rbind(out.freqs, chisq.missing)
      } else {
        chisq.unobs.det <- N - sum(chisq.missing[, "Expected"])
        chisq.table <- chisq.missing
      }
    } else {
      chisq.unobs.det <- N - sum(out.freqs[, "Expected"])
      chisq.na <- 0
      chisq.table <- out.freqs
    }
    
    ## test statistic (Y + X = Y - sum(exp observed detection histories)
    chisq <- sum(chisq.table[, "Chi-square"]) + chisq.unobs.det
    
    if (print.table) {
      out <- list(
        chisq.table = chisq.table, chi.square = chisq,
        model.type = "single-season"
      )
    } else {
      out <- list(chi.square = chisq, model.type = "single-season")
    }
    
    class(out) <- "mb.chisq"
    return(out)
  }
  
  # gof function
  observeEvent(input$run_gof_btn, {
    if (is.null(model_storage$global_model)) {
      showNotification(
        "Please build a Global Model first using 'Build Global Model' button!",
        type = "error",
        duration = 5
      )
      return()
    }
    
    nsim_value <- input$nsim_input
    
    # Validate nsim value
    if (is.null(nsim_value) || nsim_value < 1) {
      showNotification(
        "Please enter a valid number of simulations (e.g., 100 or 1000)",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    # Access global model
    global_model <- model_storage$global_model
    
    withProgress(
      message = paste("Running GoF test with", nsim_value, "simulations"),
      detail = "Preparing model...",
      value = 0,
      {
        chi_star <- numeric(nsim_value)
        
        incProgress(0.05, detail = "Starting bootstrap iterations...")
        
        for (i in 1:nsim_value) {
          boot_i <- tryCatch(
            {
              unmarked::parboot(
                global_model,
                statistic = function(m) mb.chisq.unmarkedFitOccu(m)$chi.square,
                nsim = 1
              )@t.star
            },
            error = function(e) NA
          )
          
          chi_star[i] <- ifelse(is.finite(boot_i), boot_i, NA)
          
          incProgress(1 / nsim_value, detail = paste("Bootstrap iteration", i, "of", nsim_value))
        }
        
        chi_star <- chi_star[is.finite(chi_star)]
        
        if (length(chi_star) < 10) {
          showNotification(
            "Too few valid bootstrap replicates to compute GoF.",
            type = "error",
            duration = 10
          )
          return()
        }
        
        # Compute observed chi-square & GoF stats
        t0 <- mb.chisq.unmarkedFitOccu(global_model)$chi.square
        p_value <- mean(chi_star >= t0)
        c_hat <- t0 / mean(chi_star)
        
        gof_result <- list(
          chi.square = t0,
          chi.square.boot = chi_star,
          p.value = p_value,
          c.hat.est = c_hat,
          nsim = nsim_value,
          nsim.success = length(chi_star),
          quantiles = quantile(chi_star, na.rm = TRUE),
          model.type = "single-season"
        )
        class(gof_result) <- "mb.chisq"
        
        # Store results
        model_storage$global_gof <- gof_result
        
        showNotification(
          paste0(
            "Goodness-of-Fit test completed!\n",
            "Chi-square = ", round(gof_result$chi.square, 3), "\n",
            "p-value = ", round(gof_result$p.value, 3)
          ),
          type = "message",
          duration = 8
        )
      }
    )
  })
  

  # GoF summary
  output$gof_results <- renderPrint({
    gof <- model_storage$global_gof
    if (is.null(gof)) {
      cat("No GoF results available.\nClick 'Run Goodness-of-Fit simulation' to compute statistics.\n")
    } else {
      cat("MacKenzie-Bailey Goodness-of-Fit Test\n")
      cat("=====================================\n\n")
      cat("Chi-square statistic =", round(gof$chi.square, 3), "\n")
      cat("Number of bootstrap samples =", gof$nsim.success, "\n")
      cat("P-value =", round(gof$p.value, 3), "\n\n")
      cat("Quantiles of bootstrap chi-square:\n")
      print(round(gof$quantiles, 2))
      cat("\nEstimate of c-hat =", round(gof$c.hat.est, 2), "\n\n")
      
      cat("Interpretation:\n---------------\n")
      if (gof$p.value > 0.05) {
        cat("p-value > 0.05: Model fits the data adequately (no evidence of lack of fit)\n")
      } else {
        cat("p-value < 0.05: Model shows evidence of lack of fit\n")
      }
    }
  })
  
  # GoF histogram 
  output$gof_plot <- renderPlot({
    library(ggplot2)
    
    gof <- model_storage$global_gof
    
    if (is.null(gof)) {
      ggplot() +
        theme_void() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Run GoF to see histogram",
          size = 6,
          colour = "gray50"
        )
    } else {
      df <- data.frame(chi_sq = gof$chi.square.boot)
      
      ggplot(df, aes(x = chi_sq)) +
        geom_histogram(
          bins = 30,
          fill = "steelblue",
          color = "white"
        ) +
        geom_vline(
          xintercept = gof$chi.square,
          colour = "#cc3333",
          linewidth = 1,
          linetype = "dashed"
        ) +
        labs(
          x = "Chi-square statistic",
          y = "Frequency"
        ) +
        annotate(
          "text",
          x = Inf,
          y = Inf,
          hjust = 1.1,
          vjust = 2,
          label = paste0(
            "Observed (chi^2 == ", round(gof$chi.square, 2), ")*'\n'*",
            "p-value == ", round(gof$p.value, 3)
          ),
          parse = TRUE,
          colour = "#993333",
          size = 5.5
        ) +
        theme_minimal() +
        theme(
          axis.text = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 17)
        )
    }
  })

  # update models on button click
  observeEvent(input$update_model_btn, {
    if (is.null(umf())) {
      showNotification("Please click 'Check data structure' first to create UMF!",
                       type = "error", duration = 5
      )
      return()
    }
    
    if (is.null(input$first_part) || is.null(input$second_part)) {
      showNotification("Please select covariates for detection and occupancy models!",
                       type = "warning", duration = 5
      )
      return()
    }
    
    # Fit model
    withProgress(message = "Building custom model...", value = 0, {
      incProgress(0.5, detail = "Fitting model...")
      
      reactive_values$model <- tryCatch(
        {
          reactive_model()
        },
        error = function(e) {
          showNotification(paste("Model fitting error:", e$message),
                           type = "error", duration = 10
          )
          return(NULL)
        }
      )
      
      incProgress(1)
    })
    
    if (is.null(reactive_values$model)) {
      return()
    }
    
    # unique identifier for the current model
    p_part <- ifelse(
      length(input$first_part) == 1 && input$first_part == "1", "1",
      paste(input$first_part, collapse = "+")
    )
    
    psi_part <- ifelse(
      length(input$second_part) == 1 && input$second_part == "1", "1",
      paste(input$second_part, collapse = "+")
    )
    
    output$psi_table <- renderDT({
      req(psi_all_val())
      datatable(psi_all_val())
    })
    
    showNotification("Custom model built successfully! Add it to Model List to view details.",
                     type = "message", duration = 4
    )
    
    # Update the model formula display
    reactive_values$formula <- as.formula(paste(
      "~",
      paste(input$first_part, collapse = "+"),
      "~",
      paste(input$second_part, collapse = "+")
    ))
  })
  

  # AUC CALCULATIONS - Individual model
  
  auc_data <- reactiveValues(
    auc_detection = NULL,
    auc_latent = NULL,
    ci_detection = NULL,
    ci_latent = NULL,
    plot_data_detection = NULL,
    plot_data_latent = NULL,
    model_name = NULL
  )
  
  # Trigger AUC calculation when model is selected
  observeEvent(input$auc_model_select, {
    if (is.null(input$auc_model_select) || input$auc_model_select == "") {
      auc_data$auc_detection <- NULL
      auc_data$auc_latent <- NULL
      auc_data$ci_detection <- NULL
      auc_data$ci_latent <- NULL
      auc_data$plot_data_detection <- NULL
      auc_data$plot_data_latent <- NULL
      auc_data$model_name <- NULL
      return()
    }
    
    # model name
    model_name <- input$auc_model_select
    
    # Selected model from storage
    selected_model <- model_storage$models[[model_name]]
    
    # Validate model exists
    if (is.null(selected_model)) {
      showNotification(
        paste("Model", model_name, "not found in storage!"),
        type = "error",
        duration = 3
      )
      auc_data$auc_detection <- NULL
      auc_data$auc_latent <- NULL
      auc_data$ci_detection <- NULL
      auc_data$ci_latent <- NULL
      auc_data$plot_data_detection <- NULL
      auc_data$plot_data_latent <- NULL
      auc_data$model_name <- NULL
      return()
    }
    
    # Validate detection data exists
    if (is.null(detn()) || is.null(input$col_range_detn)) {
      showNotification(
        "Detection data not found. Please process data first!",
        type = "error",
        duration = 5
      )
      return()
    }
    
    showNotification(
      paste("Calculating AUC for:", model_name),
      type = "message",
      duration = 1.5,
      id = "auc_calc_msg"
    )
    
    # Predicted occupancy
    psi_pred <- predict(selected_model, type = "state")$Predicted
    
    # Calculate z values (latent occurrence - detection corrected)
    re <- unmarked::ranef(selected_model)
    e_bup <- unmarked::bup(re, stat = "mean")
    z_vals_latent <- round(e_bup)

    # Using actual observed detections
    
    # Detection history
    detection_cols <- input$col_range_detn
    detection_history <- as.matrix(detn()[, detection_cols, drop = FALSE])
    
    # 1 if detected in ANY survey, 0 otherwise
    y_true_detection <- apply(detection_history, 1, function(x) {
      as.integer(any(x == 1, na.rm = TRUE))
    })
    
    # Calculate ROC
    roc_obj_detection <- pROC::roc(
      response = y_true_detection,
      predictor = psi_pred,
      quiet = TRUE,
      ci = TRUE
    )
    
    # Prepare for plot
    pred_rocr_detection <- ROCR::prediction(psi_pred, y_true_detection)
    perf_detection <- ROCR::performance(pred_rocr_detection, "tpr", "fpr")
 
    # Using latent occupancy estimates
    roc_obj_latent <- pROC::roc(
      response = as.numeric(z_vals_latent),
      predictor = psi_pred,
      quiet = TRUE,
      ci = TRUE
    )
    
    pred_rocr_latent <- ROCR::prediction(psi_pred, z_vals_latent)
    perf_latent <- ROCR::performance(pred_rocr_latent, "tpr", "fpr")
    
    # Store results in reactiveValues
    auc_data$auc_detection <- as.numeric(roc_obj_detection$auc)
    auc_data$auc_latent <- as.numeric(roc_obj_latent$auc)
    auc_data$ci_detection <- as.numeric(roc_obj_detection$ci)
    auc_data$ci_latent <- as.numeric(roc_obj_latent$ci)
    
    auc_data$plot_data_detection <- data.frame(
      fpr = perf_detection@x.values[[1]],
      tpr = perf_detection@y.values[[1]],
      method = "Observed detections"
    )
    
    auc_data$plot_data_latent <- data.frame(
      fpr = perf_latent@x.values[[1]],
      tpr = perf_latent@y.values[[1]],
      method = "Latent occupancy"
    )
    
    auc_data$model_name <- model_name
  })
  
  # AUC summary
  output$auc_text_individual_dual <- renderUI({
    req(auc_data$auc_detection)
    req(auc_data$auc_latent)
    
    ci_det <- auc_data$ci_detection
    ci_lat <- auc_data$ci_latent
    
    tags$div(
      tags$h4(paste("AUC Results for:", auc_data$model_name)),
      tags$hr(),
      tags$div(
        style = "display: flex; justify-content: space-around;",
        tags$div(
          style = "text-align: center; padding: 10px; border: 2px solid #2196F3; border-radius: 5px; background-color: #E3F2FD;",
          tags$h5(style = "color: #1976D2;", "Using observed detections"),
          tags$p(
            style = "font-size: 18px; font-weight: bold; color: #0D47A1;",
            sprintf("AUC = %.3f", ci_det[2])
          ),
          tags$p(
            style = "font-size: 14px; color: #1565C0;",
            sprintf("95%% CI: %.3f – %.3f", ci_det[1], ci_det[3])
          ),
          tags$p(
            style = "font-size: 12px; color: #424242;",
            "Conservative estimate based on actual observations"
          )
        ),
        tags$div(
          style = "text-align: center; padding: 10px; border: 2px solid #F44336; border-radius: 5px; background-color: #FFEBEE;",
          tags$h5(style = "color: #C62828;", "Using latent occupancy"),
          tags$p(
            style = "font-size: 18px; font-weight: bold; color: #B71C1C;",
            sprintf("AUC = %.3f", ci_lat[2])
          ),
          tags$p(
            style = "font-size: 14px; color: #D32F2F;",
            sprintf("95%% CI: %.3f – %.3f", ci_lat[1], ci_lat[3])
          ),
          tags$p(
            style = "font-size: 12px; color: #424242;",
            "Detection-corrected estimate accounting for missed detections"
          )
        )
      )
    )
  })
  
  # Create plot
  output$auc_plot_individual_dual <- renderPlotly({
    req(auc_data$plot_data_detection)
    req(auc_data$plot_data_latent)
    
    plot_data_combined <- rbind(
      auc_data$plot_data_detection,
      auc_data$plot_data_latent
    )
    
    auc_det <- auc_data$auc_detection
    auc_lat <- auc_data$auc_latent
    ci_det <- auc_data$ci_detection
    ci_lat <- auc_data$ci_latent
    
    anno_text_det <- sprintf(
      "Observed detections\nAUC = %.3f\n95%% CI: %.3f–%.3f",
      ci_det[2], ci_det[1], ci_det[3]
    )
    
    anno_text_lat <- sprintf(
      "Latent occupancy\nAUC = %.3f\n95%% CI: %.3f–%.3f",
      ci_lat[2], ci_lat[1], ci_lat[3]
    )
    
    # Create plot
    roc_plot <- ggplot(plot_data_combined, aes(x = fpr, y = tpr, color = method)) +
      geom_line(linewidth = 1.5) +
      geom_abline(linetype = "dashed", color = "grey50") +
      scale_color_manual(
        values = c(
          "Observed detections" = "#0072b2",
          "Latent occupancy" = "#cc3333"
        ),
        name = ""
      ) +
      annotate("text",
               x = 0.70, y = 0.35,
               label = anno_text_det,
               size = 3.5, hjust = 0,
               color = "#1565C0"
      ) +
      annotate("text",
               x = 0.70, y = 0.15,
               label = anno_text_lat,
               size = 3.5, hjust = 0,
               color = "#C62828"
      ) +
      labs(
        title = paste("ROC Curves:", auc_data$model_name),
        x = "1 - Specificity (False Positive Rate)",
        y = "Sensitivity (True Positive Rate)"
      ) +
      theme_bw(base_size = 14) +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold")
      )
    
    ggplotly(roc_plot) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.35
        ),
        margin = list(b = 100)
      )
  })
  
  # Model-averaged AUC
  
  # Store model-averaged AUC results
  auc_modavg_data <- reactiveValues(
    auc_detection = NULL,
    auc_latent = NULL,
    ci_detection = NULL,
    ci_latent = NULL,
    plot_data_detection = NULL,
    plot_data_latent = NULL,
    n_models = NULL,
    delta_threshold = NULL
  )
  
  # Calculate Model-Averaged AUC when button is clicked
  observeEvent(input$calc_auc_modavg_btn, {
    if (is.null(model_storage$model_selection)) {
      showNotification(
        "Please rank models first using 'Rank Models' button!",
        type = "error",
        duration = 5
      )
      return()
    }
    
    if (is.null(detn()) || is.null(input$col_range_detn)) {
      showNotification(
        "Detection data not found. Please process data first!",
        type = "error",
        duration = 5
      )
      return()
    }
    
    showNotification(
      "Calculating model-averaged AUC...",
      type = "message",
      duration = 2,
      id = "auc_modavg_calc"
    )
    
    # Get model selection object & delta threshold
    ms_obj <- model_storage$model_selection
    delta_threshold <- input$auc_delta_threshold_modavg
    
    df_ms <- as.data.frame(ms_obj@Full)
    
    # Get indices of models within threshold
    top_indices <- which(df_ms$delta <= delta_threshold)
    
    if (length(top_indices) == 0) {
      showNotification(
        paste("No models found within Delta AIC ≤", delta_threshold),
        type = "warning",
        duration = 5
      )
      removeNotification(id = "auc_modavg_calc")
      return()
    }
    
    # Extract selected models
    all_model_names <- names(model_storage$models)
    selected_model_names <- all_model_names[top_indices]
    selected_models <- model_storage$models[selected_model_names]
    
    # Re-calculate weights for the subset
    subset_deltas <- df_ms$delta[df_ms$delta <= delta_threshold]
    raw_w <- exp(-0.5 * subset_deltas)
    sub_weights <- raw_w / sum(raw_w)
    
    # Get site covariates
    first_mod <- selected_models[[1]]
    site_covs <- first_mod@data@siteCovs
    
    # Predict psi for each model & create weighted average
    pred_list <- lapply(selected_models, function(m) {
      predict(m, type = "state", newdata = site_covs)$Predicted
    })
    
    # Weighted average of predictions
    pred_matrix <- do.call(cbind, pred_list)
    psi_avg <- as.numeric(pred_matrix %*% sub_weights)
    
    # Using actual observed detections
    # Get detection history
    detection_cols <- input$col_range_detn
    detection_history <- as.matrix(detn()[, detection_cols, drop = FALSE])
    
    # 1 if detected in ANY survey, 0 otherwise
    y_true_detection <- apply(detection_history, 1, function(x) {
      as.integer(any(x == 1, na.rm = TRUE))
    })
    
    # Calculate ROC & AUC
    roc_obj_detection <- pROC::roc(
      response = y_true_detection,
      predictor = psi_avg,
      quiet = TRUE,
      ci = TRUE
    )
    
    pred_rocr_detection <- ROCR::prediction(psi_avg, y_true_detection)
    perf_detection <- ROCR::performance(pred_rocr_detection, "tpr", "fpr")
    
    # Using latent occupancy estimates (detection-corrected values)
    bup_list <- lapply(selected_models, function(m) {
      re <- unmarked::ranef(m)
      as.numeric(unmarked::bup(re, stat = "mean"))
    })
    bup_matrix <- do.call(cbind, bup_list)
    avg_bup <- as.numeric(bup_matrix %*% sub_weights)
    y_true_latent <- round(avg_bup)
    
    roc_obj_latent <- pROC::roc(
      response = y_true_latent,
      predictor = psi_avg,
      quiet = TRUE,
      ci = TRUE
    )
    
    pred_rocr_latent <- ROCR::prediction(psi_avg, y_true_latent)
    perf_latent <- ROCR::performance(pred_rocr_latent, "tpr", "fpr")
    
    # Store results in reactiveValues
    
    auc_modavg_data$auc_detection <- as.numeric(roc_obj_detection$auc)
    auc_modavg_data$auc_latent <- as.numeric(roc_obj_latent$auc)
    auc_modavg_data$ci_detection <- as.numeric(roc_obj_detection$ci)
    auc_modavg_data$ci_latent <- as.numeric(roc_obj_latent$ci)
    
    auc_modavg_data$plot_data_detection <- data.frame(
      fpr = perf_detection@x.values[[1]],
      tpr = perf_detection@y.values[[1]],
      method = "Observed detections"
    )
    
    auc_modavg_data$plot_data_latent <- data.frame(
      fpr = perf_latent@x.values[[1]],
      tpr = perf_latent@y.values[[1]],
      method = "Latent occupancy"
    )
    
    auc_modavg_data$n_models <- length(selected_models)
    auc_modavg_data$delta_threshold <- delta_threshold
    
    removeNotification(id = "auc_modavg_calc")
    showNotification(
      paste("Model-averaged AUC calculated using", length(selected_models), "models!"),
      type = "message",
      duration = 3
    )
  })
  
  # AUC summary
  output$auc_text_modavg_dual <- renderUI({
    req(auc_modavg_data$auc_detection)
    req(auc_modavg_data$auc_latent)
    
    ci_det <- auc_modavg_data$ci_detection
    ci_lat <- auc_modavg_data$ci_latent
    
    tags$div(
      tags$h4("Model-averaged AUC Results"),
      tags$p(
        sprintf(
          "Number of models included: %d (Delta AIC ≤ %.1f)",
          auc_modavg_data$n_models,
          auc_modavg_data$delta_threshold
        )
      ),
      tags$hr(),
      tags$div(
        style = "display: flex; justify-content: space-around;",
        tags$div(
          style = "text-align: center; padding: 10px; border: 2px solid #2196F3; border-radius: 5px; background-color: #E3F2FD;",
          tags$h5(style = "color: #1976D2;", "Using observed detections"),
          tags$p(
            style = "font-size: 18px; font-weight: bold; color: #0D47A1;",
            sprintf("AUC = %.3f", ci_det[2])
          ),
          tags$p(
            style = "font-size: 14px; color: #1565C0;",
            sprintf("95%% CI: %.3f – %.3f", ci_det[1], ci_det[3])
          ),
          tags$p(
            style = "font-size: 12px; color: #424242;",
            "Conservative estimate based on actual observations"
          )
        ),
        tags$div(
          style = "text-align: center; padding: 10px; border: 2px solid #F44336; border-radius: 5px; background-color: #FFEBEE;",
          tags$h5(style = "color: #C62828;", "Using latent occupancy"),
          tags$p(
            style = "font-size: 18px; font-weight: bold; color: #B71C1C;",
            sprintf("AUC = %.3f", ci_lat[2])
          ),
          tags$p(
            style = "font-size: 14px; color: #D32F2F;",
            sprintf("95%% CI: %.3f – %.3f", ci_lat[1], ci_lat[3])
          ),
          tags$p(
            style = "font-size: 12px; color: #424242;",
            "Detection-corrected estimate accounting for missed detections"
          )
        )
      )
    )
  })
  
  # Create plot
  output$auc_plot_modavg_dual <- renderPlotly({
    req(auc_modavg_data$plot_data_detection)
    req(auc_modavg_data$plot_data_latent)
    
    plot_data_combined <- rbind(
      auc_modavg_data$plot_data_detection,
      auc_modavg_data$plot_data_latent
    )
    
    auc_det <- auc_modavg_data$auc_detection
    auc_lat <- auc_modavg_data$auc_latent
    ci_det <- auc_modavg_data$ci_detection
    ci_lat <- auc_modavg_data$ci_latent
    
    anno_text_det <- sprintf(
      "Observed detections\nAUC = %.3f\n95%% CI: %.3f–%.3f",
      ci_det[2], ci_det[1], ci_det[3]
    )
    
    anno_text_lat <- sprintf(
      "Latent occupancy\nAUC = %.3f\n95%% CI: %.3f–%.3f",
      ci_lat[2], ci_lat[1], ci_lat[3]
    )
    
    # Create plot
    roc_plot <- ggplot(plot_data_combined, aes(x = fpr, y = tpr, color = method)) +
      geom_line(linewidth = 1.5) +
      geom_abline(linetype = "dashed", color = "grey50") +
      scale_color_manual(
        values = c(
          "Observed detections" = "#0072b2",
          "Latent occupancy" = "#cc3333"
        ),
        name = ""
      ) +
      annotate("text",
               x = 0.70, y = 0.35,
               label = anno_text_det,
               size = 3.5, hjust = 0,
               color = "#1565C0"
      ) +
      annotate("text",
               x = 0.70, y = 0.15,
               label = anno_text_lat,
               size = 3.5, hjust = 0,
               color = "#C62828"
      ) +
      labs(
        title = sprintf(
          "Model-averaged ROC Curves (Δ AIC ≤ %.1f, n = %d models)",
          auc_modavg_data$delta_threshold,
          auc_modavg_data$n_models
        ),
        x = "1 - Specificity (False Positive Rate)",
        y = "Sensitivity (True Positive Rate)"
      ) +
      theme_bw(base_size = 14) +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold")
      )
    
    ggplotly(roc_plot) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.35
        ),
        margin = list(b = 100)
      )
  })
  

  # Create threat map
  
  map_b <- reactive({
    req(map())
    
    sf::st_buffer(
      map(),
      units::set_units(0, m)
    )
  })
  
  # Create grid centers for mapping (for extrapolation area)
  grid_c_map_df <- reactive({
    req(map_b())
    
    current_crs <- sf::st_crs(map_b())$proj4string
    
    # Calculate area
    area_km2 <- units::set_units(sf::st_area(map_b()), "km^2") |> as.numeric()
    
    # Choose cell size based on area & CRS
    cell_size <- if (area_km2 < 1000) {
      if (identical(current_crs, "+proj=longlat +datum=WGS84 +no_defs")) {
        0.01
      } else {
        1000
      }
    } else {
      if (identical(current_crs, "+proj=longlat +datum=WGS84 +no_defs")) {
        0.02
      } else {
        2000
      }
    }
    
    t_grid <- map_b() |>
      sf::st_make_grid(
        what = "centers",
        cellsize = cell_size,
        crs = current_crs
      ) |>
      sf::st_intersection(map_b())
    
    t_grid <- sf::st_coordinates(t_grid)
    t_grid_df <- as.data.frame(t_grid)
    
    return(t_grid_df)
  })
  
  grid_map_id <- reactive({
    req(map_b())
    
    current_crs <- sf::st_crs(map_b())$proj4string
    area_km2 <- units::set_units(sf::st_area(map_b()), "km^2") |> as.numeric()
    cell_size <- if (area_km2 < 1000) {
      if (identical(current_crs, "+proj=longlat +datum=WGS84 +no_defs")) {
        0.01
      } else {
        1000
      }
    } else {
      if (identical(current_crs, "+proj=longlat +datum=WGS84 +no_defs")) {
        0.02
      } else {
        2000
      }
    }
    
    t_grid <- map_b() |>
      sf::st_make_grid(
        what = "polygons",
        cellsize = cell_size,
        crs = current_crs
      ) |>
      sf::st_intersection(map_b())
    
    t_grid_id <- sf::st_sf(
      idcell = seq_along(t_grid),
      geom = t_grid
    ) |>
      sf::st_cast("POLYGON")
    
    return(t_grid_id)
  })
  
  xy_sf_map <- reactive({
    req(grid_map_id(), grid_c_map_df())
    
    sf::st_as_sf(
      grid_c_map_df(),
      coords = c("X", "Y"),
      crs = sf::st_crs(grid_map_id())
    )
  })
  
  result_sf_map <- reactive({
    req(xy_sf_map(), grid_map_id())
    
    sf::st_join(
      xy_sf_map(),
      grid_map_id()
    )
  })
  
  result_df_map <- reactive({
    req(result_sf_map())
    
    setNames(
      data.frame(
        result_sf_map()[[1]],
        matrix(
          unlist(result_sf_map()[2]),
          ncol = 2,
          byrow = TRUE
        )
      ),
      c("idcell", "X", "Y")
    )
  })
  
  # vector object to extract raster values
  vect_pt_map <- reactive({
    req(result_df_map(), grid_map_id())
    
    xy <- data.frame(
      idcell = result_df_map()$idcell,
      lon = result_df_map()$X,
      lat = result_df_map()$Y
    )
    
    p <- terra::vect(
      x = xy,
      crs = sf::st_crs(x = grid_map_id())$proj4string
    )
    
    return(p)
  })
  
  # Convert grid to WGS84
  grid_map_id_wgs <- reactive({
    req(grid_map_id(), grid_wgs())
    
    sf::st_transform(
      grid_map_id(),
      crs = sf::st_crs(grid_wgs())
    )
  })
  
  # Individual model threat map
  
  # Update dropdown when models change
  observeEvent(model_storage$model_names, {
    updateSelectInput(session, "threat_map_model_select",
                      choices = c("", model_storage$model_names),
                      selected = ""
    )
  })
  
  threat_map_raster <- reactive({
    has_env_data <- !is.null(env_data())
    has_uploaded_rasters <- length(raster_data$rasters) > 0
    has_harmonized <- !is.null(raster_data$stacked_raster)
    
    # If no raster data at all, return NULL
    if (!has_env_data && !has_uploaded_rasters && !has_harmonized) {
      return(NULL)
    }
    
    # Determine which raster to use 
    raster_to_use <- NULL
    
    # Harmonised stack (uploaded + env data combined)
    if (has_harmonized) {
      raster_to_use <- raster_data$stacked_raster
      cat("Threat map using harmonized stack (", terra::nlyr(raster_to_use), " layers)\n")
    }
    # Environmental data only (if no uploaded rasters)
    else if (has_env_data && !has_uploaded_rasters) {
      raster_to_use <- env_data()
      cat("Threat map using environmental data only (", terra::nlyr(raster_to_use), " layers)\n")
    }
    # Uploaded rasters only
    else if (has_uploaded_rasters) {
      raster_to_use <- do.call(c, raster_data$rasters)
      cat("Threat map using uploaded rasters only (", terra::nlyr(raster_to_use), " layers)\n")
    }
    
    return(raster_to_use)
  })
  
  # Extract raster values for SELECTED model
  rast_val_pred_individual <- reactive({
    req(input$threat_map_model_select, input$threat_map_model_select != "")
    req(threat_map_raster(), vect_pt_map(), grid_c_map_df())
    
    # Get SELECTED model from dropdown
    model_name <- input$threat_map_model_select
    selected_model <- model_storage$models[[model_name]]
    
    if (is.null(selected_model)) {
      return(NULL)
    }
    
    # Check for null model
    formula_text <- as.character(selected_model@formula)[3]
    if (grepl("^1$|^\\.$", formula_text)) {
      showNotification(
        "Null model has no spatial covariates. Cannot generate spatial map.",
        type = "warning",
        duration = 5
      )
      return(NULL)
    }
    
    # Extract variable names from SELECTED model
    variable_names <- names(selected_model@estimates@estimates$state@estimates)[-1]
    
    # Subset the stacked raster based on the SELECTED model's variables
    selected_rasters <- threat_map_raster()[[variable_names]]
    
    # Extract raster values
    extract_values <- terra::extract(
      x = selected_rasters,
      y = vect_pt_map()
    )
    
    # Merge with the original data frame
    extr_val <- cbind(grid_c_map_df(), extract_values)
    
    return(extr_val)
  })
  
  # Filter variables from rast_val_pred based on SELECTED model variables
  filtered_data_individual <- reactive({
    req(rast_val_pred_individual(), input$threat_map_model_select)
    
    # Get SELECTED model
    model_name <- input$threat_map_model_select
    selected_model <- model_storage$models[[model_name]]
    
    if (is.null(selected_model)) {
      return(NULL)
    }
    
    # Extract occupancy variables that appear in the SELECTED model
    model_vars <- names(selected_model@estimates@estimates$state@estimates)[-1]
    
    # From the rast_val_pred, we only pick variables that appear in our SELECTED model
    filtered <- rast_val_pred_individual()[, c("X", "Y", model_vars)]
    
    return(filtered)
  })
  
  # Generate rasters
  generate_rasters_individual <- reactive({
    req(filtered_data_individual())
    
    variable_names <- names(filtered_data_individual())[3:length(names(filtered_data_individual()))]
    rasters_list <- lapply(variable_names, function(var_name) {
      data <- cbind(
        filtered_data_individual()$X,
        filtered_data_individual()$Y,
        filtered_data_individual()[[var_name]]
      )
      raster_object <- terra::rast(data, type = "xyz")
      names(raster_object) <- var_name 
      
      return(raster_object)
    })
    names(rasters_list) <- variable_names
    
    return(rasters_list)
  })
  
  # Standardise rasters using SELECTED model's site covariates
  standardize_rasters_individual <- reactive({
    req(generate_rasters_individual(), filtered_data_individual(), comb_fin())
    req(input$threat_map_model_select)
    
    # Get SELECTED model
    model_name <- input$threat_map_model_select
    selected_model <- model_storage$models[[model_name]]
    
    if (is.null(selected_model)) {
      return(NULL)
    }
    
    rasters <- generate_rasters_individual()
    filtered <- filtered_data_individual()
    
    # Use SELECTED model's site covariates
    site <- comb_fin()
    
    standardized_list <- lapply(seq_along(rasters), function(i) {
      var_name <- names(filtered)[2 + i]
      var_data <- as.numeric(site[[var_name]])
      
      if (anyNA(var_data)) {
        warning("NA values present in the covariate data for variable ", var_name)
      }
      
      var_mean <- mean(var_data, na.rm = TRUE)
      var_sd <- sd(var_data, na.rm = TRUE)
      
      standardized_data <- (rasters[[i]] - var_mean) / var_sd
      names(standardized_data) <- var_name
      
      return(standardized_data)
    })
    
    return(standardized_list)
  })
  
  # Stack rasters
  stacked_raster_individual <- reactive({
    req(standardize_rasters_individual())
    
    standardized_list <- standardize_rasters_individual()
    stacked_raster <- do.call(c, standardized_list)
    
    return(stacked_raster)
  })
  
  # Make prediction with SELECTED model
  psi_pred_extrapolate_individual <- reactive({
    req(stacked_raster_individual(), input$threat_map_model_select)
    
    # Get SELECTED model
    model_name <- input$threat_map_model_select
    selected_model <- model_storage$models[[model_name]]
    
    if (is.null(selected_model)) {
      return(NULL)
    }
    
    # Show progress
    showNotification(
      paste("Generating map for:", model_name),
      type = "message",
      duration = 2,
      id = "indiv_map_gen"
    )
    
    # Predict using SELECTED model
    predictions <- tryCatch(
      {
        unmarked::predict(
          selected_model,
          newdata = stacked_raster_individual(),
          type = "state"
        )
      },
      error = function(e) {
        removeNotification(id = "indiv_map_gen")
        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
    )
    
    removeNotification(id = "indiv_map_gen")
    
    if (!is.null(predictions)) {
      showNotification(
        "Map generated!",
        type = "message",
        duration = 2
      )
      
      cat("\n=== Individual map generated ===\n")
      cat("Model:", model_name, "\n")
      cat(
        "Prediction range:",
        round(min(predictions$Predicted[], na.rm = TRUE), 3), "-",
        round(max(predictions$Predicted[], na.rm = TRUE), 3), "\n"
      )
      cat("================================\n\n")
    }
    
    return(predictions)
  })
  
  # Assign CRS
  psi_pred_extrapolate_crs_individual <- reactive({
    req(psi_pred_extrapolate_individual(), map_b())
    
    p_crs <- psi_pred_extrapolate_individual()
    terra::crs(p_crs) <- sf::st_crs(map_b())$proj4string
    
    return(p_crs)
  })
  
  # Project to WGS84
  psi_pred_extrapolate_wgs_individual <- reactive({
    req(psi_pred_extrapolate_crs_individual(), grid_wgs())
    
    terra::project(
      psi_pred_extrapolate_crs_individual(),
      sf::st_crs(grid_wgs())$proj4string # or EPSG:4326
    )
  })
  
  # Extract raster values to grid polygons for hover display
  selected_raster_individual <- reactive({
    req(psi_pred_extrapolate_wgs_individual())
    
    layer <- input$variable1
    
    switch(layer,
           "Predicted" = psi_pred_extrapolate_wgs_individual()$Predicted,
           "SE" = psi_pred_extrapolate_wgs_individual()$SE,
           "lower" = psi_pred_extrapolate_wgs_individual()$lower,
           "upper" = psi_pred_extrapolate_wgs_individual()$upper,
           psi_pred_extrapolate_wgs_individual()$Predicted # default
    )
  })
  
  # Render leaflet map for INDIVIDUAL model (polygon-based with hover)
  # Extract predicted psi values to grids using centre coordinates
  psi_val_to_pts <- reactive({
    req(psi_pred_extrapolate_crs_individual(), vect_pt_map(), result_df_map())
    
    ext_pts <- terra::extract(
      x = psi_pred_extrapolate_crs_individual(),
      y = vect_pt_map()
    )
    
    all_df <- cbind(result_df_map(), ext_pts[, -1])
    
    return(all_df)
  })
  
  # Convert grid with id to WGS84
  grid_map_id_wgs <- reactive({
    req(grid_map_id(), grid_wgs())
    
    sf::st_transform(
      grid_map_id(),
      crs = sf::st_crs(grid_wgs())
    )
  })
  
  # Transfer psi values from data frame to grid polygons
  grid_psi_vals <- reactive({
    req(grid_map_id(), psi_val_to_pts())
    
    merge(
      grid_map_id(),
      psi_val_to_pts(),
      by = "idcell",
      all.x = TRUE
    )
  })
  
  # Clip larger shapefile by smaller shapefil
  clip_psi <- reactive({
    req(grid_psi_vals(), map())
    
    sf::st_intersection(
      grid_psi_vals(),
      map()
    )
  })
  
  # Reproject to WGS84 for leaflet
  clip_psi_wgs <- reactive({
    req(clip_psi())
    
    sf::st_transform(
      clip_psi(),
      crs = 4326
    )
  })
  
  # Make leaflet map
  output$map_threat_individual <- renderLeaflet({
    
    psi_pred_final <- clip_psi_wgs()
    
    t_map <- leaflet() |>
      addTiles()
    
    # Color palette
    col_pal <- colorNumeric(
      palette = c(
        "#ffffcc", "#ffeda0", "#fed976",
        "#feb24c", "#fd8d3c", "#fc4e2a",
        "#e31a1c", "#bd0026", "#800026"
      ),
      domain = c(0, 1),  # Fixed scale
      na.color = "transparent"
    )
    
    # Label text
    map_lab <- paste(
      "Grid",
      psi_pred_final$idcell,
      "=",
      round(psi_pred_final[[input$variable1]], 2)
    )
    
    t_map |>
      addPolygons(
        data = psi_pred_final,
        fillColor = col_pal(psi_pred_final[[input$variable1]]),
        fillOpacity = .7,
        color = "grey",
        weight = 1,
        label = map_lab,
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = col_pal,
        values = c(0, 1),
        position = "bottomleft",
        title = "Probability of threat"
      ) |>
      addControl(
        html = paste0(
          '<div id="map_title" style="background: white; padding: 5px; border-radius: 3px;">',
          '<b>Probability of Threat Map</b><br>',
          '</div>'
        ),
        position = "topright"
      )
  })
  
  # Download handler
  output$download_individual_threat_map <- downloadHandler(
    filename = function() {
      model_name <- gsub(" ", "_", input$threat_map_model_select)
      var_type <- input$variable1
      paste0("threat_map_", model_name, "_", var_type, "_", Sys.Date(), ".tif")
    },
    content = function(file) {
      req(selected_raster_individual())
      terra::writeRaster(selected_raster_individual(), file, filetype = "GTiff", overwrite = TRUE)
    },
    contentType = "image/tiff"
  )
  
  # Model-averaged threat mapping
  
  # Store model-averaged data
  modavg_map_data <- reactiveValues(
    predictions_wgs = NULL,
    n_models = NULL,
    delta_threshold = NULL,
    model_names = NULL,
    ready = FALSE
  )
  
  # Make ready status available to UI
  output$modavg_map_ready <- reactive({
    modavg_map_data$ready
  })
  outputOptions(output, "modavg_map_ready", suspendWhenHidden = FALSE)
  
  # Helper function to build model matrix terms
  build_model_matrix_terms <- function(formula_list, base_data) {
    all_terms <- list()
    
    for (mod in formula_list) {
      form <- formula(mod@formula)
      psi_rhs <- as.formula(paste("~", as.character(form)[3]))
      mm <- model.matrix(psi_rhs, base_data)
      all_terms[[length(all_terms) + 1]] <- colnames(mm)
    }
    
    final_terms <- unique(unlist(all_terms))
    final_terms <- setdiff(final_terms, "(Intercept)")
    
    for (term in final_terms) {
      if (!(term %in% names(base_data))) {
        tryCatch(
          {
            base_data[[term]] <- eval(parse(text = term), envir = base_data)
          },
          error = function(e) {
            warning(sprintf("Could not create term '%s': %s", term, e$message))
            cat(sprintf("WARNING: Failed to create term '%s'\n", term))
          }
        )
      }
    }
    
    return(base_data)
  }
  
  # Generate model-averaged map
  observeEvent(input$generate_modavg_map_btn, {

    if (is.null(model_storage$model_selection)) {
      showNotification(
        "Please rank models first!",
        type = "error",
        duration = 5
      )
      return()
    }
    
    if (is.null(threat_map_raster())) {
      showNotification(
        "No raster data available!",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    showNotification(
      "Generating model-averaged map... This may take some time.",
      type = "message",
      duration = NULL,
      id = "modavg_map_progress"
    )
    
    tryCatch(
      {
        ms_obj <- model_storage$model_selection
        df_ms <- as.data.frame(ms_obj@Full)
        delta_threshold <- input$delta_aic_threshold
        
        top_indices <- which(df_ms$delta <= delta_threshold)
        
        if (length(top_indices) == 0) {
          showNotification(
            paste("No models within Delta ≤", delta_threshold),
            type = "warning",
            duration = 5
          )
          removeNotification(id = "modavg_map_progress")
          return()
        }
        
        all_model_names <- names(model_storage$models)
        selected_model_names <- all_model_names[top_indices]
        selected_models <- model_storage$models[selected_model_names]
        
        # Filter out null model
        has_spatial <- sapply(selected_models, function(m) {
          formula_text <- as.character(m@formula)[3]
          !grepl("^1$|^\\.$", formula_text)
        })
        
        if (!any(has_spatial)) {
          showNotification(
            "No models with spatial covariates!",
            type = "warning",
            duration = 5
          )
          removeNotification(id = "modavg_map_progress")
          return()
        }
        
        if (!all(has_spatial)) {
          cat("Removing", sum(!has_spatial), "null model(s)\n")
          selected_models <- selected_models[has_spatial]
          selected_model_names <- selected_model_names[has_spatial]
        }
        
        # Get all unique variables across SELECTED models
        all_vars <- unique(unlist(lapply(selected_models, function(m) {
          names(m@estimates@estimates$state@estimates)[-1]
        })))
        
        # Get available raster variables
        available_vars <- names(threat_map_raster())
        use_vars <- intersect(all_vars, available_vars)
        
        if (length(use_vars) == 0) {
          stop("No matching raster variables found.")
        }
 
        # Get raster stack & reference data
        r_stack <- threat_map_raster()[[use_vars]]
        df <- comb_fin()
        
        # Match variables present in both rasters & reference data
        vars <- intersect(names(r_stack), names(df))
        
        if (length(vars) == 0) {
          stop("No overlapping variables between rasters and comb_fin().")
        }
        
        missing_vars <- setdiff(all_vars, vars)
        if (length(missing_vars) > 0) {
          warning(sprintf(
            "Some required variables missing from standardisation: %s",
            paste(missing_vars, collapse = ", ")
          ))
          cat("WARNING: Missing variables:", paste(missing_vars, collapse = ", "), "\n")
        }
        
        # Compute mean and sd for standardisation
        means <- sapply(df[vars], mean, na.rm = TRUE)
        sds <- sapply(df[vars], sd, na.rm = TRUE)
        sds[sds == 0] <- NA
        
        # Standardise raster stack
        selected_rasters <- (r_stack[[vars]] - means) / sds
        
        extract_values <- terra::extract(x = selected_rasters, y = vect_pt_map())
        extr_val <- cbind(grid_c_map_df(), extract_values)
       
        # Build model matrix for complex terms
        newdata <- build_model_matrix_terms(selected_models, extr_val[, -c(1:2)])
        
        # Model-averaged prediction using SELECTED models
        mod_avg_pred <- AICcmodavg::modavgPred(
          cand.set = selected_models,
          parm.type = "psi",
          newdata = newdata
        )
        
        # Create prediction rasters
        pred_data_predicted <- cbind(extr_val[, c("X", "Y")], mod_avg_pred$mod.avg.pred)
        pred_data_se <- cbind(extr_val[, c("X", "Y")], mod_avg_pred$uncond.se)
        pred_data_lower <- cbind(extr_val[, c("X", "Y")], mod_avg_pred$lower.CL)
        pred_data_upper <- cbind(extr_val[, c("X", "Y")], mod_avg_pred$upper.CL)
        
        rast_predicted <- terra::rast(pred_data_predicted, type = "xyz")
        rast_se <- terra::rast(pred_data_se, type = "xyz")
        rast_lower <- terra::rast(pred_data_lower, type = "xyz")
        rast_upper <- terra::rast(pred_data_upper, type = "xyz")
        
        names(rast_predicted) <- "Predicted"
        names(rast_se) <- "SE"
        names(rast_lower) <- "lower"
        names(rast_upper) <- "upper"
        
        # Stack
        psi_pred_stack <- c(rast_predicted, rast_se, rast_lower, rast_upper)
        
        # Assign CRS
        terra::crs(psi_pred_stack) <- sf::st_crs(map_b())$proj4string
        
        # Project to WGS84
        psi_pred_wgs <- terra::project(psi_pred_stack, sf::st_crs(grid_wgs())$proj4string)
        
        # Store results
        modavg_map_data$predictions_wgs <- psi_pred_wgs
        modavg_map_data$n_models <- length(selected_models)
        modavg_map_data$delta_threshold <- delta_threshold
        modavg_map_data$model_names <- selected_model_names
        modavg_map_data$ready <- TRUE
        
        removeNotification(id = "modavg_map_progress")
        showNotification(
          paste("Map generated using", length(selected_models), "models!"),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        cat("Error:", e$message, "\n")
        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 5
        )
        removeNotification(id = "modavg_map_progress")
      }
    )
  })
  
  # Map info
  output$modavg_map_info <- renderUI({
    req(modavg_map_data$ready)
    
    tags$div(
      style = "margin-top: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
      tags$h5("Map Information"),
      tags$p(sprintf("Number of models averaged: %d", modavg_map_data$n_models)),
      tags$p(sprintf("Delta AIC threshold: %.1f", modavg_map_data$delta_threshold)),
      tags$details(
        open = TRUE,
        tags$summary("Models included:"),
        tags$ul(
          lapply(modavg_map_data$model_names, function(name) {
            tags$li(name)
          })
        )
      )
    )
  })
  
  # Extract raster values to grid polygons for model-averaged map
  psi_val_to_pts_modavg <- reactive({
    req(modavg_map_data$predictions_wgs, vect_pt_map(), result_df_map())
    
    preds_proj <- terra::project(
      modavg_map_data$predictions_wgs,
      sf::st_crs(grid_map_id())$proj4string
    )
    
    ext_pts <- terra::extract(
      x = preds_proj,
      y = vect_pt_map()
    )
    
    all_df <- cbind(result_df_map(), ext_pts[, -1])
    
    return(all_df)
  })
  
  grid_psi_vals_modavg <- reactive({
    req(grid_map_id(), psi_val_to_pts_modavg())
    
    merge(
      grid_map_id(),
      psi_val_to_pts_modavg(),
      by = "idcell",
      all.x = TRUE
    )
  })
  
  clip_psi_modavg <- reactive({
    req(grid_psi_vals_modavg(), map())
    
    sf::st_intersection(
      grid_psi_vals_modavg(),
      map()
    )
  })
  
  clip_psi_wgs_modavg <- reactive({
    req(clip_psi_modavg())
    
    sf::st_transform(
      clip_psi_modavg(),
      crs = 4326
    )
  })
  
  output$map_threat_modavg <- renderLeaflet({
    req(modavg_map_data$ready, clip_psi_wgs_modavg(), map_b())
    
    map_wgs <- sf::st_transform(map_b(), 4326)
    psi_pred_final <- clip_psi_wgs_modavg()
    
    var_col <- switch(input$variable2,
                      "Predicted" = "Predicted",
                      "SE" = "SE",
                      "lower" = "lower",
                      "upper" = "upper",
                      "Predicted")
    
    var_name <- switch(input$variable2,
                       "Predicted" = "Predicted occupancy",
                       "SE" = "Standard Error",
                       "lower" = "Lower 95% CI",
                       "upper" = "Upper 95% CI",
                       "Predicted occupancy")
    
    col_pal <- colorNumeric(
      palette = c(
        "#ffffcc", "#ffeda0", "#fed976",
        "#feb24c", "#fd8d3c", "#fc4e2a",
        "#e31a1c", "#bd0026", "#800026"
      ),
      domain = c(0, 1), 
      na.color = "transparent"
    )
    
    map_lab <- paste(
      "Grid",
      psi_pred_final$idcell,
      "=",
      round(psi_pred_final[[var_col]], 3)
    )
    
    leaflet() %>%
      setView(
        lng = mean(sf::st_bbox(psi_pred_final)[c("xmin", "xmax")]),
        lat = mean(sf::st_bbox(psi_pred_final)[c("ymin", "ymax")]),
        zoom = 10
      ) %>%
      addTiles() %>%
      addPolygons(
        data = psi_pred_final,
        fillColor = ~col_pal(psi_pred_final[[var_col]]),
        fillOpacity = 0.7,
        color = "grey",
        weight = 1,
        label = map_lab,
        labelOptions = labelOptions(textsize = "12px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = col_pal,
        values = c(0, 1),
        title = "Probability of threat"
      ) %>%
      addControl(
        html = paste0(
          '<div id="map_title" style="background: white; padding: 5px; border-radius: 3px;">',
          '<b>Model-averaged Threat Map</b><br>',
          modavg_map_data$n_models, ' models<br>',
          var_name, '</div>'
        ),
        position = "topright"
      )
  })
  
  output$download_modavg_map_data <- downloadHandler(
    filename = function() {
      paste0("model_averaged_threat_map_", Sys.Date(), ".tif")
    },
    content = function(file) {
      terra::writeRaster(modavg_map_data$predictions_wgs, file, overwrite = TRUE)
    }
  )
  
  # Plot download handlers
  # Download GoF bootstrap plot
  output$download_gof_plot <- downloadHandler(
    filename = function() {
      paste0("gof_bootstrap_", Sys.Date(), ".png")
    },
    content = function(file) {
      gof <- model_storage$global_gof
      
      if (!is.null(gof)) {
        df <- data.frame(chi_sq = gof$chi.square.boot)
        
        p <- ggplot(df, aes(x = chi_sq)) +
          geom_histogram(
            bins = 30,
            fill = "steelblue",
            color = "white"
          ) +
          geom_vline(
            xintercept = gof$chi.square,
            colour = "#cc3333",
            linewidth = 1,
            linetype = "dashed"
          ) +
          labs(
            x = "Chi-square statistic",
            y = "Frequency"
          ) +
          annotate(
            "text",
            x = Inf,
            y = Inf,
            hjust = 1.1,
            vjust = 2,
            label = paste0(
              "Observed (χ² = ", round(gof$chi.square, 2), ")\n",
              "p-value = ", round(gof$p.value, 3)
            ),
            colour = "#cc3333",
            size = 5
          ) +
          theme_minimal() +
          theme(
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
          )
        
        ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
      }
    },
    contentType = "image/png"
  )
  
  # Download individual model AUC plot
  output$download_auc_individual <- downloadHandler(
    filename = function() {
      paste0("auc_individual_", gsub(" ", "_", input$auc_model_select), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(auc_data$plot_data_detection, auc_data$plot_data_latent)
      
      roc_obs <- auc_data$plot_data_detection
      roc_latent <- auc_data$plot_data_latent
      auc_obs <- auc_data$auc_detection
      auc_latent <- auc_data$auc_latent
      
      df <- rbind(roc_obs, roc_latent)
      
      p <- ggplot(df, aes(x = fpr, y = tpr, color = method)) +
        geom_line(linewidth = 1.5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
        scale_color_manual(
          values = c(
            "Observed detections" = "#0072b2",
            "Latent occupancy" = "#cc3333"
          ),
          labels = c(
            paste0("Observed detections (AUC = ", round(auc_obs, 3), ")"),
            paste0("Latent occupancy (AUC = ", round(auc_latent, 3), ")")
          )
        ) +
        labs(
          title = paste("ROC Curves:", auc_data$model_name),
          x = "1 - Specificity (False Positive Rate)",
          y = "Sensitivity (True Positive Rate)",
          color = ""
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")
        )
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download model-averaged AUC plot
  output$download_auc_modavg <- downloadHandler(
    filename = function() {
      paste0("auc_model_averaged_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(auc_modavg_data$plot_data_detection, auc_modavg_data$plot_data_latent)
      
      roc_obs <- auc_modavg_data$plot_data_detection
      roc_latent <- auc_modavg_data$plot_data_latent
      auc_obs <- auc_modavg_data$auc_detection
      auc_latent <- auc_modavg_data$auc_latent
      
      df <- rbind(roc_obs, roc_latent)
      
      p <- ggplot(df, aes(x = fpr, y = tpr, color = method)) +
        geom_line(linewidth = 1.5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
        scale_color_manual(
          values = c(
            "Observed detections" = "#0072b2",
            "Latent occupancy" = "#cc3333"
          ),
          labels = c(
            paste0("Observed detections (AUC = ", round(auc_obs, 3), ")"),
            paste0("Latent occupancy (AUC = ", round(auc_latent, 3), ")")
          )
        ) +
        labs(
          title = paste(
            "Model-Averaged ROC Curves (",
            auc_modavg_data$n_models,
            "models, Δ AIC ≤",
            auc_modavg_data$delta_threshold,
            ")"
          ),
          x = "1 - Specificity (False Positive Rate)",
          y = "Sensitivity (True Positive Rate)",
          color = ""
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")
        )
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download individual model occupancy plot
  output$download_individual_occ <- downloadHandler(
    filename = function() {
      var_name <- gsub(" ", "_", tolower(input$individual_occ_var))
      paste0("occupancy_vs_", var_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(occ_plot_predictions(), input$individual_occ_var)
      
      pred <- as.data.frame(occ_plot_predictions())
      selected_var <- input$individual_occ_var
      
      plot_data <- data.frame(
        x = pred[[selected_var]],
        Predicted = pred$Predicted,
        lower = pred$lower,
        upper = pred$upper
      )
      
      var_label <- gsub("_", " ", selected_var)
      var_label <- tools::toTitleCase(var_label)
      
      p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#56b4e9") +
        geom_line(color = "#0072b2", linewidth = 1.2) +
        labs(
          x = var_label,
          y = "Occupancy probability",
          title = paste("Occupancy vs", var_label)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        ylim(0, 1)
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download individual model detection plot
  output$download_individual_det <- downloadHandler(
    filename = function() {
      var_name <- gsub(" ", "_", tolower(input$individual_det_var))
      paste0("detection_vs_", var_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(det_plot_predictions(), input$individual_det_var)
      
      pred <- as.data.frame(det_plot_predictions())
      selected_var <- input$individual_det_var
      
      plot_data <- data.frame(
        x = pred[[selected_var]],
        Predicted = pred$Predicted,
        lower = pred$lower,
        upper = pred$upper
      )
      
      var_label <- gsub("_", " ", selected_var)
      var_label <- tools::toTitleCase(var_label)
      
      p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#e69f00") +
        geom_line(color = "#d55e00", linewidth = 1.2) +
        labs(
          x = var_label,
          y = "Detection probability",
          title = paste("Detection vs", var_label)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        ylim(0, 1)
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download model-averaged occupancy plot
  output$download_modavg_occ <- downloadHandler(
    filename = function() {
      var_name <- gsub(" ", "_", tolower(input$modavg_occ_var))
      paste0("modavg_occupancy_vs_", var_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(modavg_occ_predictions(), input$modavg_occ_var)
      
      pred <- as.data.frame(modavg_occ_predictions())
      selected_var <- input$modavg_occ_var
      
      plot_data <- data.frame(
        x = pred[[selected_var]],
        Predicted = pred$Predicted,
        lower = pred$lower,
        upper = pred$upper
      )
      
      var_label <- gsub("_", " ", selected_var)
      var_label <- tools::toTitleCase(var_label)
      
      p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#56b4e9") +
        geom_line(color = "#0072b2", linewidth = 1.2) +
        labs(
          x = var_label,
          y = "Occupancy probability",
          title = paste("Model-Averaged Occupancy vs", var_label)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        ylim(0, 1)
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download model-averaged detection plot
  output$download_modavg_det <- downloadHandler(
    filename = function() {
      var_name <- gsub(" ", "_", tolower(input$modavg_det_var))
      paste0("modavg_detection_vs_", var_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(modavg_det_predictions(), input$modavg_det_var)
      
      pred <- as.data.frame(modavg_det_predictions())
      selected_var <- input$modavg_det_var
      
      plot_data <- data.frame(
        x = pred[[selected_var]],
        Predicted = pred$Predicted,
        lower = pred$lower,
        upper = pred$upper
      )
      
      var_label <- gsub("_", " ", selected_var)
      var_label <- tools::toTitleCase(var_label)
      
      p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#e69f00") +
        geom_line(color = "#d55e00", linewidth = 1.2) +
        labs(
          x = var_label,
          y = "Detection probability",
          title = paste("Model-Averaged Detection vs", var_label)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        ylim(0, 1)
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download correlation plot
  output$download_correlation_plot <- downloadHandler(
    filename = function() {
      paste0("correlation_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(site_cov_standardized())
      
      cor_matrix <- round(cor(site_cov_standardized(), use = "complete.obs"), 2)
      
      p <- ggcorrplot::ggcorrplot(
        corr = cor_matrix,
        hc.order = TRUE,
        type = "lower",
        lab = TRUE,
        lab_size = 3
      ) +
        ggplot2::ggtitle("Pearson's Correlation") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          axis.text.y = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        ggplot2::coord_fixed()
      
      ggsave(file, plot = p, width = 10, height = 10, dpi = 300, bg = "white")
    },
    contentType = "image/png"
  )
  
  # Download Model Selection table (CSV)
  output$download_model_selection <- downloadHandler(
    filename = function() {
      paste0("model_selection_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(model_storage$model_selection)
      
      ms_df <- as.data.frame(model_storage$model_selection@Full)
      
      ms_df <- cbind(Model = rownames(ms_df), ms_df)
      
      write.csv(ms_df, file, row.names = FALSE)
    }
  )
  
  # Download Model-averaging table (CSV)
  output$download_model_averaging <- downloadHandler(
    filename = function() {
      paste0("model_averaging_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(model_storage$averaged_model)
      
      write.csv(model_storage$averaged_model$table, file, row.names = FALSE)
    }
  )
  
  # Individual model covariate relationship plots
  
  # Update model selector when models change
  observeEvent(model_storage$model_names, {
    updateSelectInput(session, "covariate_plot_model_select",
                      choices = c("Select a model..." = "", model_storage$model_names))
  })
  
  # Reactive to get selected model
  selected_plot_model <- reactive({
    req(input$covariate_plot_model_select, input$covariate_plot_model_select != "")
    model_storage$models[[input$covariate_plot_model_select]]
  })
  
  # Model summary
  output$selected_model_summary <- renderPrint({
    req(selected_plot_model())
    print(selected_plot_model())
  })
  
  # Extract occupancy variables from selected model
  occ_vars_individual <- reactive({
    req(selected_plot_model())
    
    # Occupancy parameter names
    occ_params <- names(selected_plot_model()@estimates@estimates$state@estimates)
    
    # Remove intercept
    occ_vars <- setdiff(occ_params, "(Intercept)")
    
    return(occ_vars)
  })
  
  # Extract detection variables from selected model
  det_vars_individual <- reactive({
    req(selected_plot_model())
    
    det_params <- names(selected_plot_model()@estimates@estimates$det@estimates)
    
    det_vars <- setdiff(det_params, "(Intercept)")
    
    return(det_vars)
  })
  
  # Update occupancy variable selector
  observeEvent(occ_vars_individual(), {
    vars <- occ_vars_individual()
    
    if (length(vars) == 0) {
      updateSelectInput(session, "individual_occ_var",
                        choices = c("No occupancy covariates" = ""))
    } else {
      updateSelectInput(session, "individual_occ_var",
                        choices = c("Select variable..." = "", vars),
                        selected = "")
    }
  })
  
  # Update detection variable selector
  observeEvent(det_vars_individual(), {
    vars <- det_vars_individual()
    
    if (length(vars) == 0) {
      updateSelectInput(session, "individual_det_var",
                        choices = c("No detection covariates" = ""))
    } else {
      updateSelectInput(session, "individual_det_var",
                        choices = c("Select variable..." = "", vars),
                        selected = "")
    }
  })
  
  # Occupancy vs covariate plot
  
  # Create prediction data for occupancy plot
  occ_plot_newdata <- reactive({
    req(selected_plot_model(), input$individual_occ_var, input$individual_occ_var != "")
    
    selected_var <- input$individual_occ_var
    model <- selected_plot_model()
    
    # Get all occupancy variables (exclude intercept)
    all_occ_vars <- names(model@estimates@estimates$state@estimates)
    all_occ_vars <- setdiff(all_occ_vars, "(Intercept)")
    
    # Get site covariates from the model
    site_covs <- model@data@siteCovs
    
    # Create sequence for the selected variable (on standardised scale)
    if (selected_var %in% names(site_covs)) {
      var_seq <- seq(
        from = min(site_covs[[selected_var]], na.rm = TRUE),
        to = max(site_covs[[selected_var]], na.rm = TRUE),
        length.out = 100
      )
    } else {
      # Variable might be transformed (e.g., I(var^2))
      # Use range from -3 to 3 for standardised variables
      var_seq <- seq(-3, 3, length.out = 100) # should make it dynamic (!!!)
    }
    
    # Create newdata data frame
    newdata <- as.data.frame(matrix(0, nrow = 100, ncol = length(all_occ_vars)))
    names(newdata) <- all_occ_vars
    
    # Set selected variable to the sequence
    newdata[[selected_var]] <- var_seq
    
    # Set other variables to zero (mean for standardized data)
    other_vars <- setdiff(all_occ_vars, selected_var)
    if (length(other_vars) > 0) {
      newdata[, other_vars] <- 0
    }
    
    return(newdata)
  })
  
  # Predict occupancy probabilities
  occ_plot_predictions <- reactive({
    req(selected_plot_model(), occ_plot_newdata())
    
    model <- selected_plot_model()
    newdata <- occ_plot_newdata()
    
    # Predict
    preds <- predict(model, type = "state", newdata = newdata, appendData = TRUE)
    
    return(preds)
  })
  
  # Occupancy plot
  output$individual_occ_plot <- renderPlotly({
    req(occ_plot_predictions(), input$individual_occ_var)
    
    pred <- as.data.frame(occ_plot_predictions())
    selected_var <- input$individual_occ_var
    
    plot_data <- data.frame(
      x = pred[[selected_var]],
      Predicted = pred$Predicted,
      lower = pred$lower,
      upper = pred$upper
    )
    
    var_label <- gsub("_", " ", selected_var)
    var_label <- tools::toTitleCase(var_label)
    
    p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#56b4e9") +
      geom_line(color = "#0072b2", linewidth = 1) +
      labs(
        x = var_label,
        y = "Occupancy probability",
        title = paste("Occupancy vs", var_label)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      ) +
      ylim(0, 1)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Detection vs covaraite plot
  
  # Create prediction data for detection plot
  det_plot_newdata <- reactive({
    req(selected_plot_model(), input$individual_det_var, input$individual_det_var != "")
    
    selected_var <- input$individual_det_var
    model <- selected_plot_model()
    
    all_det_vars <- names(model@estimates@estimates$det@estimates)
    all_det_vars <- setdiff(all_det_vars, "(Intercept)")
    
    obs_covs <- model@data@obsCovs
    
    if (selected_var %in% names(obs_covs)) {
      var_seq <- seq(
        from = min(obs_covs[[selected_var]], na.rm = TRUE),
        to = max(obs_covs[[selected_var]], na.rm = TRUE),
        length.out = 100
      )
    } else {
      var_seq <- seq(-3, 3, length.out = 100)
    }
    
    newdata <- as.data.frame(matrix(0, nrow = 100, ncol = length(all_det_vars)))
    names(newdata) <- all_det_vars
    
    newdata[[selected_var]] <- var_seq
    
    other_vars <- setdiff(all_det_vars, selected_var)
    if (length(other_vars) > 0) {
      newdata[, other_vars] <- 0
    }
    
    return(newdata)
  })
  
  # Predict detection probabilities
  det_plot_predictions <- reactive({
    req(selected_plot_model(), det_plot_newdata())
    
    model <- selected_plot_model()
    newdata <- det_plot_newdata()
    preds <- predict(model, type = "det", newdata = newdata, appendData = TRUE)
    
    return(preds)
  })
  
  # Render detection plot
  output$individual_det_plot <- renderPlotly({
    req(det_plot_predictions(), input$individual_det_var)
    
    pred <- as.data.frame(det_plot_predictions())
    selected_var <- input$individual_det_var
    
    plot_data <- data.frame(
      x = pred[[selected_var]],
      Predicted = pred$Predicted,
      lower = pred$lower,
      upper = pred$upper
    )
    
    var_label <- gsub("_", " ", selected_var)
    var_label <- tools::toTitleCase(var_label)
    
    p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#56b4e9") +
      geom_line(color = "#0072b2", linewidth = 1) +
      labs(
        x = var_label,
        y = "Detection probability",
        title = paste("Detection vs", var_label)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      ) +
      ylim(0, 1)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Model-averaged covariate relationship plots
  
  modavg_plot_data <- reactiveValues(
    fitlist = NULL,
    occ_vars = NULL,
    det_vars = NULL,
    n_models = NULL,
    delta_threshold = NULL,
    ready = FALSE
  )
  
  output$modavg_plots_ready <- reactive({
    modavg_plot_data$ready
  })
  outputOptions(output, "modavg_plots_ready", suspendWhenHidden = FALSE)
  
  observeEvent(input$generate_modavg_plots_btn, {
    
    # Validate model selection exists
    if (is.null(model_storage$model_selection)) {
      showNotification(
        "Please rank models first using 'Rank Models' button!",
        type = "error",
        duration = 5
      )
      return()
    }
    
    showNotification(
      "Generating model-averaged plots... This may take 1-2 minutes.",
      type = "message",
      duration = NULL,
      id = "modavg_plots_progress"
    )
    
    tryCatch({
      ms_obj <- model_storage$model_selection
      df_ms <- as.data.frame(ms_obj@Full)
      
      # Use delta threshold from Model Selection slider
      delta_threshold <- input$delta_aic_threshold
      
      top_indices <- which(df_ms$delta <= delta_threshold)
      
      if (length(top_indices) == 0) {
        showNotification(
          paste("No models found within Delta (Δ) AIC ≤", delta_threshold),
          type = "warning",
          duration = 5
        )
        removeNotification(id = "modavg_plots_progress")
        return()
      }
      
      all_model_names <- names(model_storage$models)
      selected_model_names <- all_model_names[top_indices]
      selected_models <- model_storage$models[selected_model_names]
      
      has_spatial <- sapply(selected_models, function(m) {
        formula_text <- as.character(m@formula)[3]
        !grepl("^1$|^\\.$", formula_text)
      })
      
      if (!all(has_spatial)) {
        cat("Removing", sum(!has_spatial), "null model(s)\n")
        selected_models <- selected_models[has_spatial]
        selected_model_names <- selected_model_names[has_spatial]
      }
      
      if (length(selected_models) == 0) {
        showNotification(
          "No models with covariates within threshold!",
          type = "warning",
          duration = 5
        )
        removeNotification(id = "modavg_plots_progress")
        return()
      }
      
      fitlist <- methods::new("unmarkedFitList", fits = selected_models)
      
      all_occ_vars <- unique(unlist(lapply(selected_models, function(m) {
        params <- names(m@estimates@estimates$state@estimates)
        setdiff(params, "(Intercept)")
      })))
      
      all_det_vars <- unique(unlist(lapply(selected_models, function(m) {
        params <- names(m@estimates@estimates$det@estimates)
        setdiff(params, "(Intercept)")
      })))
      
      cat("=======================================\n\n")
      
      modavg_plot_data$fitlist <- fitlist
      modavg_plot_data$occ_vars <- all_occ_vars
      modavg_plot_data$det_vars <- all_det_vars
      modavg_plot_data$n_models <- length(selected_models)
      modavg_plot_data$delta_threshold <- delta_threshold
      modavg_plot_data$ready <- TRUE
      
      removeNotification(id = "modavg_plots_progress")
      showNotification(
        paste("Model-averaged plots ready! Using", length(selected_models), "models."),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      cat("Error in model-averaged plots:\n")
      cat(e$message, "\n")
      showNotification(
        paste("Error generating plots:", e$message),
        type = "error",
        duration = 10
      )
      removeNotification(id = "modavg_plots_progress")
    })
  })
  
  output$modavg_plots_info <- renderPrint({
    req(modavg_plot_data$ready)
    
    cat("Number of models averaged:", modavg_plot_data$n_models, "\n")
    cat("Delta (Δ) AIC threshold:", modavg_plot_data$delta_threshold, "\n\n")
    
    cat("Occupancy variables:\n")
    if (length(modavg_plot_data$occ_vars) > 0) {
      cat(paste("-", modavg_plot_data$occ_vars, collapse = "\n"), "\n")
    } else {
      cat("(none)\n")
    }
    
    cat("\nDetection variables:\n")
    if (length(modavg_plot_data$det_vars) > 0) {
      cat(paste("-", modavg_plot_data$det_vars, collapse = "\n"), "\n")
    } else {
      cat("(none)\n")
    }
  })
  
  # Update occupancy variable dropdown
  observeEvent(modavg_plot_data$ready, {
    req(modavg_plot_data$ready)
    
    vars <- modavg_plot_data$occ_vars
    
    if (length(vars) == 0) {
      updateSelectInput(session, "modavg_occ_var",
                        choices = c("No occupancy covariates" = ""))
    } else {
      updateSelectInput(session, "modavg_occ_var",
                        choices = c("Select variable..." = "", vars),
                        selected = "")
    }
  })
  
  # Update detection variable dropdown
  observeEvent(modavg_plot_data$ready, {
    req(modavg_plot_data$ready)
    
    vars <- modavg_plot_data$det_vars
    
    if (length(vars) == 0) {
      updateSelectInput(session, "modavg_det_var",
                        choices = c("No detection covariates" = ""))
    } else {
      updateSelectInput(session, "modavg_det_var",
                        choices = c("Select variable..." = "", vars),
                        selected = "")
    }
  })
  
  # Occupancy vs covaraite plots (model-averaged)
  
  # Create prediction data for model-averaged occupancy plot
  modavg_occ_newdata <- reactive({
    req(modavg_plot_data$fitlist, input$modavg_occ_var, input$modavg_occ_var != "")
    
    selected_var <- input$modavg_occ_var
    fitlist <- modavg_plot_data$fitlist
    
    site_covs <- fitlist@fits[[1]]@data@siteCovs
    
    if (!(selected_var %in% names(site_covs))) {
      var_seq <- seq(-3, 3, length.out = 100)
    } else {
      # Use actual min/max from data
      var_seq <- seq(
        from = min(site_covs[[selected_var]], na.rm = TRUE),
        to = max(site_covs[[selected_var]], na.rm = TRUE),
        length.out = 100
      )
    }
    
    # Get all unique occupancy variables across all models in fitlist
    all_occ_vars <- modavg_plot_data$occ_vars
    
    # Create newdata data frame
    newdata <- as.data.frame(matrix(0, nrow = 100, ncol = length(all_occ_vars)))
    names(newdata) <- all_occ_vars
    
    # Set selected variable to the sequence
    newdata[[selected_var]] <- var_seq
    
    return(newdata)
  })
  
  # Predict model-averaged occupancy probabilities
  modavg_occ_predictions <- reactive({
    req(modavg_plot_data$fitlist, modavg_occ_newdata())
    
    fitlist <- modavg_plot_data$fitlist
    newdata <- modavg_occ_newdata()
    
    showNotification(
      "Calculating model-averaged predictions...",
      type = "message",
      duration = 2,
      id = "modavg_occ_pred"
    )
    
    preds <- predict(fitlist, type = "state", newdata = newdata, appendData = TRUE)
    
    removeNotification(id = "modavg_occ_pred")
    
    return(preds)
  })
  
  # Render model-averaged occupancy plot
  output$modavg_occ_plot <- renderPlotly({
    req(modavg_occ_predictions(), input$modavg_occ_var)
    
    pred <- as.data.frame(modavg_occ_predictions())
    selected_var <- input$modavg_occ_var
    
    plot_data <- data.frame(
      x = pred[[selected_var]],
      Predicted = pred$Predicted,
      lower = pred$lower,
      upper = pred$upper
    )
    
    var_label <- gsub("_", " ", selected_var)
    var_label <- tools::toTitleCase(var_label)
    
    p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, fill = "#cc99cc") +
      geom_line(color = "#660066", linewidth = 1.2) +
      labs(
        x = paste(var_label, "(standardised)"),
        y = "Probability of occupancy (threat)",
        title = paste("Model-averaged: Occupancy vs", var_label),
        subtitle = paste("Averaged across", modavg_plot_data$n_models, "models")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      ) +
      ylim(0, 1)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Detection vs covariate plot (model-averaged)
  
  # Create prediction data for model-averaged detection plot
  modavg_det_newdata <- reactive({
    req(modavg_plot_data$fitlist, input$modavg_det_var, input$modavg_det_var != "")
    
    selected_var <- input$modavg_det_var
    fitlist <- modavg_plot_data$fitlist
    
    site_covs <- fitlist@fits[[1]]@data@siteCovs
    obs_covs <- fitlist@fits[[1]]@data@obsCovs
    
    cov_in_obs <- selected_var %in% names(obs_covs)
    cov_in_site <- selected_var %in% names(site_covs)
    
    if (cov_in_obs) {
      var_seq <- seq(
        from = min(obs_covs[[selected_var]], na.rm = TRUE),
        to = max(obs_covs[[selected_var]], na.rm = TRUE),
        length.out = 100
      )
    } else if (cov_in_site) {
      var_seq <- seq(
        from = min(site_covs[[selected_var]], na.rm = TRUE),
        to = max(site_covs[[selected_var]], na.rm = TRUE),
        length.out = 100
      )
    } else {
      var_seq <- seq(-3, 3, length.out = 100)
    }
    
    all_det_vars <- modavg_plot_data$det_vars
    
    if (cov_in_obs) {
      # For observation covariates, replicate across surveys
      n_surveys <- ncol(fitlist@fits[[1]]@data@y)
      
      newdata <- do.call(rbind, lapply(var_seq, function(val) {
        data.frame(matrix(val, nrow = 1, ncol = n_surveys))
      }))
      colnames(newdata) <- rep(selected_var, ncol(newdata))
      
      # Add site covariates set to 0
      for (cov in names(site_covs)) {
        newdata[[cov]] <- 0
      }
      
    } else {
      # For site-level detection covariates
      newdata <- as.data.frame(matrix(0, nrow = 100, ncol = length(all_det_vars)))
      names(newdata) <- all_det_vars
      
      # Set selected variable to sequence
      newdata[[selected_var]] <- var_seq
    }
    
    return(newdata)
  })
  
  # Predict model-averaged detection probabilities
  modavg_det_predictions <- reactive({
    req(modavg_plot_data$fitlist, modavg_det_newdata())
    
    fitlist <- modavg_plot_data$fitlist
    newdata <- modavg_det_newdata()
    
    showNotification(
      "Calculating model-averaged predictions...",
      type = "message",
      duration = 2,
      id = "modavg_det_pred"
    )
    
    preds <- predict(fitlist, type = "det", newdata = newdata, appendData = TRUE)
    
    removeNotification(id = "modavg_det_pred")
    
    return(preds)
  })
  
  # Render model-averaged detection plot
  output$modavg_det_plot <- renderPlotly({
    req(modavg_det_predictions(), input$modavg_det_var)
    
    pred <- as.data.frame(modavg_det_predictions())
    selected_var <- input$modavg_det_var
    
    if (selected_var %in% names(pred)) {
      x_vals <- pred[[selected_var]]
    } else {
      x_vals <- pred[[1]]
    }
    
    plot_data <- data.frame(
      x = x_vals,
      Predicted = pred$Predicted,
      lower = pred$lower,
      upper = pred$upper
    )
    
    var_label <- gsub("_", " ", selected_var)
    var_label <- tools::toTitleCase(var_label)
    
    p <- ggplot(plot_data, aes(x = x, y = Predicted)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, fill = "#cc99cc") +
      geom_line(color = "#660066", linewidth = 1.2) +
      labs(
        x = paste(var_label, "(standardised)"),
        y = "Probability of detection",
        title = paste("Model-averaged: Detection vs", var_label),
        subtitle = paste("Averaged across", modavg_plot_data$n_models, "models")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      ) +
      ylim(0, 1)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  

  # Covariate relationship plot - older version (defunct!!!) 
  
  det_var_names <- reactiveVal(NULL)
  occ_var_names <- reactiveVal(NULL)
  
  observe({
    updateSelectizeInput(
      session,
      "det_var",
      choices = names(det_cov()),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "occ_var",
      choices = names(site_cov())
    )
    # Update reactive values with column names
    det_var_names(names(det_cov()))
    occ_var_names(names(site_cov()))
  })
  
  # Occupancy probability
  # Create a reactive object for new_data based on user selection
  new_data <- reactive({
    req(input$occ_var)
    selected_var <- input$occ_var[1]
    all_vars <- setdiff(
      names(reactive_model()@estimates@estimates$state@estimates),
      "(Intercept)"
    )
    
    # Create a data frame with all columns set to zero
    new_data <- as.data.frame(
      matrix(0, ncol = length(all_vars), nrow = 101)
    )
    
    # Fill in the selected variable with values
    new_data[, selected_var] <- seq(
      from = min(site_cov_standardized()[[selected_var]], na.rm = TRUE),
      to = max(site_cov_standardized()[[selected_var]], na.rm = TRUE),
      length.out = 101
    )
    
    # Set other variables to zero (at their mean for standardised cov)
    other_vars <- setdiff(all_vars, selected_var)
    new_data[, other_vars] <- 0
    
    new_data
  })
  
  # Removing columns with names starting with V
  new_data_df <- reactive({
    req(new_data())
    new_data() |>
      dplyr::select(-starts_with("V"))
  })
  
  # Render the new_data table
  output$new_data_table <- renderDT({
    datatable(new_data_df())
  })
  
  # Create a reactive object for occ_prob
  occ_prob <- reactive({
    req(reactive_model(), new_data_df())
    unmarked::predict(
      reactive_model(),
      type = "state",
      newdata = new_data_df(),
      appendData = TRUE
    )
  })
  
  # Render the occ_prob table
  output$occ_prob_table <- renderDT({
    datatable(occ_prob())
  })
  
  # Create a reactive object for ggplot
  output$occ_plot <- renderPlotly({
    req(input$occ_var)
    pred <- as.data.frame.array(occ_prob())
    
    ggplot_data <- data.frame(
      x = pred[[input$occ_var[1]]],
      Predicted = pred$Predicted,
      SE = pred$SE,
      lower = pred$lower,
      upper = pred$upper
    )
    
    ggplot_data_label <- gsub("_", " ", input$occ_var[1])
    
    psi <- ggplot(
      data = ggplot_data,
      mapping = aes(x = x, y = Predicted)
    ) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = "blue",
        alpha = 0.4
      ) +
      geom_line(color = "darkred", linewidth = 1.2) +
      labs(
        x = ggplot_data_label,
        y = "Probability of threat"
      ) +
      ylim(0, 1) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 15),
        axis.line = element_line(colour = "transparent"),
        axis.title = element_text(size = 17, vjust = 0.8),
        legend.text = element_text(size = 25)
      )
    
    print(ggplotly(psi))
  })
  
  # Detection probability
  # Create a reactive object for pnew_data based on user selection
  pnew_data <- reactive({
    req(input$det_var)
    selected_pvar <- input$det_var[1]
    all_pvars <- setdiff(
      names(reactive_model()@estimates@estimates$det@estimates),
      "(Intercept)"
    )
    
    # Create a data frame with all columns set to zero
    pnew_data <- as.data.frame(
      matrix(0, ncol = length(all_pvars), nrow = 101)
    )
    
    # Fill in the selected variable with values
    pnew_data[, selected_pvar] <- seq(
      from = min(det_cov_standardized()[[selected_pvar]], na.rm = TRUE),
      to = max(det_cov_standardized()[[selected_pvar]], na.rm = TRUE),
      length.out = 101
    )
    
    # Set other variables to zero (at their mean for standardised cov)
    other_pvars <- setdiff(all_pvars, selected_pvar)
    pnew_data[, other_pvars] <- 0
    
    pnew_data
  })
  
  # Removing columns with names starting with V
  pnew_data_df <- reactive({
    req(pnew_data())
    pnew_data() |>
      dplyr::select(-starts_with("V"))
  })
  
  # Render the pnew_data table
  output$pnew_data_table <- renderDT({
    datatable(pnew_data_df())
  })
  
  # Create a reactive object for occ_prob
  det_prob <- reactive({
    req(reactive_model(), pnew_data_df())
    unmarked::predict(
      reactive_model(),
      type = "det",
      newdata = pnew_data_df(),
      appendData = TRUE
    )
  })
  
  # Render the det_prob table
  output$det_prob_table <- renderDT({
    datatable(det_prob())
  })
  
  # Create a reactive object for ggplot
  output$det_plot <- renderPlotly({
    req(input$det_var)
    p_pred <- as.data.frame.array(det_prob())
    
    ggplot_pdata <- data.frame(
      x = p_pred[[input$det_var[1]]],
      Predicted = p_pred$Predicted,
      SE = p_pred$SE,
      lower = p_pred$lower,
      upper = p_pred$upper
    )
    
    ggplot_pdata_label <- gsub("_", " ", input$det_var[1])
    
    p <- ggplot(
      data = ggplot_pdata,
      mapping = aes(x = x, y = Predicted)
    ) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = "blue",
        alpha = 0.4
      ) +
      geom_line(color = "darkred", linewidth = 1.2) +
      labs(
        x = ggplot_pdata_label,
        y = "Probability of detecting threat"
      ) +
      ylim(0, 1) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 15),
        axis.line = element_line(colour = "transparent"),
        axis.title = element_text(size = 17, vjust = 0.8),
        legend.text = element_text(size = 25)
      )
    
    print(ggplotly(p))
  })
} # end