testthat::test_that("compute horizon works", {

  # Load the station metadata
  centre  <- cbind(260966.8, 6652718)
  colnames(centre) <- c("x", "y")
  stn_test <- terra::vect(centre, "points", crs = "epsg:25833")
  stn_test$id.stationid <- 18700
  stn_test <- sf::st_as_sf(stn_test)

  # Set expected horizon
  horizon_expected <- data.frame(azimuth=c(0, 350, 340, 330, 320, 310, 300,
                                           290, 280, 270, 260, 250, 240, 230,
                                           220, 210, 200, 190, 180, 170, 160,
                                           150, 140, 130, 120, 110, 100, 90,
                                           80, 70, 60, 50, 40, 30, 20, 10),
                                 horizon_height=c(9.9, 2.3, 3.1, 20.9, 12.2,
                                                  32.8, 6.6, 9.1, 10.8, 9.8, 8,
                                                  6.6, 12.6, 14.4, 11.8, 22.7,
                                                  63.3, 63.3, 63.3, 63.3, 63.3,
                                                  63.3, 7.3, 7.9, 12.5, 9.7,
                                                  10.6, 9.1, 7.8, 21.4, 22.8,
                                                  18.3, 18.6, 18.4, 17.7, 9.9)
  )

  # Load a digital elevation model
  dsm   <- download_dem_kartverket(stn_test, name = "dom", dx = 100, resx = 1)

  # Compute the horizon
  horizon_computed <- compute_horizon(centre, dsm)

  # Compare result and expected horizon
  testthat::expect_equal(round(horizon_computed,1), horizon_expected)
}
)
