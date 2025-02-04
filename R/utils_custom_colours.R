#' Vector of custom colors for land type classification
#'
#' @examples
#' fill_landtype
#'
#' @export
fill_landtype <- c("building" = "skyblue3",
                   "road" = "azure3",
                   "water" = "cadetblue2",
                   "grass" = "darkolivegreen1",
                   "crop" = "gold1",
                   "bush" = "darkolivegreen3",
                   "tree" = "chartreuse4")

#' Vector of custom colors for equipmenttype type classification
#'
#' @examples
#' colour_equipmenttype
#'
#' @export
colour_equipmenttype <- c("Nedb\u00f8rm\u00e5ler" = "skyblue3",
                          "Fuktm\u00e5ler" = "skyblue1",
                          "Vindm\u00e5ler" = "orchid",
                          "Lufttemperaturm\u00e5ler" = "coral",
                          "Sn\u00f8dybdem\u00e5ler" = "khaki",
                          "Trykkm\u00e5ler" = "red",
                          "Trykk-, temperatur- og fuktm\u00e5ler" = "chartreuse4",
                          "Present-weather-sensor" = "azure3",
                          "Str\u00e5lingsm\u00e5ler" = "gold1",
                          "Nedb\u00f8rindikator" = "cadetblue2",
                          "Flatetemperaturm\u00e5ler" = "chocolate")

#' Colours for scale_fill_gradientn() in \code{"plot_station_horizon_range"}
#'
#' Colour palette is attributed for a set of range values in metre and
#' changes at 100 m corresponding to the default distance to download
#' high resolution DEMs
#'
#' @examples
#' colour_range
#'
#' ## Generated from
#' # c(rev(RColorBrewer::brewer.pal(6, "Paired")),
#'         RColorBrewer::brewer.pal(4, "Purples"))
#'
#' @export
#'
colour_range <- list(colour = c("#E31A1C",
                                "#FB9A99",
                                "#33A02C",
                                "#B2DF8A",
                                "#1F78B4",
                                "#A6CEE3",
                                "#F2F0F7",
                                "#CBC9E2",
                                "#9E9AC8",
                                "#6A51A3"),
                     range = c(0, 5, 10, 30, 50, 70, 90, 100,
                               1000, 5000, 20000))

#' Vector of CORINE land cover colors and types
#'
#' @references \url{https:custom-scripts.sentinel-hub.com/copernicus_services/corine/corine_land_cover/} and
#' \url{https://www.gofcgold.org/sites/default/files/docs/ReportSeries/GOLD_43.pdf}
#'
#' @examples
#' fill_corine
#' # col2rgb(fill_corine)
#'
#' @export
fill_corine <- c(
  "111" = "#e6004d",  #"Continuous urban fabric"
  "112" = "#ff0000",  #"Discontinuous urban fabric"
  "121" = "#cc4df2",  #"Industrial or commercial units"
  "122" = "#cc0000",  #"Road and rail networks and associated land"
  "123" = "#e6cccc",  #"Port areas"
  "124" = "#e6cce6",  #"Airports"
  "131" = "#a600cc",  #"Mineral extraction sites"
  "132" = "#a64d00",  #"Dump sites"
  "133" = "#ff4dff",  #"Construction sites"
  "141" = "#ffa6ff",  #"Green urban areas"
  "142" = "#ffe6ff",  #"Sport and leisure facilities"
  "211" = "#ffffa8",  #"Non-irrigated arable land"
  "212" = "#ffff00",  #"Permanently irrigated land"
  "213" = "#e6e600",  #"Rice fields"
  "221" = "#e68000",  #"Vineyards"
  "222" = "#f2a64d",  #"Fruit trees and berry plantations"
  "223" = "#e6a600",  #"Olive groves"
  "231" = "#e6e64d",  #"Pastures"
  "241" = "#ffe6a6",  #"Annual crops associated with permanent crops"
  "242" = "#ffe64d",  #"Complex cultivation patterns"
  "243" = "#e6cc4d",  #"Land principally occupied by agriculture with
  #                     significant areas of natural vegetation"
  "244" = "#f2cca6",  #"Agro-forestry areas"
  "311" = "#80ff00",  #"Broad-leaved forest"
  "312" = "#00a600",  #"Coniferous forest"
  "313" = "#4dff00",  #"Mixed forest"
  "321" = "#ccf24d",  #"Natural grasslands"
  "322" = "#a6ff80",  #"Moors and heathland"
  "323" = "#a6e64d",  #"Sclerophyllous vegetation"
  "324" = "#a6f200",  #"Transitional woodland-shrub"
  "331" = "#e6e6e6",  #"Beaches, dunes, sands"
  "332" = "#cccccc",  #"Bare rocks"
  "333" = "#ccffcc",  #"Sparsely vegetated areas"
  "334" = "#000000",  #"Burnt areas"
  "335" = "#a6e6cc",  #"Glaciers and perpetual snow"
  "411" = "#a6a6ff",  #"Inland marshes"
  "412" = "#4d4dff",  #"Peat bogs"
  "421" = "#ccccff",  #"Salt marshes"
  "422" = "#e6e6ff",  #"Salines"
  "423" = "#a6a6e6",  #"Intertidal flats"
  "511" = "#00ccf2",  #"Water courses"
  "512" = "#80f2e6",  #"Water bodies"
  "521" = "#00ffa6",  #"Coastal lagoons"
  "522" = "#a6ffe6",  #"Estuaries"
  "523" = "#e6f2ff",  #"Sea and ocean"
  "999" = "#ffffff") #"NODATA"

# row.names <- c(111:112,121:124,131:133,141:142,
#                211:213,221:223,231,241:244,
#                311:313,321:324,331:335,
#                411:412,421:423,
#                511:512,521:523,
#                999)
#
# fill_corine <- c(
#   1, "#e6004d",  111 , "Continuous urban fabric",
#   2, "#ff0000",  112 , "Discontinuous urban fabric",
#   3, "#cc4df2",  121 , "Industrial or commercial units",
#   4, "#cc0000",  122 , "Road and rail networks and associated land",
#   5, "#e6cccc",  123 , "Port areas",
#   6, "#e6cce6",  124 , "Airports",
#   7, "#a600cc",  131 , "Mineral extraction sites",
#   8, "#a64d00",  132 , "Dump sites",
#   9, "#ff4dff",  133 , "Construction sites",
#   10, "#ffa6ff", 141 , "Green urban areas",
#   11, "#ffe6ff", 142 , "Sport and leisure facilities",
#   12, "#ffffa8", 211 , "Non-irrigated arable land",
#   13, "#ffff00", 212 , "Permanently irrigated land",
#   14, "#e6e600", 213 , "Rice fields",
#   15, "#e68000", 221 , "Vineyards",
#   16, "#f2a64d", 222 , "Fruit trees and berry plantations",
#   17, "#e6a600", 223 , "Olive groves",
#   18, "#e6e64d", 231 , "Pastures",
#   19, "#ffe6a6", 241 , "Annual crops associated with permanent crops",
#   20, "#ffe64d", 242 , "Complex cultivation patterns",
#   21, "#e6cc4d", 243 , "Land principally occupied by agriculture with
#                         significant areas of natural vegetation",
#   22, "#f2cca6", 244 , "Agro-forestry areas",
#   23, "#80ff00", 311 , "Broad-leaved forest",
#   24, "#00a600", 312 , "Coniferous forest",
#   25, "#4dff00", 313 , "Mixed forest",
#   26, "#ccf24d", 321 , "Natural grasslands",
#   27, "#a6ff80", 322 , "Moors and heathland",
#   28, "#a6e64d", 323 , "Sclerophyllous vegetation",
#   29, "#a6f200", 324 , "Transitional woodland-shrub",
#   30, "#e6e6e6", 331 , "Beaches, dunes, sands",
#   31, "#cccccc", 332 , "Bare rocks",
#   32, "#ccffcc", 333 , "Sparsely vegetated areas",
#   33, "#000000", 334 , "Burnt areas",
#   34, "#a6e6cc", 335 , "Glaciers and perpetual snow",
#   35, "#a6a6ff", 411 , "Inland marshes",
#   36, "#4d4dff", 412 , "Peat bogs",
#   37, "#ccccff", 421 , "Salt marshes",
#   38, "#e6e6ff", 422 , "Salines",
#   39, "#a6a6e6", 423 , "Intertidal flats",
#   40, "#00ccf2", 511 , "Water courses",
#   41, "#80f2e6", 512 , "Water bodies",
#   42, "#00ffa6", 521 , "Coastal lagoons",
#   43, "#a6ffe6", 522 , "Estuaries",
#   44, "#e6f2ff", 523 , "Sea and ocean",
#   48, "#ffffff", 999 , "NODATA")
# fill_corine <- as.data.frame(t(matrix(fill_corine, nrow = 4 )),
#                              stringsAsFactors = TRUE)
