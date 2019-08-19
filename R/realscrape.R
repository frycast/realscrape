#' scrape_sbc
#'
#' Scrape house sales data from a list of suburbs by council.
#'
#' This function relies on a loop which repeatedly
#' calls \code{\link[realscrape]{scrape_suburb}}.
#'
#' @param sbc A list of suburbs by council
#' in the same format as the example list
#' \code{STA3LM_suburbs_by_council}
#' @param B Number of boards (i.e., pages of ads) to scape per suburb.
#' @param P Number of ad pages per board (determined by inspecting the
#' AU House Prices website).
#'
#' @return
#' A tibble of house sales data.
#'
#' @export
#'
#' @examples
#' View( STA3LM_suburbs_by_council )
#'
#' # Normally we would set P = 12 (12 ad pages per board)
#' data <- scrape_sbc( STA3LM_suburbs_by_council, B = 1, P = 1 )
#' data_clean <- tidyr::drop_na( data )
#'
scrape_sbc <- function( sbc, B = 1, P = 12 ) {

  data <- data.frame()

  for ( council in names( sbc )  ) {

    for ( suburb in sbc[[ council ]] ) {

      sub_data <- scrape_suburb( suburb = suburb[1],
                                 postcode = suburb[2],
                                 B = B, P = P )
      sub_data$suburb   <- suburb[1]
      sub_data$postcode <- suburb[2]
      sub_data$council  <- council

      data <- rbind(data, sub_data)
    }
  }

  return(data)
}



#' scrape_suburb
#'
#' Scrape house sales data from a suburb.
#'
#' @param suburb A character string giving the suburb name
#' with empty spaces replaced with '+', and first letter of each
#' word capitalised.
#' @param postcode A character string giving the 4 digit suburb
#' postcode.
#' @param B Number of boards (i.e., pages of ads) to scape per suburb.
#' @param P Number of ad pages per board (determined by inspecting the
#' AU House Prices website).
#'
#' @return
#' A tibble containing house sales data with missing values
#' represented by \code{NA}.
#'
#' @export
#'
#' @examples
#'
#' # Normally we would set P = 12 (12 ad pages per board)
#' data <- scrape_suburb( suburb = "Bayswater+North", postcode = "3153", P = 1 )
#'
#'
scrape_suburb <- function( suburb, postcode, B = 1, P = 12 ) {

  assertthat::assert_that( P >= 1 )
  assertthat::assert_that( P <= 12 )

  board_urls <- paste0("https://www.auhouseprices.com/sold/list/VIC/",
                       postcode, "/", suburb, "/", 1:B, "/")

  data <- data.frame()

  ls <- list(
    address  = vector(mode = "character" , length = P),
    price    = vector(mode = "integer"   , length = P),
    type     = vector(mode = "character" , length = P),
    bed      = vector(mode = "integer"   , length = P),
    bath     = vector(mode = "integer"   , length = P),
    car      = vector(mode = "integer"   , length = P),
    CBD      = vector(mode = "numeric"   , length = P),
    date     = rep(as.Date("01/01/01")   , times  = P),
    agency   = vector(mode = "character" , length = P),
    images   = vector(mode = "integer"   , length = P),
    land     = vector(mode = "integer"   , length = P),
    med_h    = vector(mode = "integer"   , length = P),
    med_u    = vector(mode = "integer"   , length = P),
    school   = vector(mode = "numeric"   , length = P),
    station  = vector(mode = "numeric"   , length = P)
  )


  for ( i in 1:B ) {
    page_urls <- board_urls[i] %>%
      get_page_urls(P)

    for ( j in 1:P ) {
      page     <- read_html(page_urls[j])
      stations <- get_stations(page)
      schools  <- get_schools(page)
      medians  <- get_medians(page)
      ls$address[j] <- get_address(page, suburb)
      ls$price[j]   <- get_price(page)
      ls$type[j]    <- get_type(page)
      ls$bed[j]     <- get_bed(page)
      ls$bath[j]    <- get_bath(page)
      ls$car[j]     <- get_car(page)
      ls$CBD[j]     <- get_CBD(page)
      ls$date[j]    <- get_date(page)
      ls$agency[j]  <- get_agency(page)
      ls$images[j]  <- get_images(page)
      ls$land[j]    <- get_land(page)
      ls$med_h[j]   <- medians[1]
      ls$med_u[j]   <- medians[2]
      ls$school[j]  <- schools[1]
      ls$station[j] <- stations[1]

    }

    this_data <- data.frame(ls, stringsAsFactors = F)
    data <- rbind(data, this_data)
  }

  data <- as_tibble(data)

  return(data)
}

# LOW LEVEL HELPER FUNCTIONS --------------------------------------------------------

# Extract all the urls for ad pages on this board
get_page_urls <- function(board, P) {
  board %>%
    read_html() %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    str_subset('https://www.auhouseprices.com/sold/view/VIC') %>%
    unique() %>%
    .[1:P]
}

# Helper function to get html text
quick_text <- function(page, tag) {
  page %>%
    html_nodes(tag) %>%
    html_text()
}

# Address
get_address <- function(page, sub) {
  sub <- gsub('\\+',' ',sub)
  page %>%
    quick_text('h2') %>%
    .[1] %>%
    gsub(paste0(', ', sub, '.+'),'',.)
}

# Sale price
get_price <- function(page) {
  page %>%
    quick_text('code') %>%
    .[1] %>%
    gsub("[$,]", '', .) %>%
    as.integer()
}

# Type, bedrooms, bathroom, car
get_type_bed_bath_car <- function(page) {
  page %>%
    quick_text('li') %>%
    .[34]  %>%
    strsplit(split = " ") %>%
    unlist() %>%
    .[.!=""]
}

# Type
get_type <- function(page) {
  page %>%
    get_type_bed_bath_car() %>%
    .[1]
}

# Bedrooms
get_bed <- function(page) {
  page %>%
    get_type_bed_bath_car() %>%
    .[2] %>%
    as.integer()
}

# Bathrooms
get_bath <- function(page) {
  page %>%
    get_type_bed_bath_car() %>%
    .[3] %>%
    as.integer()
}

# Carports
get_car <- function(page) {
  car <- page %>%
    get_type_bed_bath_car() %>%
    .[4]
  if ( is.na(car) ) {
    car <- 0
  } else { car <- as.integer(car) }
  return(car)
}


# Distance to CBD
get_CBD <- function(page) {
  page %>%
    quick_text('code') %>%
    str_subset('km') %>%
    gsub('km', '', .) %>%
    .[1] %>%
    as.numeric()
}

# Sold date
get_date <- function(page) {
  page %>%
    quick_text('h5') %>%
    .[1] %>%
    substr(nchar(.)-11, nchar(.)-1) %>%
    toupper() %>%
    gsub('^\\s', '', .) %>%
    gsub(' ', '-', .) %>%
    as.Date(format = "%d-%b-%Y")
}

# Nearest stations
get_stations <- function(page) {
  page %>%
    quick_text('.pull-right') %>%
    str_subset('km') %>%
    gsub(' km', '', .) %>%
    as.numeric()
}

# Nearest schools
get_schools <- function(page) {
  page %>%
    quick_text('.rounded-2x') %>%
    str_subset('km') %>%
    gsub(' km', '', .) %>%
    as.numeric() %>%
    .[!(. %in% get_stations(page))]
}

# Agency
get_agency <- function(page) {
  page %>%
    quick_text('.btn-link') %>%
    .[1]
}

# Median house price of suburb &
# Median unit price of suburb
get_medians <- function(page) {
  page %>%
    quick_text('.down') %>%
    gsub('[$,]','',.) %>%
    as.integer()
}

# Number of images on advertisement
get_images <- function(page) {
  page %>%
    quick_text('div img') %>%
    length() - 8 %>%
    as.integer()
}

# Land size
get_land <- function(page) {
  land <- page %>%
    quick_text('li') %>%
    str_subset('^Land Size:') %>%
    gsub('Land Size: ', '', .) %>%
    gsub(' m2','',.) %>%
    as.integer()
  if ( length(land) == 0 ) land <- -1
  return(land)
}
