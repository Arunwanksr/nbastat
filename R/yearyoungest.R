#' A Youngest Function
#'
#' @param NBA Dataset. In this case is NBA stat.
#' @param pick_year Year of interest.
#' @keywords youngest
#' @return The youngest player in NBA within the chosen year.
#' @export
#'
#' @examples function(NBA, 1995)
yearyoungest <- function(NBA, pick_year){
  select <- select(NBA, Year, Age, Player)
  select_year <- filter(select, pick_year == Year)
  select_year_youngest <- filter(select_year, Age == min(Age))
  print(head(select_year_youngest))
}
