#' An Oldest Function
#'
#' @param NBA Dataset. In this case is NBA stat.
#' @param pick_year Year of interest.
#' @keywords oldest
#' @return The oldest player in NBA within the chosen year.
#' @export
#'
#' @examples function(NBA, 1995)
yearoldest <- function(NBA, pick_year){
  select <- select(NBA, Year, Age, Player)
  select_year <- filter(select, pick_year == Year)
  select_year_oldest <- filter(select_year, Age == max(Age))
  print(head(select_year_oldest))
}
