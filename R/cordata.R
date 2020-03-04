#' A Corr Data Function
#'
#' @param NBA Dataset. In this case is NBA stat.
#' @param pick_year Year of interest.
#' @keywords Correlation
#' @return A correlation of numberic values within NBA stat dataset.
#' @export
#'
#' @examples cordata(NBA, 1995)
cordata <- function(NBA, pick_year){
  num_data <- keep(NBA, is.numeric) %>% drop_na() %>% select(Year:G, TS., FTr, -(ORB.:USG.),OWS:WS,-(WS.48:VORP), FG:FG.,-(X3P:X3P.),X2P:FT.,-(ORB:TRB),AST,-(STL:TOV),PF:PTS)

  select_year_cor <- num_data %>% filter(Year == pick_year)
  year_cor <- cor(num_data, y = NULL, use = "everything",
                  method = c("pearson", "kendall", "spearman"))
  print(corrplot(year_cor, type="upper", order="hclust", tl.col="black", tl.srt=90))
  print(head(year_cor))
}

