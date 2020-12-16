#' Get Fred Data
#'
#' @param APIkey This should be the API key of the Fred page
#' @param search_text This should be the key word of the data you want to get. The default is "interest+rate"
#' @param realtime_start This should be the starting data, the format should be yyyy-mm-dd and the default is "2000-01-01"
#'
#' @return Returns the data frame with the data you want
#' @export
#'
#' @examples
#' fred_get_series(your_API_Key, search_text="interest+rate", realtime_start = "2000-01-01")
fred_get_series <- function(APIkey, search_text, realtime_start){
  URL ="https://api.stlouisfed.org/fred/series/search"
  search_text = paste("interest","+","rate",sep = "")
  realtime_start = "2000-01-01"

  PATH = paste(URL,
               '?search_text=', search_text,
               '&api_key=',APIkey,
               "&file_type=json",
               "&realtime_start=", realtime_start,sep = "")

  initialquery = fromJSON(PATH)
  df = initialquery$seriess
  title = list()
  for (i in 1:length(df)){
    title = as.data.frame(append(title, df[[i]][["title"]]))
  }
  popularity = list()
  for (i in 1:length(df)){
    popularity = as.data.frame(append(popularity, df[[i]][["popularity"]]))
  }
  frequency = list()
  for (i in 1:length(df)){
    frequency = as.data.frame(append(frequency, df[[i]][["frequency"]]))
  }
  unit = list()
  for (i in 1:length(df)){
    unit = as.data.frame(append(unit, df[[i]][["units_short"]]))
  }
  final_data = cbind(t(title), t(unit), t(frequency), t(popularity))
  colnames(final_data) <- c("Title", "Unit", "Frequency", "Popularity")
  rownames(final_data) <- 1:nrow(final_data)

  return(final_data)
}
