#' Acquire listing of company annual reports.
#' 
#' Extracts and displays listing of annual reports filed by a company in a data frame.
#' 
#' @export
#' @import dplyr
#' @param symbol A character vector specifying the stock symbol of the company of interest.
#' @param foreign A logical vector indicating whether the company is domestic or foreign. \code{foreign = FALSE} by default.
#' @examples
#' AnnualReports("TSLA")
#' AnnualReports("HTHIY", foreign = TRUE)

Reports <- function(symbol, annual = TRUE, quarter = TRUE) {
  
  options(stringsAsFactors = FALSE)
  
  if (annual & quarter){
    url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                  symbol, "&type=10&dateb=&owner=exclude&count=100")
  }else if (annual){
    url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                  symbol, "&type=10-K&dateb=&owner=exclude&count=100")
  } else{
    url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                  symbol, "&type=10-Q&dateb=&owner=exclude&count=100")
  }

  filings <- xml2::read_html(url)
  
  ##   Generic function to extract info
  ExtractInfo <- function(html.node) {
    info <-
      filings %>%
      rvest::html_nodes(html.node) %>%
      rvest::html_text()
    return(info)
  }
  
  ##   Acquire filing name
  filing.name <- ExtractInfo("#seriesDiv td:nth-child(1)")
  
  ##   Error message for function
  if(length(filing.name) == 0) {
    url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                  symbol, "&type=20-f&dateb=&owner=exclude&count=100")
    filings <- xml2::read_html(url)
    filing.name <- ExtractInfo("#seriesDiv td:nth-child(1)")
    
  }
  
  ##   Acquire filing date
  filing.date <- ExtractInfo(".small+ td")
  
  ##   Acquire accession number
  accession.no.raw <- ExtractInfo(".small")
  
  accession.no <-
    gsub("^.*Acc-no: ", "", accession.no.raw) %>%
    substr(1, 20)
  
  ##   Create dataframe
  info.df <- data.frame(filing.name = filing.name, filing.date = filing.date, 
                        accession.no = accession.no)
  return(filter(info.df, filing.name %in% c("10-Q","10-K","20-F")))
}
