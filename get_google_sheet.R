library(readr)
library(stringr)

get_google_sheet <- function(
    url = 'https://docs.google.com/spreadsheets/d/1duebsRHUe2AEsHomm5wfkfsZLYvVSrW_Dl1lwtP3t80',
    format = 'csv',
    sheetId = 0
) {

  # Taken from Max Conway: https://github.com/maxconway/gsheet/tree/master
  key <- str_extract(
    url,
    '[[:alnum:]_-]{30,}'
  
  )
  if(
    is.null(sheetId) && str_detect(
      url,
      'gid=[[:digit:]]+'
    )
  ) {
    sheetId <- as.numeric(
      str_extract(
        str_extract(
          url,
          'gid=[[:digit:]]+'),
          '[[:digit:]]+'
        )
      )
  }

  address <- paste0(
    'https://docs.google.com/spreadsheets/export?id=',
    key,
    '&format=',
    format
  )

  if(
    !is.null(sheetId)
  ){
    address <- paste0(
      address,
      '&gid=',
      sheetId
    )
  }
  
  df <- read_csv(
    address,
    col_types = cols(
      Who = col_character(),
      Month = col_character(),
      Hours = col_double(),
      "hourly cost" = col_double(),
      km = col_double(),
      "distance cost" = col_double(),
      fees = col_double(),
      "Actual Paid" = col_double(),
      "Open (2026 rate)" = col_double(),
      "Open Plus" = col_double(),
      Value = col_double(),
      "Value Plus" = col_double(),
    )
  )

  return(df)
}