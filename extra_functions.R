capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s,1,1)), {
      s <- substring(s,2); if (strict) tolower(s) else s
    }, sep = "", collapse = " " )
  }
  
  if (!onlyfirst) {
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  } else {
    sapply(s, function(x)
      paste(toupper(substring(x,1,1)),
            tolower(substring(x,2)),
            sep = "", collapse = " "), USE.NAMES = FALSE)
  }
}

if.na <- function(A,B) ifelse(!is.na(A),A,B)
if.zero <- function(A,B) ifelse(A==0,B,A)
greatest <- function(A,B) ifelse(A<B,B,A)
least <- function(A,B) ifelse(A<B,A,B)

elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date,"GMT")
    sd <- as.POSIXlt(start_date,"GMT")
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

month_name <- function(start_date,end_date) {
    if (is.na(end_date)) {
        date <- start_date + 6
    } else {
        if (elapsed_months(start_date+6,start_date) > 0 & elapsed_months(end_date,start_date) > 0) {
            date <- start_date + 6
        } else {
            date <- start_date
        }
    }
    return(capwords(format(date,format="%B")))
}
