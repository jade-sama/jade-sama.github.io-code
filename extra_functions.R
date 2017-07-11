capwords <- function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)

if.na <- function(A,B) ifelse(!is.na(A),A,B)
if.zero <- function(A,B) ifelse(A==0,B,A)

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
