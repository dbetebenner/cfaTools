##    Akinshin's Gamma  --  https://aakinshin.net/posts/nonparametric-effect-size/

`gammaEffectSize` <- function(x, y, prob) {
  as.numeric((Hmisc::hdquantile(y, prob) - Hmisc::hdquantile(x, prob)) / phdmad(x, y))
}

###   Function to apply gammaEffectSize to longitudinal data (e.g. from the SGP package)
`gammaEffectSizeLong` <- function(
  data_table,
  variable,
  year_1,
  year_2,
  quantiles=1:9/10,
  digits=3) {
    
    YEAR <- NULL
    x <- na.omit(data_table[YEAR==year_1, get(variable)])
    y <- na.omit(data_table[YEAR==year_2, get(variable)])
    gma <- round(gammaEffectSize(x, y, prob = quantiles), digits)
    names(gma) <- paste0("Q_", quantiles*100)
    gma
}

###   Utility Functions
pooled <- function(x, y, FUN) {
  nx <- length(x)
  ny <- length(y)
  sqrt(((nx - 1) * FUN(x) ^ 2 + (ny - 1) * FUN(y) ^ 2) / (nx + ny - 2))
}

hdmedian <- function(x) as.numeric(Hmisc::hdquantile(x, 0.5))
hdmad <- function(x) 1.4826 * hdmedian(abs(x - hdmedian(x)))
phdmad <- function(x, y) pooled(x, y, hdmad)
