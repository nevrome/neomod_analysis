# load libraries
library(RCurl)
library(magrittr)

# read data from URL
myfile <- getURL(
  'https://raw.githubusercontent.com/nevrome/CalPal-Database/master/CalPal_14C-Database.csv', 
  ssl.verifyhost = FALSE, 
  ssl.verifypeer = FALSE
)

CALPAL <- read.csv(
  textConnection(myfile), 
  header = TRUE, 
  sep = ",",
  stringsAsFactors = FALSE
)

# remove ID column and empty colums at the end
CALPAL <- CALPAL[, 2:20]

# add key attributes ORIGIN and ID
CALPAL <- data.frame(
  ORIGIN = "CALPAL",
  ID = 1:nrow(CALPAL), 
  CALPAL
)

CALPAL$LONGITUDE <- CALPAL$LONGITUDE %>%
  taRifx::destring()
CALPAL$LATITUDE <- CALPAL$LATITUDE %>%
  taRifx::destring()

