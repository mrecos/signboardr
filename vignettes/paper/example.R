# devtools::install_github("cloudyr/RoogleVision")
require("signboardr")
require("RoogleVision")
require("googleAuthR")
require("exifr")
library("stringr")
library("tidyverse") # cant require entire tidyverse
library("grid") # for rasterGrob()
library("OpenImageR") # for readImage()

get_key <- readLines("key.api")

### plugin your credentials
options("googleAuthR.client_id" = get_key[1])
options("googleAuthR.client_secret" = get_key[2])
## use the fantastic Google Auth R package
### define scope!
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

img_path <- list.files("./vignettes/data/raw_data/example_images", full.names = TRUE)[1]
file.exists(img_path)
img_results <- extract_text(img_path)
p <- plot_results(img_path, img_results)
p
img_tags    <- make_tags(img_results)
write_XMP(img_path, img_tags)
