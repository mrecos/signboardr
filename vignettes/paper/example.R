extract_text <- function(imgPath){
  image_text = getGoogleVisionResponse(imgPath, feature = 'TEXT_DETECTION')
}
make_tags <- function(image_text){
  img_tags <- image_text[1,"description"] %>%
    str_replace_all("\n", " ") %>%
    str_trim()
}
plot_results <- function(img_url, image_text){
  img <- readImage(img_url)
  bbox_dat <- get_box_coords(img, image_text)
  results_plot <- ggplot_bbox_coords(img, bbox_dat[["bbox_coords"]], bbox_dat[["bbox"]])
}
get_box_coords <- function(img, image_text){
  bbox_cnt <- length(image_text$boundingPoly$vertices)
  bbox <- data.frame(group = bbox_cnt + 1,
                     desc  = "bounding box",
                     xLL   = 0, xUR   = dim(img)[2],
                     xUL   = 0, xLR   = dim(img)[2],
                     yLL   = 0, yUR   = dim(img)[1],
                     yUL   = dim(img)[1], yLR = 0)
  bbox_coords <- bind_rows(image_text$boundingPoly$vertices) %>%
    mutate(group = rep(seq(1,bbox_cnt,1), each = 4),
           desc  = rep(image_text$description, each = 4),
           pos   = rep(c("LL","LR","UR","UL"), times = bbox_cnt)) %>%
    filter(group != 1)
  return(list(bbox = bbox, bbox_coords = bbox_coords))
}
ggplot_bbox_coords <- function(img, bbox_coords, bbox){
  g <- rasterGrob(img, interpolate=TRUE)
  p <- ggplot(bbox_coords,aes(x=x, y=(bbox$yUR-y))) +
    annotation_custom(g, xmin=bbox$xLL, xmax=bbox$xUR,
                      ymin=bbox$yLL, ymax=bbox$yUR) +
    # geom_polygon(aes(group = group, color = as.factor(desc)), fill = NA, size = 0.75) +
    geom_polygon(aes(group = group, fill = as.factor(desc)), alpha = 0.5) +
    coord_fixed(xlim = c(bbox$xLL,bbox$xUR), ylim = c(bbox$yLL,bbox$yUR)) +
    labs(fill = "Label Text") +
    theme_void()
  return(p)
}
write_XMP <- function(img_url, img_tags){
  exifr::exifr(image_url,
               exiftoolargs=paste0("-XMP:Subject=", "'",
                                   img_tags, "'",
                                   " -overwrite_original"))
}

# devtools::install_github("cloudyr/RoogleVision")
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

img_url <- list.files("./vignettes/data/raw_data/example_images", full.names = TRUE)[1]
file.exists(img_url)
img_results <- extract_text(img_url)
img_tags    <- make_tags(img_results)
write_XMP(img_url, img_tags)
