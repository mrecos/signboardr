#' @title Extract text from photo with GoogleVision API
#'
#' @description
#' \code{extract_text} Calls GoogleVision API with TEXT_DETECTION feature
#'
#' @details
#' This is a function that simply calls the GoogleVision API and passes a photo URL
#' The only argument is \code{imgPath} which should be a file path to an image file.
#' @param img_path a file path or URL to an image file of type jpg, tiff, or png
#' @examples
#' \dontrun{
#' img_file_location <- "./vignettes/data/raw_data/example_images/IMG_0797.JPG"
#' image_text <- extract_text(img_file_location)
#' }
#' @importFrom RoogleVision getGoogleVisionResponse
#' @export
extract_text <- function(img_path){
  image_text = RoogleVision::getGoogleVisionResponse(img_path, feature = 'TEXT_DETECTION')
}

#' @title Convert API response to tags
#'
#' @description
#' \code{make_tags} Takes the response from GoogleVision API and returns tags for metadata
#' @param image_text a data.frame of extracted text as returned from \code{extract_text}
#' @export
#' #' @importFrom stringr str_replace_all str_trim
make_tags <- function(image_text){
  img_tags <- image_text[1,"description"] %>%
    stringr::str_replace_all("\n", " ") %>%
    stringr::str_trim()
}

#' @title Plots text and bounding boxes on photo
#'
#' @description
#' \code{plot_results} Utilizes ggplot2 to plot extracted text and bounding boxes on image
#' @param img_path a file path or URL to an image file of type jpg, tiff, or png
#' @param image_text a data.frame of extracted text as returned from \code{extract_text} using the same image as \code{img_path}
#' @examples
#' \dontrun{
#' img_file_location <- "./vignettes/data/raw_data/example_images/IMG_0797.JPG"
#' image_text <- extract_text(img_file_location)
#' img_plot <- plot_results(img_file_location, image_text)
#' img_plot
#' }
#' @export
plot_results <- function(img_path, image_text){
  image_name <- basename(img_path)
  if(grepl("[A-Z]", str_sub(image_name, -3, -1))){
    image_name <- paste0(str_sub(image_name, 1, -4),
                         tolower(str_sub(image_name, -3, -1)))
    img_path <- file.path(dirname(img_path),image_name)
  }
  img <- readImage(img_path)
  bbox_dat <- get_box_coords(img, image_text)
  results_plot <- ggplot_bbox_coords(img, bbox_dat[["bbox_coords"]], bbox_dat[["bbox"]])
}

#' @title Create bounding box coordinates
#'
#' @description
#' \code{get_box_coords} Strips bounding box coordinates from API response
#' and returns a list includng the image bounding box as \code{bbox} and
#' extracted text bounding boxes as \code{bbox_coords}
#'
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

#' @title Create ggplot object of bounding boxes
#'
#' @description
#' \code{ggplot_bbox_coords} Builds ggplot object from image, tags, and bounding boxes
#' @importFrom ggplot2 ggplot
ggplot_bbox_coords <- function(img, bbox_coords, bbox){
  g <- rasterGrob(img, interpolate=TRUE)
  p <- ggplot2::ggplot(bbox_coords,aes(x=x, y=(bbox$yUR-y))) +
    annotation_custom(g, xmin=bbox$xLL, xmax=bbox$xUR,
                      ymin=bbox$yLL, ymax=bbox$yUR) +
    # geom_polygon(aes(group = group, color = as.factor(desc)), fill = NA, size = 0.75) +
    geom_polygon(aes(group = group, fill = as.factor(desc)), alpha = 0.5) +
    coord_fixed(xlim = c(bbox$xLL,bbox$xUR), ylim = c(bbox$yLL,bbox$yUR)) +
    labs(fill = "Label Text") +
    theme_void()
  return(p)
}

#' @title Write metadata tags to image
#'
#' @description
#' \code{extract_text} Writes XMP metadata tags to image using exiftools
#' @param img_path a file path or URL to an image file of type jpg, tiff, or png
#' @param img_tags a character vector of metadata tags
#' @seealso \code{\link{extract_text}} and \code{\link{make_tags}}
#' @export
#' @import exifr
write_XMP <- function(img_path, img_tags){
  exifr::exifr(img_path,
               exiftoolargs=paste0("-XMP:Subject=", "'",
                                   img_tags, "'",
                                   " -overwrite_original"))
}
