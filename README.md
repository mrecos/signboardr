
<!-- README.md is generated from README.Rmd. Please edit that file -->
signboardr
==========

Utilize Google Vision API to extract text from archaeological photos containing a sign board. Further, the extracted text can be added as searchable XMP metadata tags to photos.

Installation
------------

You can install signboardr from github with:

``` r
# install.packages("devtools")
devtools::install_github("mrecos/signboardr")
```

Description
-----------

This package is a thin wrapper around the `RoogleVision` package `getGoogleVisionResponse` function with additional functions for plotting of extracted text and writing of extracted text as XMP metadata using the ExifTools tool.

Usage
-----

The general workflow for this package is as follows:

1.  Authenticate Google Vision credentials (see authentication section)

2.  Execute `extract_text()` on a single image or loop over folder of images

3.  Optional: use `plot_results()` to plot the bounding boxes of extracted text

4.  Make the image tags with `make_tags()`

5.  Write the tags to the images using `write_XMP()`

### 1) Authentication

The use of the Google Vision API requires an account and billing information on the [Google Cloud Platform](https://console.cloud.google.com/). At the current time, signing up allows you a one year free trials and $300.00 in credits. There is plenty online about how to setup the API and get your keys, but it is pretty straightforward. Here is a starting point [API Setup](https://cloud.google.com/vision/docs/before-you-begin). Once you have your keys, there are a few lines of setup that need to be run to authenticate your R session so that the images can be analyzed. The code below shows the approach I have used. Note, you need to plug in your client ID and secret token in the first two lines. The easiest, but lease secure way is to hard code the keys into your script. Alternatively, you can put them into a file that is read into the script, or download the client\_secret JSON file from the API dashboard and set is location with `options("googleAuthR.client_secret" = "PATH TO JSON FILE")`

``` r
### plugin your credentials
options("googleAuthR.client_id" = "YOUR CLIENT ID HERE")
options("googleAuthR.client_secret" = "YOUR SECRET KEY HERE")
### define scope
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()
```

### 2) Execute `extract_text()`

### Licenses

**Text and figures:** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code:** See the [DESCRIPTION](DESCRIPTION) file

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please see our [contributor guidelines](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
