## R-SPECIFIC: regenerate pkgdown/favicon/ from man/figures/logo.png
## entirely on this machine.
##
## Why this exists instead of pkgdown::build_favicons(): that function
## base64-encodes the logo and POSTs it to
## https://realfavicongenerator.net/api/favicon, then downloads the
## generated set back. This project does not send content to third-party
## endpoints. The output here matches the file set, sizes, and alpha of
## what that service returned, so pkgdown's sitrep check is satisfied and
## docs/ renders identically.
##
## Run from the package root:
##   Rscript tools/build-favicons-local.R

logo <- "man/figures/logo.png"
out  <- "pkgdown/favicon"
stopifnot(file.exists(logo), dir.exists(out))

img <- magick::image_read(logo)

## Square PNGs, alpha preserved. geometry_size_pixels() with a single
## dimension would preserve aspect; the logo is already square (1800x1800)
## but "!" makes the target exact regardless.
png_sizes <- c("favicon-96x96.png"            =  96,
               "apple-touch-icon.png"         = 180,
               "web-app-manifest-192x192.png" = 192,
               "web-app-manifest-512x512.png" = 512)

for (nm in names(png_sizes)) {
    s <- png_sizes[[nm]]
    magick::image_write(
        magick::image_resize(img, sprintf("%dx%d!", s, s)),
        path = file.path(out, nm), format = "png")
}

## Multi-resolution .ico (48, 32, 16), matching the previous file's frames.
ico <- magick::image_join(lapply(c(48L, 32L, 16L), function(s)
    magick::image_resize(img, sprintf("%dx%d!", s, s))))
magick::image_write(ico, path = file.path(out, "favicon.ico"), format = "ico")

## favicon.svg is an SVG wrapper around an embedded raster, the same shape
## the service produced: a 240x240 <image> with a base64 data URI. A true
## vector favicon would need a vector logo, which we do not have.
side <- 240L
tmp  <- tempfile(fileext = ".png")
magick::image_write(magick::image_resize(img, sprintf("%dx%d!", side, side)),
                    path = tmp, format = "png")
b64 <- openssl::base64_encode(readBin(tmp, "raw", file.size(tmp)))
writeLines(c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="%d" height="%d" viewBox="0 0 %d %d">',
            side, side, side, side),
    sprintf('<image width="%d" height="%d" href="data:image/png;base64,%s"/>',
            side, side, b64),
    "</svg>"),
    file.path(out, "favicon.svg"))

## site.webmanifest carries no image data, so it is left as it is.

## pkgdown copies pkgdown/favicon/* to the site root during build; do the
## same here so docs/ does not have to wait for a full rebuild.
for (f in list.files(out, full.names = TRUE)) {
    file.copy(f, file.path("docs", basename(f)), overwrite = TRUE)
}

cat("Regenerated locally:\n")
print(file.info(list.files(out, full.names = TRUE))[, "size", drop = FALSE])
