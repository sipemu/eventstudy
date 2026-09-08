# Render the EventStudy Open Graph social card.
#
# Source:  data-raw/brand/og-card.svg  (1200x630, ASCII-only copy)
# Output:  man/figures/og-card.png     (tarball-safe, < 200 KB)
#
# Dev-time only. rsvg / rsvg-convert / magick are NOT declared in DESCRIPTION.
# Prefers the rsvg R package when installed; otherwise falls back to the
# librsvg `rsvg-convert` CLI (the same tool that produced logo.png in 20-01).
# Run from the package root:  Rscript data-raw/brand/make_og_card.R

svg_in  <- "data-raw/brand/og-card.svg"
png_out <- "man/figures/og-card.png"
stopifnot(file.exists(svg_in))

if (requireNamespace("rsvg", quietly = TRUE)) {
  rsvg::rsvg_png(svg_in, png_out, width = 1200, height = 630)
} else if (nzchar(Sys.which("rsvg-convert"))) {
  status <- system2(
    "rsvg-convert",
    c("-w", "1200", "-h", "630", shQuote(svg_in), "-o", shQuote(png_out))
  )
  if (!identical(status, 0L)) stop("rsvg-convert failed to render og-card.png")
} else {
  stop("Neither the rsvg package nor the rsvg-convert CLI is available.")
}

# Optimise only if over the 200 KB Open Graph budget. Try pngquant, then
# optipng, then ImageMagick -- whichever is on PATH. All are dev-time tools.
size_limit <- 204800L
if (file.exists(png_out) && file.size(png_out) >= size_limit) {
  if (nzchar(Sys.which("pngquant"))) {
    system2("pngquant", c("--force", "--skip-if-larger", "--output",
                          shQuote(png_out), "256", shQuote(png_out)))
  } else if (nzchar(Sys.which("optipng"))) {
    system2("optipng", c("-o5", "-quiet", shQuote(png_out)))
  } else if (nzchar(Sys.which("magick"))) {
    system2("magick", c(shQuote(png_out), "-strip",
                        "-define", "png:compression-level=9", shQuote(png_out)))
  }
}

stopifnot(
  file.exists(png_out),
  file.size(png_out) < size_limit
)
message(sprintf("og-card.png written: %d bytes", file.size(png_out)))
