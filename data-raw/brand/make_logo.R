# Render the EventStudy logo mark from its SVG source to a tarball-safe PNG.
#
# DEV-TIME ONLY. rsvg is a development tool used to author brand assets. It MUST
# NOT be added to DESCRIPTION (Imports/Suggests) -- Phase 20 adds zero runtime
# dependencies. Install it interactively (install.packages("rsvg")) if you want
# to re-render, or fall back to the librsvg `rsvg-convert` CLI (used below when
# the R package is unavailable).
#
# Run from the package root:  Rscript data-raw/brand/make_logo.R

svg_in  <- "data-raw/brand/logo.svg"
png_out <- "man/figures/logo.png"

if (requireNamespace("rsvg", quietly = TRUE)) {
  rsvg::rsvg_png(svg_in, png_out, width = 240, height = 240)
} else if (nzchar(Sys.which("rsvg-convert"))) {
  # librsvg CLI fallback -- no R dependency required.
  system2("rsvg-convert",
          c("-w", "240", "-h", "240", "-o", shQuote(png_out), shQuote(svg_in)))
} else {
  stop("Neither the 'rsvg' R package nor the 'rsvg-convert' CLI is available. ",
       "Install one to render ", png_out, " (do NOT add rsvg to DESCRIPTION).")
}

# Optional dev-time size optimisation (CLI tools, not dependencies).
if (file.exists(png_out) && file.size(png_out) >= 51200) {
  if (nzchar(Sys.which("pngquant"))) {
    system2("pngquant", c("--force", "--output", shQuote(png_out),
                          "256", shQuote(png_out)))
  } else if (nzchar(Sys.which("optipng"))) {
    system2("optipng", c("-o7", shQuote(png_out)))
  }
}

stopifnot(file.exists(png_out), file.size(png_out) < 51200)
message("Wrote ", png_out, " (", file.size(png_out), " bytes)")
