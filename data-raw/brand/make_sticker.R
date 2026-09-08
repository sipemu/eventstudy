# Generate the EventStudy hex sticker -> man/figures/logo-hex.png
#
# DEV-TIME ONLY. hexSticker, magick and rsvg are development tools installed
# interactively by the maintainer. They MUST NOT be added to DESCRIPTION
# (Imports/Suggests) -- Phase 20 adds zero runtime dependencies. This script is
# committed for reproducibility; the rendered PNG is the tracked artifact.
#
# Run from the package root:  Rscript data-raw/brand/make_sticker.R
#
# Two paths are provided:
#   (1) Canonical hexSticker::sticker() call (UI-SPEC "Hex Sticker Layout").
#       Used when hexSticker is installed. Pitfall 1 (hexSticker's SVG subplot
#       fails silently through the `magick` path) is guarded: we probe
#       magick::image_read_svg() first and, if it errors, rasterise logo.svg to
#       a temp PNG via rsvg and hand hexSticker that PNG instead.
#   (2) Pure-CLI fallback (librsvg `rsvg-convert`) that composites an equivalent
#       hexagon from the same logo.svg source. Used when hexSticker is not
#       installed, so the sticker can be regenerated on any machine with librsvg
#       and without adding a single R dependency.

svg_in  <- "data-raw/brand/logo.svg"
png_out <- "man/figures/logo-hex.png"

make_with_hexsticker <- function() {
  # Pitfall 1: hexSticker's SVG subplot path silently drops the artwork on some
  # magick/rsvg builds. Probe first; fall back to a rasterised subplot.
  subplot_src <- svg_in
  svg_ok <- tryCatch({
    magick::image_read_svg(svg_in)
    TRUE
  }, error = function(e) FALSE)

  if (!svg_ok) {
    tmp_png <- tempfile(fileext = ".png")
    rsvg::rsvg_png(svg_in, tmp_png, width = 480, height = 480)
    subplot_src <- tmp_png
  }

  hexSticker::sticker(
    subplot  = subplot_src,
    package  = "EventStudy",
    p_size   = 20,
    p_color  = "#ffffff",
    p_y      = 1.55,
    s_x      = 1,
    s_y      = 0.9,
    s_width  = 0.55,
    h_fill   = "#1e3a8a",   # primary-800 (deep blue hex background)
    h_color  = "#2563eb",   # primary-600 (border)
    url      = "eventstudy.de",
    u_size   = 6,
    u_color  = "#bfdbfe",   # primary-200 (subtle url text)
    filename = png_out,
    dpi      = 300
  )
}

make_with_cli <- function() {
  # Composite an equivalent hex sticker from the same logo.svg motif using only
  # librsvg (rsvg-convert). No R package dependency. The logo strokes are
  # recoloured light/white for contrast against the deep-blue hex fill.
  if (!nzchar(Sys.which("rsvg-convert"))) {
    stop("Neither hexSticker nor the 'rsvg-convert' CLI is available. ",
         "Install one to render ", png_out, " (do NOT add hexSticker/rsvg/",
         "magick to DESCRIPTION).")
  }
  hex_svg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="524" height="606" viewBox="0 0 524 606">
  <polygon points="262,3 521,153 521,453 262,603 3,453 3,153"
           fill="#1e3a8a" stroke="#2563eb" stroke-width="6"/>
  <text x="262" y="135" text-anchor="middle"
        font-family="Plus Jakarta Sans, DejaVu Sans, sans-serif" font-weight="bold"
        font-size="54" fill="#ffffff" letter-spacing="0.5">EventStudy</text>
  <g transform="translate(146,210) scale(2.3)">
    <line x1="10" y1="70" x2="90" y2="70" stroke="#93c5fd" stroke-width="1"
          stroke-dasharray="4 3" opacity="0.55"/>
    <line x1="50" y1="10" x2="50" y2="90" stroke="#bfdbfe" stroke-width="1.5" opacity="0.45"/>
    <path d="M 10 68 C 24 65, 40 52, 50 30 C 58 20, 78 17, 92 16"
          stroke="#ffffff" stroke-width="3.2" fill="none"
          stroke-linecap="round" stroke-linejoin="round"/>
    <circle cx="50" cy="30" r="4.2" fill="#ffffff"/>
  </g>
  <text x="262" y="500" text-anchor="middle"
        font-family="Inter, DejaVu Sans, sans-serif" font-weight="normal"
        font-size="28" fill="#bfdbfe" letter-spacing="2">eventstudy.de</text>
</svg>')
  tmp_svg <- tempfile(fileext = ".svg")
  writeLines(hex_svg, tmp_svg)
  system2("rsvg-convert",
          c("-w", "480", "-h", "555", "-o", shQuote(png_out), shQuote(tmp_svg)))
}

if (requireNamespace("hexSticker", quietly = TRUE) &&
    requireNamespace("magick", quietly = TRUE) &&
    requireNamespace("rsvg", quietly = TRUE)) {
  make_with_hexsticker()
} else {
  make_with_cli()
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
