# Install packages if required
# install.packages(c("hexSticker", "usethis", "pkgdown"))

# 1. Create the hex sticker
hexSticker::sticker(
  subplot = "inst/cat.png",
  package = "WondeCL",
  p_size = 20,
  s_x = 1,
  s_y = 0.8,
  s_width = 0.7,
  h_fill = "#009BC1",
  h_color = "#10263B",
  filename = "inst/figures/imgfile.png"
)

# 2. Update the package logo
usethis::use_logo("inst/figures/imgfile.png")

# 3. Regenerate pkgdown favicons
pkgdown::build_favicons(overwrite = TRUE)

# 4. Rebuild the website
pkgdown::build_site()
