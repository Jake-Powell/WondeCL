install.packages("hexSticker")
library(hexSticker)

sticker(subplot = '/Users/jakepowell/OME/WondePull/inst/cat.png', package="WondePull", p_size=20, s_x=1, s_y=.8, s_width=.7,
        h_fill="#009BC1", h_color="#10263B",
        filename="inst/figures/imgfile.png")
usethis::use_logo("inst/figures/imgfile.png")
