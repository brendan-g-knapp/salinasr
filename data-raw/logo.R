library(hexSticker)
library(magick)

city_seal_url <- "http://files.ctctcdn.com/b9f56ac3301/ee9d8250-fe4b-490b-9879-378786452ae7.png"

dest_path <- "man/figures/logo.png"

logo <- image_read(city_seal_url)

seal_res_x <- magick::image_info(logo)[["width"]] / 275
seal_res_y <- magick::image_info(logo)[["height"]] / 275

sticker(expression(plot(logo)),
        s_x = 0.725,
        s_y = 0.6,
        s_width = seal_res_x,
        s_height = seal_res_y,
        package = "salinasr",
        p_y = 1.6, p_size = 18,
        h_color = "#3b3b3b",
        h_fill = "#5E6B28",
        u_size = 4,
        u_color = "white",
        url = "github.com/brendan-g-knapp/salinasr",
        filename = dest_path)
