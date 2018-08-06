library(hexSticker)
library(magick)

city_seal_url <- "http://files.ctctcdn.com/b9f56ac3301/ee9d8250-fe4b-490b-9879-378786452ae7.png"

logo_path <- "man/figures/logo.png"
site_logo_path <- "docs/logo.png"
favicon_path <- "docs/favicon.ico"

if(!dir.exists("man/figures")) {
  dir.create("man/figures")
} 
if(!dir.exists("docs")) {
  dir.create("docs")
} 

# logo ====
logo <- image_read(city_seal_url)

seal_res_x <- image_info(logo)[["width"]] / 275
seal_res_y <- image_info(logo)[["height"]] / 275

sticker(expression(plot(logo)),
        s_x = 0.725,
        s_y = 0.6,
        s_width = seal_res_x,
        s_height = seal_res_y,
        package = "salinasr",
        p_y = 1.6, p_size = 18,
        h_color = "#3b3b3b",
        h_fill = "#5E6B28",
        u_size = 5,
        u_color = "white",
        url = "knapply.github.io/salinasr",
        filename = logo_path)

sticker(expression(plot(logo)),
        s_x = 0.725,
        s_y = 0.6,
        s_width = seal_res_x,
        s_height = seal_res_y,
        package = "salinasr",
        p_y = 1.6, p_size = 18,
        h_color = "#3b3b3b",
        h_fill = "#5E6B28",
        u_size = 5,
        u_color = "white",
        url = "knapply.github.io/salinasr",
        filename = site_logo_path)

# favicon ====
# logo <- image_read(logo_path)
# 
# image_write(logo, favicon_path)
# 
# scale_x <- 28 / image_info(logo)[["width"]] %>% round(2)
# scale_y <- 32 / image_info(logo)[["height"]]
# 
# scale_geometry <- glue::glue("{scale_x}x")
# 
# favicon <- image_scale(logo, "75")
# 
# image_write(favicon, favicon_path)
