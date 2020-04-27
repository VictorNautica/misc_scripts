hex_bw <- function(hex_code) {
  
  myrgb <- as.integer(col2rgb(hex_code))
  
  rgb_conv <- lapply(myrgb, function(x) {
    i <- x / 255
    if (i <= 0.03928) {
      i <- i / 12.92
    } else {
      i <- ((i + 0.055) / 1.055) ^ 2.4
    }
    return(i)
  })

 rgb_calc <- (0.2126*rgb_conv[[1]]) + (0.7152*rgb_conv[[2]]) + (0.0722*rgb_conv[[3]])
 
 if (rgb_calc > 0.179) return("#000000") else return("#ffffff")
   
}

## Implementing R version answer for this question https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color

