Package for loudness analysis. It performs a series of analyses from an
audio file by first transforming it into a timeline with the loudness
values

``` r
library(loudness)

tl <- lutl_from_file('audio/R_MIC_200903-165318.mp3')
#> Get loudness from audio/R_MIC_200903-165318.mp3 using ffmpeg... 
#> Parsinng data...
#> Warning in min(x, na.rm = na.rm): ningún argumento finito para min; retornando Inf
#> Warning in max(x, na.rm = na.rm): ningun argumento finito para max; retornando -Inf
#> Warning in min(x, na.rm = na.rm): ningún argumento finito para min; retornando Inf
#> Warning in max(x, na.rm = na.rm): ningun argumento finito para max; retornando -Inf
#> Warning in min(x, na.rm = na.rm): ningún argumento finito para min; retornando Inf
#> Warning in max(x, na.rm = na.rm): ningun argumento finito para max; retornando -Inf
#> Process datetime... 
#> Elapsed time: 4.52

plot_density(tl)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="80%" style="display: block; margin: auto;" />
