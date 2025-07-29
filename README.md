Before installing the `cleandataframe` package, install the `devtools` package if it is not installed locally yet.

```r
install.packages("devtools")
```

To reduce the possibility of errors during installation, install the `stringdist` and `snakecase` packages too. (optional)

```r
install.packages(c("snakecase", "stringdist"))
```

Install the `cleandataframe` package.

```r
devtools::install_github("harrison-zhu/cleandataframe", build_vignettes = TRUE)
```
