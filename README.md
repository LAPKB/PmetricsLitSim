
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PmetricsLitSim

<!-- badges: start -->
[![R-CMD-check](https://github.com/LAPKB/PmetricsLitSim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LAPKB/PmetricsLitSim/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`PmetricsLitSim` is a Shiny companion app for the
[Pmetrics](https://github.com/LAPKB/Pmetrics) package, designed to
facilitate extraction of models from published parametric population
models.

## Installation

`PmetricsLitSim` will be automatically installed as a dependency of the
`Pmetrics` package.

### If automatic installation fails…

You can also manually install `PmetricsLitSim` from
[r-universe](https://lapkb.r-universe.dev/PmetricsLitSim). To install,
you must specify `r-universe` as the repository using

``` r
install.packages("PmetricsLitSim", repos = "https://lapkb.r-universe.dev")
```

For updating the package and for easier future manual installations, you
can run the following command to add the `r-universe` repository to your
`R.profile`

``` r
write('options(repos = c(CRAN = "https://cloud.r-project.org", LAPKB = "https://lapkb.r-universe.dev"))', "~/.Rprofile", append = TRUE)
```

With this setup, you can install or update `PmetricsLitSim` using the
standard command `install.packages("PmetricsLitSim")` without specifying
the repository each time.

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
