### auto::check()

A small personal package to automate checking of R packages, using `devtools::check()` and `goodpractice::gp()`, with some minor tweaks.

I use this workflow as part of my code reviews of R packages submitted to Methods in Ecology and Evolution.

You can use it like this:

```r
auto::check("cran/versions")
``` 
