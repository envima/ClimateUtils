\name{Voronoi}
\alias{voi}
\title{Test for Voronoi Function}
\usage{
voronoi()
}
\description{
Prints 'Hello, world!'.
}
\examples{
# extracted from terra package
wkt <- c("MULTIPOLYGON ( ((40 40, 20 45, 45 30, 40 40)),
        ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),(30 20, 20 15, 20 25, 30 20)))",
        "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))")
x <- vect(wkt)


# terra
vt <- voronoi(x, "terra")
plot(vt, lwd=2, col=rainbow(15))


# sf
s <- st_as_sf(x)
vs <- st_voronoi(s)
plot(vs, col = 0)
}
