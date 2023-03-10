library(leaflet)
library(leafpop)
library(plotly)
library(htmlwidgets)

# https://github.com/r-spatial/leafpop/issues/20

df = read.csv(textConnection(
  "Name,Lat,Long
  Samurai Noodle,47.597131,-122.327298
  Another Place,47.687,-121.753"
))

p = Map(
  function(x, y) {
    df = data.frame(x = x, y = y)
    plot_ly(data=df, x=~x, y=~y, type="scatter")
  }
  , x = list(1:10, 3:12)
  , y = list(10:1, 1:10)
)

fl = lapply(
  p
  , function(j) {
    fl = tempfile(fileext = ".html")
    saveWidget(j, file = fl)
    return(fl)
  }
)

leaflet(df) %>% addTiles() %>%
  addMarkers(
    ~Long
    , ~Lat
    , group="3"
  ) %>%
  leafpop:::addPopupIframes(
    source = fl
    , group = "3"
  )