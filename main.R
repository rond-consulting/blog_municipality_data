# Script related to the municipal data blog
#
# Script contains:
# - code examples on how to do things
# - visuals for the blog
#
# Author: Hans Weda @ rond consulting
# Date: 28 februari 2023

rm(list=ls())

# search statline database
require("cbsodataR")
out <- cbsodataR::cbs_search(query="Kerncijfers wijken en buurten")
write.csv(out[,1:4], file=file.path("data", "statline_search.csv"))

# take one of the Kerncijfers datasets and look at the contents
df <- cbsodataR::cbs_get_data(id="84799NED") 
View(df)
write.csv(head(df), file=file.path("data", "kerncijfers_example.csv"))
print(table(df["SoortRegio_2"]))


# load all kerncijfers datasets
require(dplyr)

# find all relevant identifiers - the title starts with 'Kerncijfers'
ids <- out$Identifier[grepl("^Kerncijfers", out$Title)]

# create empty data-frame to be filled
df <- data.frame()

# loop over all id's 
for (id in ids) {
  df <- rbind(
    df,
    # download data, filter on 'Gemeente' and select relevant columns
    cbsodataR::cbs_get_data(
      id=id,
      select=c("WijkenEnBuurten", "AantalInwoners_5", "k_65JaarOfOuder_12"),
      WijkenEnBuurten = has_substring("GM")
    ) %>%
      cbs_add_label_columns() %>%
      # add a 'Year' column to keep track of the year the data is from
      mutate(
        Year = as.numeric(
          gsub("\\D", "", out[out["Identifier"]==id, "Title"])
        )
      )
  )
}

# take a look at the result
View(df)


# explore the number of municipalities
require(ggplot2)

ggplot2::ggplot(
  data = df,
  mapping = ggplot2::aes(x=Year)
) +
  ggplot2::geom_bar(fill="dodgerblue") +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "Number of Dutch municipalities per year",
    y = "Number of municipalities"
  )
  


# show population of selected municipalities
selected_municipalities = c("Vught", "Eemsdelta", "Wageningen", "Montferland", "Gorinchem")

ggplot2::ggplot(
  data = df %>%
    filter(WijkenEnBuurten_label %in% selected_municipalities),
  mapping = ggplot2::aes(
    x=Year, 
    y=AantalInwoners_5, 
    color=WijkenEnBuurten_label
  )
) + 
  ggplot2::geom_line(size=1.5) +
  ggplot2::labs(
    title = "Inhabitants per year",
    y = "Number of inhabitants",
    color = "Municipality"
  ) +
  ggplot2::theme_bw() 

# # find suitable municipalities for plotting
# test <- df %>%
#   mutate(diff = abs(AantalInwoners_5 - 35000)) %>%
#   arrange(diff)

require(devtools)

# install 'grenswijzigen' package from Github
devtools::install_github("https://github.com/VNG-Realisatie/grenswijzigen")
library(grenswijzigen)

# Transform the historic data to 2022
df_transformed <- wrapper_vertaal_naar_peiljaar(
  
  # the package requires 'wijkcode' and 'jaar' as column names
  as.data.frame(
    df %>% rename(
      wijkcode = WijkenEnBuurten,
      gemeentenaam = WijkenEnBuurten_label,
      jaar = Year
    ) %>%
      mutate(wijkcode=trimws(wijkcode))
  ),
  peiljaar = 2022,
  model="model.2",
  regionaalniveau = "gemeente",
  kolommen_aantal = c("AantalInwoners_5", "k_65JaarOfOuder_12")
) 

# show that we have equal numbers of municipalities each year
print(table(df_transformed$jaar))

View(df_transformed)

# show how the transformed data looks like
ggplot2::ggplot(
  data = df_transformed %>%
    filter(gemeentenaam %in% selected_municipalities),
  mapping = ggplot2::aes(
    x=jaar, 
    y=AantalInwoners_5, 
    color=gemeentenaam,
    linetype=berekend
  )
) + 
  ggplot2::geom_line(size=1.5, linetype="dotted") +
  ggplot2::geom_line(size=1.5) +
  ggplot2::labs(
    title = "Inhabitants per year",
    y = "Number of inhabitants",
    color = "Municipality",
    linetype = "Calculation"
  ) + 
  ggplot2::scale_linetype_manual(values=c("solid", "dotted"), labels=c("Factual", "Estimated")) +
  ggplot2::theme_bw()


# development of elderly population
require(scales)
ggplot2::ggplot(
  data = df_transformed %>%
    mutate(perc = k_65JaarOfOuder_12 / AantalInwoners_5),
  mapping = ggplot2::aes(
    x=jaar, 
    y=perc, 
    group=jaar
  )
) + 
  ggplot2::geom_boxplot(fill="dodgerblue") +
  ggplot2::scale_y_continuous(labels=scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Percentage of elderly by year",
    x = "Year",
    y = "Percentage"
  )
  
# forecast
require(dlm)


create_forecast <- function(municipality, feature) {
  # state space model with deterministic trend
  build.dt <- function(theta) {
    dlmModPoly(2, dV=exp(theta[1]), dW=c(0,0))
  }
  # create vector with number of elderly (time sorted)
  d <- df_transformed %>% 
    filter(gemeentenaam==municipality) %>% 
    arrange(jaar) %>% 
    pull(`feature`)
  
  # reduce size for better estimation
  d = d/1000
  
  # fit and filter the state-space model
  fit.dt <- dlmMLE(d, parm=log(var(d, na.rm=T)+1), build.dt)
  
  mod.dt <- build.dt(fit.dt$par)
  
  filter.dt <- dlmFilter(d, mod.dt)
  
  # forecast 5 years ahead
  pred.dt <- dlmForecast(filter.dt, nAhead=5)
  
  sqrtR <- sapply(pred.dt$R, function(x) sqrt(x[1,1]))
  
  # add prediction to data frame
  df.out <- data.frame(
    out=1000*pred.dt$a[,1], 
    pl=1000*(pred.dt$a[,1] + qnorm(0.05, sd = sqrtR)), 
    pu=1000*(pred.dt$a[,1] + qnorm(0.95, sd = sqrtR)),
    jaar=2023:2027,
    gemeentenaam=municipality,
    wijkcode=df_transformed %>% filter(gemeentenaam==municipality) %>% pull(wijkcode) %>% head(1)
  )

  return(df.out)
}


# create forecast for Vught
p <- ggplot2::ggplot(
  data = df.plot <- bind_rows(
    df_transformed %>% 
      filter(gemeentenaam=="Vught") %>%
      rename(out=k_65JaarOfOuder_12) %>%
      mutate(pl=out, pu=out),
    create_forecast("Vught", "k_65JaarOfOuder_12")
  ),
  mapping = aes(x=jaar, color=gemeentenaam, fill=gemeentenaam)
) + 
  ggplot2::geom_ribbon(aes(ymin=pl, ymax=pu), alpha=0.5) +
  ggplot2::geom_line(aes(y=out), size=1.5) +
  ggplot2::scale_x_continuous(breaks=2016:2027) +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "Expected number of elderly inhabitants in Vught",
    x = "Jaar",
    y = "Number of elderly inhabitants"
  )
print(p)

# save the figure
jpeg(
  file=file.path("figures", "04_forecast.jpg"), 
  width=6, 
  height=4, 
  units="in", 
  res=100
)
p
dev.off()


# make forecast for all municipalities
forecast_all <- function(feature) {
  cat(sprintf("Forecasting all for feature %s", feature), "\n")
  df.plot <- data.frame()
  for (municipality in unique(df_transformed$gemeentenaam)) {
    cat(municipality, ",")
    df.plot <- bind_rows(
      df.plot,
      df_transformed %>% 
        filter(gemeentenaam==municipality) %>%
        rename(out=`feature`) %>%
        mutate(pl=out, pu=out),
      create_forecast(municipality, feature)
    )
  }
  return(df.plot)
}

df1 <- forecast_all("k_65JaarOfOuder_12")
df2 <- forecast_all("AantalInwoners_5")
df.plot <- cbind(
  df1 %>% rename(k_65JaarOfOuder_12=out) %>% select(wijkcode, k_65JaarOfOuder_12, jaar), 
  df2 %>% rename(AantalInwoners_5=out) %>% select(AantalInwoners_5)
)

print(table(df.plot$jaar))

# get geo-boundaries for municipalities
require(sf)
url <- 'https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?'
options <- paste('request=GetFeature', 'service=WFS', 'version=2.0.0', 'outputFormat=json', 'srsName=epsg:4326', sep = '&')
df.geo <- sf::st_read(paste0(url, options, sprintf('&typeName=cbs_%s_%d_gegeneraliseerd', "gemeente", 2022)))

# merge with data.frame
df <- merge(
  df.geo, 
  df.plot %>%
    mutate(perc = k_65JaarOfOuder_12 / AantalInwoners_5), 
  by.x="statcode", 
  by.y="wijkcode"
)

require(gganimate)
require(viridis)
library(gifski)
myGif <- ggplot2::ggplot(data=df, mapping=aes(fill=perc)) + 
  ggplot2::geom_sf(size=0.05) +
  theme_void() +
  scale_fill_viridis(labels=scales::percent) +
  theme(
    legend.position = c(0.1, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin=margin(10, 10, 10, 10, "pt")
  ) +
  ## gganimate functionality starts here
  transition_time(jaar) +
  labs(
    title = "Percentage elderly in year {round(frame_time, 0)}",
    fill = "Percentage elderly"
  ) +
  ease_aes("linear")

animate(myGif, duration = 5, fps = 10, start_pause=5, end_pause=5, width = 600, height = 800, renderer = gifski_renderer())
anim_save(file.path("figures", "elderly.gif"))

# auto.arima
require(forecast)
ts_municipality <- ts(
  df_transformed %>% 
    filter(gemeentenaam==municipality) %>%
    arrange(jaar) %>% 
    select(k_65JaarOfOuder_12),
  start=2016
)

plot(
  forecast(auto.arima(ts_municipality), h=5),
  xlab = "Year",
  ylab = "Number of elderly inhabitants"
)
lines(ts(df.plot %>% arrange(jaar) %>% pull(out), start=2016))

