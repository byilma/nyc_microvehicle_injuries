Pull and Tidy
================
11/27/2020

  - [Setup](#setup)
  - [Data Import](#data-import)
  - [Tidying, Data Wrangling](#tidying-data-wrangling)
  - [Filtering](#filtering)
  - [EDA, Injuries by Month](#eda-injuries-by-month)
  - [EDA, Crashes Over Time, Cont’d](#eda-crashes-over-time-contd)
  - [EDA, Contributing Factors](#eda-contributing-factors)
  - [Visualizations: Maps](#visualizations-maps)
      - [Location Data](#location-data)
          - [Crashes with Injuries by
            Borough](#crashes-with-injuries-by-borough)
      - [Density](#density)
          - [Density of Bicycle Crashes in
            2019](#density-of-bicycle-crashes-in-2019)

# Setup

# Data Import

# Tidying, Data Wrangling

``` r
#cleaning - transpose so vehicle types are listed in one column
crash_dat = 
  crash_dat %>% 
  mutate(
    dow = as.factor(weekdays(crash_date))
  ) %>%
  separate(crash_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         day = as.integer(day)) %>%
  pivot_longer(
    vehicle_type_code1:vehicle_type_code_5,
    names_to = "vehicle_number",
    values_to = "vehicle_options",
    names_prefix = "vehicle_type_code"
   ) %>%
  drop_na(vehicle_options)
```

# Filtering

``` r
#Exploring the vehicle types so that we can limit to bicycles -- double check this list
bikes = crash_dat %>%
  filter(str_detect(vehicle_options, "[Bb]ike") | 
           str_detect(vehicle_options, "REVEL") | 
           str_detect(vehicle_options, "SCO")  |
           str_detect(vehicle_options, "MOP")   |
           str_detect(vehicle_options, "ELEC")  |
           str_detect(vehicle_options, "^E-")) %>% 
  filter(vehicle_options != "ESCOVATOR" & vehicle_options != "Bike" &
      str_detect(vehicle_options, "Dirt", negate = TRUE),
      str_detect(vehicle_options, "[Mm]otorbike", negate = TRUE)
           ) 
           
# Run to check what the filter ^ resulted in
# bikes %>%
#   group_by(vehicle_options) %>%
#   count() %>% View()
```

Microvehicle incidents in NYC \_ a leaflet map – I just commented this
out to knit the document

# EDA, Injuries by Month

``` r
month_df=
  tibble(
    month = 1:12,
    month_name = factor(month.name, ordered = TRUE, levels = month.name)
  )

fit_injuries_month = crash_dat %>%
  group_by(year) %>%
  mutate(year_2020 = year - 2019) %>%
  nest(data = -month) %>%
  mutate(models = map(data, ~glm(number_of_persons_injured ~ year_2020:borough,
                                 family = "poisson", data = .x)),
         models = map(models, broom::tidy)) %>% 
  select(-data) %>% 
  unnest(models) %>%
  select(month, term, estimate, std.error, p.value) %>% 
  mutate(term = str_replace(term, "year_2020:borough", "2020 v. 2019, Borough: ")) %>%
  left_join(month_df, by = "month") %>%
  select(-month) %>%
  rename(month = month_name) %>%
  select(month, everything())

fit_injuries_month
```

    ## # A tibble: 72 x 5
    ##    month    term                                 estimate std.error  p.value
    ##    <ord>    <chr>                                   <dbl>     <dbl>    <dbl>
    ##  1 January  (Intercept)                            -1.41     0.0143 0.      
    ##  2 January  2020 v. 2019, Borough: BRONX            0.261    0.0354 1.69e-13
    ##  3 January  2020 v. 2019, Borough: BROOKLYN         0.216    0.0283 2.21e-14
    ##  4 January  2020 v. 2019, Borough: MANHATTAN       -0.332    0.0470 1.63e-12
    ##  5 January  2020 v. 2019, Borough: QUEENS           0.154    0.0294 1.58e- 7
    ##  6 January  2020 v. 2019, Borough: STATEN ISLAND    0.683    0.0739 2.59e-20
    ##  7 February (Intercept)                            -1.44     0.0149 0.      
    ##  8 February 2020 v. 2019, Borough: BRONX            0.240    0.0383 3.77e-10
    ##  9 February 2020 v. 2019, Borough: BROOKLYN         0.179    0.0292 9.46e-10
    ## 10 February 2020 v. 2019, Borough: MANHATTAN       -0.180    0.0438 4.05e- 5
    ## # … with 62 more rows

It seems like what this is looking at is the number of persons injured
per crash, and seeing if there is a significant difference between years
(for each month.) Right? So we’re hypothesizing that each crash would be
more dangerous in 2020, (rather than that there are more crashes?) But
sure enough, seems like 2020 is more dangerous overall, interesting\! -
Emma

``` r
fit_injuries_month %>% 
  filter(term != "(Intercept)" & month != "November" & month != "December") %>%
  mutate(term = str_replace(term, "2020 v. 2019, ", "")) %>%
  ggplot(aes(x = month, y = exp(estimate), color = term)) + 
  geom_point(show.legend = FALSE, aes(size = estimate, alpha = .7)) +
  geom_errorbar(aes(ymin = exp(estimate - (1.96*std.error)), 
                    ymax = exp(estimate + (1.96*std.error)))) +
  geom_hline(yintercept = 1, linetype="dashed", 
                color = "darkred", size = 1, alpha = .7) +
  labs(
    title = "Difference in Rate of Injuries Per Crash in 2020 v. 2019",
    x = "Month",
    y = "2020 v. 2019 Difference"
  ) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
```

![](20201113_import_tidy_files/figure-gfm/plot%20injuries%20by%20month-1.png)<!-- -->

# EDA, Crashes Over Time, Cont’d

``` r
#for the line plot, we will want a summary of the number of bike crashes by day. The collisions variable calculates the number of unique crashes and the bikes variable calculates the total number of bikes that crashed (there can be mutliple bikes involved in one crash but this is rare)
crash_date_summ = 
  crash_dat %>%
  filter(str_detect(vehicle_options, "[Bb]ike")) %>%
  group_by(year, month, day) %>%
  summarize(
    collisions = n_distinct(collision_id),
    bikes = n()
  )
```

    ## `summarise()` regrouping output by 'year', 'month' (override with `.groups` argument)

``` r
#simple plot just to see what the data look like
crash_date_summ %>%
  ggplot(aes(x = paste(month, day, sep="-"), y = collisions, 
             group = year, color = as.factor(year))) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE) + 
  labs(
    title = "Number of Collisions Over Time in January-October of 2019 and 2020",
    x = "Month and Day",
    y = "Number of Collisions"
    ) +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(angle = 60, hjust=1, size=10)) +
  scale_colour_discrete("Year")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](20201113_import_tidy_files/figure-gfm/line%20plot%20of%20crashes%20over%20time-1.png)<!-- -->

``` r
# Make df with aggregate bike counts by day. I noticed that there are some days with NAs - haven't explored yet but might want to look into if the bike counting operation itself was limited at all by covid.
bike_counts_aggregate = 
  bike_dat %>% 
  mutate(
    date_time = date,
    date = lubridate::date(date_time)
  ) %>% 
  group_by(date) %>% 
  summarize(total_daily_bikes = sum(counts, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
# Look at simple graph of bike counts by day, analagous to the one of crashes above.
bike_counts_aggregate %>%
  ggplot(aes(x = date, y = total_daily_bikes)) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](20201113_import_tidy_files/figure-gfm/tidy%20bikes-1.png)<!-- -->

``` r
## Ok, even with the partial fix above, there seem to be bands of missing dates. I think there's something wrong with the import still.
```

# EDA, Contributing Factors

``` r
crash_factor = 
  bikes %>%
  filter(
    month %in% (3:10),
    number_of_cyclist_injured > 0
    ) %>%
  pivot_longer(
    contributing_factor_vehicle_1:contributing_factor_vehicle_5,
    names_to = "vehicle_factor_num",
    names_prefix = "contributing_factor_vehicle_",
    values_to = "contributing_factor"
   ) %>%
  mutate(
    covid = as.factor(if_else(year == 2019, 'Pre-COVID', 'COVID'))
  ) %>%
  group_by(covid, contributing_factor) %>%
  summarise(
    collisions = n_distinct(collision_id)
  ) %>%
  drop_na(contributing_factor) %>%
  ungroup(contributing_factor) %>%
  filter(contributing_factor != "Unspecified") %>%
  mutate(
    covid = fct_relevel(covid, 'Pre-COVID'),
    proportion = round(collisions / sum(collisions), 2) 
  ) 
```

    ## `summarise()` regrouping output by 'covid' (override with `.groups` argument)

``` r
crash_factor %>%
  group_by(covid) %>%
  top_n(n = 10, wt = proportion) %>%
  mutate(
    contributing_factor = fct_reorder(as.factor(contributing_factor), collisions, .desc = TRUE)
  ) %>%
  ggplot(aes(x = contributing_factor, y = proportion, fill = covid)) +
  geom_bar(stat = "identity") +
  facet_grid( . ~covid, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none") +
  labs(
    title = "Top Ten Contributing Factors for Crashes Involving Injuries to Cyclists",
    x = "",
    y = "Proportion of Crashes"
  )
```

![](20201113_import_tidy_files/figure-gfm/contributing_factor-1.png)<!-- -->

# Visualizations: Maps

## Location Data

This is my initial thought on how this map might look… but I want to see
about the density functions. I could change the size of the marker based
on \# people injured, or the color of the marker based on \# people
injured. The boroughs adds a nice touch too.

### Crashes with Injuries by Borough

``` r
crash_map_boro = 
  crash_dat %>% 
  drop_na(c(latitude, longitude, borough)) %>% 
  filter(str_detect(vehicle_options, "[Bb]ike"))  %>% 
  filter(number_of_cyclist_injured > 0) %>% 
  mutate(
    text_label = str_c("Crash Time: ", crash_time, "\nNumber of Persons Injured: ", number_of_persons_injured)) %>% 
  plot_mapbox(
    x = ~longitude, 
    y = ~latitude,  
    mode = 'scattermapbox', 
    split = ~borough,
    size = ~number_of_persons_injured,
    color = ~borough,
    text = ~text_label,
    hoverinfo = "text") %>% 
  layout(
    mapbox = 
      list(
        style = "dark",
        zoom = 9.5,
        center = list(lat = 40.71, lon = -73.97)))

crash_map_boro %>% 
  layout(
    title = "Bicycle Crashes with Injuries in New York City (2019-2020)")
```

    ## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
    ## Please use `arrange()` instead.
    ## See vignette('programming') for more help
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `line.width` does not currently support multiple values.
    
    ## Warning: `line.width` does not currently support multiple values.
    
    ## Warning: `line.width` does not currently support multiple values.
    
    ## Warning: `line.width` does not currently support multiple values.
    
    ## Warning: `line.width` does not currently support multiple values.

![](20201113_import_tidy_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Density

Playing around with the plot\_ly density functions.  
Here are the areas most prone to bicycle accidents in 2019

### Density of Bicycle Crashes in 2019

``` r
crash_map_density = 
  crash_dat %>% 
  drop_na(c(latitude, longitude)) %>% 
  filter(str_detect(vehicle_options, "[Bb]ike"))  %>% 
  filter(number_of_cyclist_injured > 0) %>% 
  mutate(
    text_label = str_c("Crash Time: ", crash_time, "\nNumber of Persons Injured: ", number_of_persons_injured)) %>% 
  plot_ly(
    lon = ~longitude, 
    lat = ~latitude,  
    type = 'densitymapbox',
    symbol = 'square',
    radius = 3) %>% 
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom = 9.5,
      center = list(lat = 40.71, lon = -73.97)))

crash_map_density %>% 
  layout(
    style = 'open-street-map',
    title = "Density of Bicycle Crashes with Injuries in New York City in 2019") %>% 
  colorbar(title = "Density of Bicycle Crashes")
```

    ## Adding markers to mode; otherwise symbol would have no effect.

    ## Warning: 'layout' objects don't have these attributes: 'style'
    ## Valid attributes include:
    ## 'font', 'title', 'uniformtext', 'autosize', 'width', 'height', 'margin', 'paper_bgcolor', 'plot_bgcolor', 'separators', 'hidesources', 'showlegend', 'colorway', 'datarevision', 'uirevision', 'editrevision', 'selectionrevision', 'template', 'modebar', 'meta', 'transition', '_deprecated', 'clickmode', 'dragmode', 'hovermode', 'hoverdistance', 'spikedistance', 'hoverlabel', 'selectdirection', 'grid', 'calendar', 'xaxis', 'yaxis', 'ternary', 'scene', 'geo', 'mapbox', 'polar', 'radialaxis', 'angularaxis', 'direction', 'orientation', 'editType', 'legend', 'annotations', 'shapes', 'images', 'updatemenus', 'sliders', 'colorscale', 'coloraxis', 'metasrc', 'barmode', 'bargap', 'mapType'

    ## Warning: 'densitymapbox' objects don't have these attributes: 'marker', 'mode'
    ## Valid attributes include:
    ## 'type', 'visible', 'legendgroup', 'opacity', 'name', 'uid', 'ids', 'customdata', 'meta', 'hoverlabel', 'stream', 'transforms', 'uirevision', 'lon', 'lat', 'z', 'radius', 'below', 'text', 'hovertext', 'hoverinfo', 'hovertemplate', 'showlegend', 'zauto', 'zmin', 'zmax', 'zmid', 'colorscale', 'autocolorscale', 'reversescale', 'showscale', 'colorbar', 'coloraxis', 'subplot', 'idssrc', 'customdatasrc', 'metasrc', 'lonsrc', 'latsrc', 'zsrc', 'radiussrc', 'textsrc', 'hovertextsrc', 'hoverinfosrc', 'hovertemplatesrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'

    ## Warning: Didn't find a colorbar to modify.

    ## Warning: 'layout' objects don't have these attributes: 'style'
    ## Valid attributes include:
    ## 'font', 'title', 'uniformtext', 'autosize', 'width', 'height', 'margin', 'paper_bgcolor', 'plot_bgcolor', 'separators', 'hidesources', 'showlegend', 'colorway', 'datarevision', 'uirevision', 'editrevision', 'selectionrevision', 'template', 'modebar', 'meta', 'transition', '_deprecated', 'clickmode', 'dragmode', 'hovermode', 'hoverdistance', 'spikedistance', 'hoverlabel', 'selectdirection', 'grid', 'calendar', 'xaxis', 'yaxis', 'ternary', 'scene', 'geo', 'mapbox', 'polar', 'radialaxis', 'angularaxis', 'direction', 'orientation', 'editType', 'legend', 'annotations', 'shapes', 'images', 'updatemenus', 'sliders', 'colorscale', 'coloraxis', 'metasrc', 'barmode', 'bargap', 'mapType'

    ## Warning: 'densitymapbox' objects don't have these attributes: 'marker', 'mode'
    ## Valid attributes include:
    ## 'type', 'visible', 'legendgroup', 'opacity', 'name', 'uid', 'ids', 'customdata', 'meta', 'hoverlabel', 'stream', 'transforms', 'uirevision', 'lon', 'lat', 'z', 'radius', 'below', 'text', 'hovertext', 'hoverinfo', 'hovertemplate', 'showlegend', 'zauto', 'zmin', 'zmax', 'zmid', 'colorscale', 'autocolorscale', 'reversescale', 'showscale', 'colorbar', 'coloraxis', 'subplot', 'idssrc', 'customdatasrc', 'metasrc', 'lonsrc', 'latsrc', 'zsrc', 'radiussrc', 'textsrc', 'hovertextsrc', 'hoverinfosrc', 'hovertemplatesrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'

![](20201113_import_tidy_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
