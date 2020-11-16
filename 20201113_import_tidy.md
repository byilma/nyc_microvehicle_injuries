Pull and Tidy
================
11/6/2020

``` r
crash_dat = 
  GET("https://data.cityofnewyork.us/resource/h9gi-nx95.csv", 
      query = list("$limit" = 5000)) %>% 
  content("parsed") %>%
  as_tibble()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   crash_date = col_datetime(format = ""),
    ##   crash_time = col_time(format = ""),
    ##   zip_code = col_double(),
    ##   number_of_persons_injured = col_double(),
    ##   number_of_persons_killed = col_double(),
    ##   number_of_pedestrians_injured = col_double(),
    ##   number_of_pedestrians_killed = col_double(),
    ##   number_of_cyclist_injured = col_double(),
    ##   number_of_cyclist_killed = col_double(),
    ##   number_of_motorist_injured = col_double(),
    ##   number_of_motorist_killed = col_double(),
    ##   collision_id = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
#Importing crash data using the NYC Open Data API. The API response is limited to 50,000 rows, so we need a function that can be used to page through these results to capture all crashes in 2020. 

crash_api = function(offset) {
  GET("https://data.cityofnewyork.us/resource/h9gi-nx95.csv", 
      query = list("$where" = "crash_date between '2020-01-01T00:00:00' and '2020-10-31T12:00:00'", "$limit" = 50000, "$offset" = offset)) %>% 
  content("parsed") %>%
  as_tibble() 
}

offsets = c(0, 50000)
crash_dat_2020 = map_df(offsets, crash_api)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   crash_date = col_datetime(format = ""),
    ##   crash_time = col_time(format = ""),
    ##   zip_code = col_double(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   number_of_persons_injured = col_double(),
    ##   number_of_persons_killed = col_double(),
    ##   number_of_pedestrians_injured = col_double(),
    ##   number_of_pedestrians_killed = col_double(),
    ##   number_of_cyclist_injured = col_double(),
    ##   number_of_cyclist_killed = col_double(),
    ##   number_of_motorist_injured = col_double(),
    ##   number_of_motorist_killed = col_double(),
    ##   collision_id = col_double()
    ## )
    ## See spec(...) for full column specifications.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   crash_date = col_datetime(format = ""),
    ##   crash_time = col_time(format = ""),
    ##   zip_code = col_double(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   number_of_persons_injured = col_double(),
    ##   number_of_persons_killed = col_double(),
    ##   number_of_pedestrians_injured = col_double(),
    ##   number_of_pedestrians_killed = col_double(),
    ##   number_of_cyclist_injured = col_double(),
    ##   number_of_cyclist_killed = col_double(),
    ##   number_of_motorist_injured = col_double(),
    ##   number_of_motorist_killed = col_double(),
    ##   collision_id = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
#cleaning - transpose so vehicle types are listed in one column
crash_dat = 
  crash_dat_2020 %>%
  pivot_longer(
    vehicle_type_code1:vehicle_type_code_5,
    names_to = "vehicle_number",
    values_to = "vehicle_options"
   ) %>%
  drop_na(vehicle_options)


#Exploring the vehicle types so that we can limit to bicycles -- double check this list
bikes = 
  crash_dat %>%
  distinct(vehicle_options) %>%
  filter(str_detect(vehicle_options, "[Bb]ike")) 
  
  
#for the line plot, we will want a summary of the number of bike crashes by day. The collisions variable calculates the number of unique crashes and the bikes variable calculates the total number of bikes that crashed (there can be mutliple bikes involved in one crash but this is rare)
crash_date_summ = 
  crash_dat %>%
  filter(str_detect(vehicle_options, "[Bb]ike")) %>%
  group_by(crash_date) %>%
  summarize(
    collisions = n_distinct(collision_id),
    bikes = n()
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#simple plot just to see what the data look like
crash_date_summ %>%
  ggplot(aes(x = crash_date, y = collisions)) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](20201113_import_tidy_files/figure-gfm/import%20and%20tidy%20MV%20crash-1.png)<!-- -->

``` r
bike_dat = 
  GET("https://data.cityofnewyork.us/resource/uczf-rk3c.csv", 
      query = list("$limit" = 55000)) %>% 
  content("parsed") %>%
  as_tibble()
```

    ## Parsed with column specification:
    ## cols(
    ##   id1 = col_double(),
    ##   counts = col_double(),
    ##   date = col_datetime(format = ""),
    ##   status = col_double(),
    ##   site = col_double()
    ## )

``` r
#Importing bike data using the NYC Open Data API. The API response is limited to 50,000 rows, so we need a function that can be used to page through these results to capture all bike counts in 2020. 

bike_api = function(offset) {
  GET("https://data.cityofnewyork.us/resource/uczf-rk3c.csv", 
      query = list("$where" = "date between '2020-01-01T00:00:00' and '2020-10-31T12:00:00'", "$limit" = 50000, "$offset" = offset)) %>% 
  content("parsed") %>%
  as_tibble() 
}

offsets = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000)
bike_dat_2020 = map_df(offsets, bike_api)
```

    ## Parsed with column specification:
    ## cols(
    ##   id1 = col_double(),
    ##   counts = col_double(),
    ##   date = col_datetime(format = ""),
    ##   status = col_double(),
    ##   site = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   id1 = col_double(),
    ##   counts = col_double(),
    ##   date = col_datetime(format = ""),
    ##   status = col_double(),
    ##   site = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   id1 = col_double(),
    ##   counts = col_double(),
    ##   date = col_datetime(format = ""),
    ##   status = col_double(),
    ##   site = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   id1 = col_double(),
    ##   counts = col_double(),
    ##   date = col_datetime(format = ""),
    ##   status = col_double(),
    ##   site = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   id1 = col_double(),
    ##   counts = col_double(),
    ##   date = col_datetime(format = ""),
    ##   status = col_double(),
    ##   site = col_double()
    ## )
