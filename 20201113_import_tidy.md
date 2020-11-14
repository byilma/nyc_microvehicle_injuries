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
crash_api = function(offset) {
  GET("https://data.cityofnewyork.us/resource/h9gi-nx95.csv", 
      query = list("$where" = "crash_date between '2020-01-01T00:00:00' and '2020-10-31T12:00:00'", "$limit" = 50000, "$offset" = offset)) %>% 
  content("parsed") %>%
  as_tibble() 
}

offsets = c(0, 50001)
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
bike_api = function(offset) {
  GET("https://data.cityofnewyork.us/resource/uczf-rk3c.csv", 
      query = list("$where" = "date between '2020-01-01T00:00:00' and '2020-10-31T12:00:00'", "$limit" = 50000, "$offset" = offset)) %>% 
  content("parsed") %>%
  as_tibble() 
}

offsets = c(0, 50001, 100001, 150001, 200001, 250001, 300001, 350001, 400001)
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
