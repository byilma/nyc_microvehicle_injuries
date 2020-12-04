Rates of Injury
================
12/2/2020

``` r
crash_dat_tidy = read_csv("./data/crash_dat.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   date = col_date(format = ""),
    ##   crash_time = col_time(format = ""),
    ##   borough = col_character(),
    ##   location = col_character(),
    ##   on_street_name = col_character(),
    ##   off_street_name = col_character(),
    ##   cross_street_name = col_character(),
    ##   contributing_factor_vehicle_1 = col_character(),
    ##   contributing_factor_vehicle_2 = col_character(),
    ##   contributing_factor_vehicle_3 = col_character(),
    ##   contributing_factor_vehicle_4 = col_character(),
    ##   contributing_factor_vehicle_5 = col_character(),
    ##   dow = col_character(),
    ##   vehicle_number = col_character(),
    ##   vehicle_options = col_character()
    ## )

    ## See spec(...) for full column specifications.

**My goal was to compare the rate of injuries in a crash in 2020 to the
same month in 2019 (e.g., rate of injuries in January 2020 v. rate of
injuries in January 2019). I examined January through October. I
additionally stratified these rate ratio estimates by borough.**

**To accomplish this, I filtered the tidy data to create one dataset for
microvehicles and one for bikes. In each dataset, I then created nested
datasets by month, and, in each of these datasets, I mapped Poisson
models to extract rate ratio estimates for number of injuries in each
borough. Finally, I unnessted these models to extract the desired
coefficients: rate ratios and standard errors (used to compute 95%
confidence intervals).**

**Below, I write functions to easily filter for either microvehicles or
for bikes.**

``` r
filter_for = function(dataset, type) {
  if (type == "micro") {
  dataset %>% filter(str_detect(vehicle_options, "[Bb]ike") | 
           str_detect(vehicle_options, "REVEL") | 
           str_detect(vehicle_options, "SCO")  |
           str_detect(vehicle_options, "MOP")   |
           str_detect(vehicle_options, "ELEC")  |
           str_detect(vehicle_options, "^E-")) %>% 
  filter(vehicle_options != "ESCOVATOR" & vehicle_options != "Bike" &
      str_detect(vehicle_options, "Dirt", negate = TRUE),
      str_detect(vehicle_options, "[Mm]otorbike", negate = TRUE)
           )
}

  else if (type == "bike") {
  dataset %>% filter(str_starts(vehicle_options, "[Bb]ike")) 
  }
}
```

**First, I tested to see if there was an overall association of 2020 v.
2019 with number of injuries in microvehicle crashes.**

``` r
month_df=
  tibble(
    month = 1:12,
    month_name = factor(month.name, ordered = TRUE, levels = month.name)
  )
```

``` r
fit_injuries_micro_all = crash_dat_tidy %>%
  filter_for("micro") %>% 
  filter(year %in% c(2019, 2020)) %>%
  group_by(year) %>%
  mutate(year_2020 = year - 2019) %>%
  nest(data = -month) %>%
  mutate(models = map(data, ~glm(number_of_persons_injured ~ year_2020,
                                 family = "poisson", data = .x)),
         models = map(models, broom::tidy)) %>% 
  select(-data) %>% 
  unnest(models) %>%
  select(month, term, estimate, std.error, p.value) %>% 
  mutate(term = str_replace(term, "year_2020", "2020 v. 2019")) %>%
  left_join(month_df, by = "month") %>%
  select(-month) %>%
  rename(month = month_name) %>%
  filter(term != "(Intercept)" & month != "November" & month != "December") %>%
  select(month, everything()) 

fit_injuries_micro_all
```

    ## # A tibble: 10 x 5
    ##    month     term         estimate std.error p.value
    ##    <ord>     <chr>           <dbl>     <dbl>   <dbl>
    ##  1 January   2020 v. 2019  -0.288      0.401   0.473
    ##  2 February  2020 v. 2019   0.744      0.717   0.300
    ##  3 March     2020 v. 2019  -0.431      0.318   0.175
    ##  4 April     2020 v. 2019   0.140      0.416   0.737
    ##  5 May       2020 v. 2019  -0.225      0.264   0.394
    ##  6 June      2020 v. 2019  -0.115      0.227   0.613
    ##  7 July      2020 v. 2019   0.131      0.163   0.423
    ##  8 August    2020 v. 2019   0.0720     0.110   0.513
    ##  9 September 2020 v. 2019   0.0437     0.109   0.687
    ## 10 October   2020 v. 2019  -0.0576     0.118   0.626

**Although there was no statistically significant evidence for a
different rate of injury in 2020 v. 21 in any month, I proceeded to
examine this stratified by borough.**

**I also wanted to get descriptives for number of microvehicle crashes
and injuries in each borough in each month.**

``` r
micro_num = crash_dat_tidy %>%
  filter_for("micro") %>% 
  filter(year %in% c(2019, 2020)) %>%
  group_by(year, month, borough) %>%
  summarise(number_of_crashes = n(),
            mean_number_of_injuries = mean(number_of_persons_injured)) %>%
  drop_na(borough) %>%
  left_join(month_df, by = "month") %>%
  as.tibble() %>%
  mutate(borough = str_to_title(borough))
```

    ## `summarise()` regrouping output by 'year', 'month' (override with `.groups` argument)

``` r
micro_num %>%
  select(year, month_name, borough, number_of_crashes, 
         mean_number_of_injuries) %>%
  knitr::kable(digits = 2)
```

| year | month\_name | borough       | number\_of\_crashes | mean\_number\_of\_injuries |
| ---: | :---------- | :------------ | ------------------: | -------------------------: |
| 2019 | January     | Brooklyn      |                   2 |                       1.00 |
| 2019 | January     | Manhattan     |                   1 |                       1.00 |
| 2019 | January     | Queens        |                   1 |                       1.00 |
| 2019 | February    | Bronx         |                   2 |                       0.50 |
| 2019 | February    | Brooklyn      |                   2 |                       0.00 |
| 2019 | March       | Bronx         |                   3 |                       1.33 |
| 2019 | March       | Brooklyn      |                   3 |                       1.00 |
| 2019 | March       | Queens        |                   1 |                       1.00 |
| 2019 | April       | Bronx         |                   1 |                       0.00 |
| 2019 | April       | Brooklyn      |                   1 |                       2.00 |
| 2019 | April       | Queens        |                   5 |                       0.60 |
| 2019 | May         | Bronx         |                   7 |                       1.00 |
| 2019 | May         | Brooklyn      |                   2 |                       1.00 |
| 2019 | May         | Manhattan     |                   2 |                       1.00 |
| 2019 | May         | Queens        |                   4 |                       0.75 |
| 2019 | June        | Bronx         |                   5 |                       0.80 |
| 2019 | June        | Brooklyn      |                   9 |                       0.67 |
| 2019 | June        | Manhattan     |                   1 |                       3.00 |
| 2019 | June        | Queens        |                   4 |                       0.75 |
| 2019 | July        | Bronx         |                   9 |                       0.78 |
| 2019 | July        | Brooklyn      |                  21 |                       0.81 |
| 2019 | July        | Manhattan     |                   5 |                       0.80 |
| 2019 | July        | Queens        |                  10 |                       0.90 |
| 2019 | August      | Bronx         |                  21 |                       0.86 |
| 2019 | August      | Brooklyn      |                  50 |                       0.82 |
| 2019 | August      | Manhattan     |                  21 |                       0.81 |
| 2019 | August      | Queens        |                  18 |                       0.83 |
| 2019 | September   | Bronx         |                  27 |                       0.74 |
| 2019 | September   | Brooklyn      |                  54 |                       0.76 |
| 2019 | September   | Manhattan     |                  25 |                       0.92 |
| 2019 | September   | Queens        |                  14 |                       0.71 |
| 2019 | October     | Bronx         |                  11 |                       0.91 |
| 2019 | October     | Brooklyn      |                  38 |                       0.92 |
| 2019 | October     | Manhattan     |                  15 |                       0.93 |
| 2019 | October     | Queens        |                  23 |                       1.00 |
| 2019 | November    | Bronx         |                  18 |                       0.72 |
| 2019 | November    | Brooklyn      |                  29 |                       0.55 |
| 2019 | November    | Manhattan     |                  13 |                       0.77 |
| 2019 | November    | Queens        |                  15 |                       0.93 |
| 2019 | December    | Bronx         |                   7 |                       0.71 |
| 2019 | December    | Brooklyn      |                  26 |                       0.77 |
| 2019 | December    | Manhattan     |                  11 |                       0.82 |
| 2019 | December    | Queens        |                  15 |                       1.00 |
| 2020 | January     | Bronx         |                  11 |                       0.73 |
| 2020 | January     | Brooklyn      |                  22 |                       0.91 |
| 2020 | January     | Manhattan     |                  14 |                       0.71 |
| 2020 | January     | Queens        |                  10 |                       0.80 |
| 2020 | February    | Bronx         |                  11 |                       0.91 |
| 2020 | February    | Brooklyn      |                  23 |                       0.87 |
| 2020 | February    | Manhattan     |                  15 |                       0.67 |
| 2020 | February    | Queens        |                  15 |                       0.67 |
| 2020 | March       | Bronx         |                  19 |                       0.63 |
| 2020 | March       | Brooklyn      |                  18 |                       0.61 |
| 2020 | March       | Manhattan     |                  16 |                       0.81 |
| 2020 | March       | Queens        |                   4 |                       1.00 |
| 2020 | April       | Bronx         |                  11 |                       0.73 |
| 2020 | April       | Brooklyn      |                   4 |                       0.75 |
| 2020 | April       | Manhattan     |                   4 |                       1.25 |
| 2020 | April       | Queens        |                   9 |                       1.00 |
| 2020 | May         | Bronx         |                  24 |                       0.58 |
| 2020 | May         | Brooklyn      |                  25 |                       0.76 |
| 2020 | May         | Manhattan     |                  20 |                       0.85 |
| 2020 | May         | Queens        |                  13 |                       0.69 |
| 2020 | June        | Bronx         |                  49 |                       0.76 |
| 2020 | June        | Brooklyn      |                  35 |                       0.97 |
| 2020 | June        | Manhattan     |                  45 |                       0.76 |
| 2020 | June        | Queens        |                  23 |                       0.70 |
| 2020 | July        | Bronx         |                  68 |                       0.85 |
| 2020 | July        | Brooklyn      |                  63 |                       0.89 |
| 2020 | July        | Manhattan     |                  54 |                       0.94 |
| 2020 | July        | Queens        |                  26 |                       0.88 |
| 2020 | July        | Staten Island |                   1 |                       1.00 |
| 2020 | August      | Bronx         |                  57 |                       0.84 |
| 2020 | August      | Brooklyn      |                  52 |                       0.96 |
| 2020 | August      | Manhattan     |                  40 |                       0.78 |
| 2020 | August      | Queens        |                  28 |                       1.00 |
| 2020 | August      | Staten Island |                   1 |                       1.00 |
| 2020 | September   | Bronx         |                  66 |                       0.85 |
| 2020 | September   | Brooklyn      |                  76 |                       0.78 |
| 2020 | September   | Manhattan     |                  50 |                       0.76 |
| 2020 | September   | Queens        |                  37 |                       0.78 |
| 2020 | September   | Staten Island |                   2 |                       1.00 |
| 2020 | October     | Bronx         |                  53 |                       0.87 |
| 2020 | October     | Brooklyn      |                  67 |                       0.79 |
| 2020 | October     | Manhattan     |                  49 |                       0.78 |
| 2020 | October     | Queens        |                  33 |                       0.91 |
| 2020 | October     | Staten Island |                   1 |                       0.00 |

``` r
micro_num %>%
  mutate(date = as.yearmon(paste(year, month, sep = "-"))) %>%
  ggplot(aes(x = date, y = number_of_crashes, fill = borough)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Number of Crashes by Borough",
    x = "Month",
    y = "Number of Crashes"
  ) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  facet_grid(. ~ borough)
```

![](20201125_InjuryPlots_files/figure-gfm/histogram%20for%20n%20microvehicle%20crashes-1.png)<!-- -->

**It is apparent that there are very low numbers of microvehicle crashes
when stratifying by both month and borough - sometimes as low as only 1
crash, or even 0 crashes (particularly in Staten Island).**

**Below, I extract the rate ratio estimates for the microvehicle data by
borough.**

``` r
fit_injuries_micro = crash_dat_tidy %>%
  filter_for("micro") %>% 
  filter(year %in% c(2019, 2020)) %>%
  mutate(borough = str_to_title(borough)) %>%
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

fit_injuries_micro %>% 
  filter(term != "(Intercept)" & month != "November" & month != "December" & p.value < .05)
```

    ## # A tibble: 0 x 5
    ## # … with 5 variables: month <ord>, term <chr>, estimate <dbl>, std.error <dbl>,
    ## #   p.value <dbl>

**Here, I extract my coefficient of interest, which is the difference in
injury rate in microvehicle crashes occurring in 2020 v. occurring in
2019. I plot estimates for each borough within each month, making sure
to exponentiate these estimates. I include a horizontal line at Y = 1 to
indicate the null value.**

``` r
fit_injuries_micro %>% 
  filter(term != "(Intercept)" & month != "November" & month != "December") %>%
  mutate(term = str_replace(term, "2020 v. 2019, ", "")) %>%
  ggplot(aes(x = month, y = exp(estimate), color = term)) + 
  geom_point(show.legend = FALSE, aes(size = estimate, alpha = .7)) +
  geom_errorbar(aes(ymin = exp(estimate - (1.96*std.error)), 
                    ymax = exp(estimate + (1.96*std.error)))) +
  geom_hline(yintercept = 1, linetype="dashed", 
                color = "darkred", size = 1, alpha = .7) +
  labs(
    title = "Difference in Rate of Injuries Per Crash Involving a Microvehicle in 2020 v. 2019",
    x = "Month",
    y = "2020 v. 2019 Difference"
  ) +
  ylim(0, 5) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + 
  facet_grid(. ~ term)
```

![](20201125_InjuryPlots_files/figure-gfm/plot%20microvehicle%20injuries%20by%20month-1.png)<!-- -->

**There is no discernable pattern in the figure, suggesting that rates
of injury per microvehicle crash did not differ between 2020 and 2019.
In part, this could be due to very wide confidence intervals.**

**Again, I first tested to see if there was an overall association of
2020 v. 2019 with number of injuries in bike crashes (as in the
microvehicle data).**

``` r
fit_injuries_bike_all = crash_dat_tidy %>%
  filter_for("bike") %>% 
  filter(year %in% c(2019, 2020)) %>%
  group_by(year) %>%
  mutate(year_2020 = year - 2019) %>%
  nest(data = -month) %>%
  mutate(models = map(data, ~glm(number_of_persons_injured ~ year_2020,
                                 family = "poisson", data = .x)),
         models = map(models, broom::tidy)) %>% 
  select(-data) %>% 
  unnest(models) %>%
  select(month, term, estimate, std.error, p.value) %>% 
  mutate(term = str_replace(term, "year_2020", "2020 v. 2019")) %>%
  left_join(month_df, by = "month") %>%
  select(-month) %>%
  rename(month = month_name) %>%
  filter(term != "(Intercept)" & month != "November" & month != "December") %>%
  select(month, everything()) 

fit_injuries_bike_all
```

    ## # A tibble: 10 x 5
    ##    month     term         estimate std.error p.value
    ##    <ord>     <chr>           <dbl>     <dbl>   <dbl>
    ##  1 January   2020 v. 2019  0.0130     0.0940   0.890
    ##  2 February  2020 v. 2019  0.0275     0.0967   0.776
    ##  3 March     2020 v. 2019  0.131      0.0873   0.132
    ##  4 April     2020 v. 2019  0.00546    0.102    0.957
    ##  5 May       2020 v. 2019  0.00906    0.0702   0.897
    ##  6 June      2020 v. 2019  0.0235     0.0588   0.690
    ##  7 July      2020 v. 2019  0.0535     0.0576   0.352
    ##  8 August    2020 v. 2019  0.0380     0.0581   0.513
    ##  9 September 2020 v. 2019  0.0426     0.0594   0.473
    ## 10 October   2020 v. 2019  0.0780     0.0671   0.245

**Although there was no statistically significant evidence for a
different rate of injury in 2020 v. 21 in any month, I proceeded to
examine this stratified by borough.**

**I also wanted to get descriptives for number of bike crashes and
injuries in each borough in each month.**

``` r
bike_num = crash_dat_tidy %>%
  filter_for("bike") %>% 
  filter(year %in% c(2019, 2020)) %>%
  group_by(year, month, borough) %>%
  summarise(number_of_crashes = n(),
            mean_number_of_injuries = mean(number_of_persons_injured)) %>%
  drop_na(borough) %>%
  left_join(month_df, by = "month") %>%
  as.tibble() %>%
  mutate(borough = str_to_title(borough))
```

    ## `summarise()` regrouping output by 'year', 'month' (override with `.groups` argument)

``` r
bike_num %>%
  select(year, month_name, borough, number_of_crashes, 
         mean_number_of_injuries) %>%
  knitr::kable(digits = 2)
```

| year | month\_name | borough       | number\_of\_crashes | mean\_number\_of\_injuries |
| ---: | :---------- | :------------ | ------------------: | -------------------------: |
| 2019 | January     | Bronx         |                  17 |                       0.76 |
| 2019 | January     | Brooklyn      |                  74 |                       0.85 |
| 2019 | January     | Manhattan     |                  67 |                       0.76 |
| 2019 | January     | Queens        |                  41 |                       0.88 |
| 2019 | January     | Staten Island |                   1 |                       0.00 |
| 2019 | February    | Bronx         |                  18 |                       0.83 |
| 2019 | February    | Brooklyn      |                  67 |                       0.78 |
| 2019 | February    | Manhattan     |                  90 |                       0.69 |
| 2019 | February    | Queens        |                  28 |                       0.89 |
| 2019 | February    | Staten Island |                   2 |                       1.00 |
| 2019 | March       | Bronx         |                  19 |                       0.89 |
| 2019 | March       | Brooklyn      |                  99 |                       0.76 |
| 2019 | March       | Manhattan     |                 111 |                       0.68 |
| 2019 | March       | Queens        |                  42 |                       0.79 |
| 2019 | March       | Staten Island |                   1 |                       1.00 |
| 2019 | April       | Bronx         |                  34 |                       0.79 |
| 2019 | April       | Brooklyn      |                 120 |                       0.87 |
| 2019 | April       | Manhattan     |                 140 |                       0.81 |
| 2019 | April       | Queens        |                  70 |                       0.89 |
| 2019 | April       | Staten Island |                   5 |                       1.00 |
| 2019 | May         | Bronx         |                  58 |                       0.83 |
| 2019 | May         | Brooklyn      |                 159 |                       0.86 |
| 2019 | May         | Manhattan     |                 143 |                       0.74 |
| 2019 | May         | Queens        |                  82 |                       0.93 |
| 2019 | May         | Staten Island |                   5 |                       0.80 |
| 2019 | June        | Bronx         |                  55 |                       0.87 |
| 2019 | June        | Brooklyn      |                 190 |                       0.89 |
| 2019 | June        | Manhattan     |                 149 |                       0.74 |
| 2019 | June        | Queens        |                 105 |                       0.91 |
| 2019 | June        | Staten Island |                   6 |                       0.83 |
| 2019 | July        | Bronx         |                  73 |                       0.84 |
| 2019 | July        | Brooklyn      |                 211 |                       0.89 |
| 2019 | July        | Manhattan     |                 174 |                       0.75 |
| 2019 | July        | Queens        |                  94 |                       0.80 |
| 2019 | July        | Staten Island |                  10 |                       0.90 |
| 2019 | August      | Bronx         |                  43 |                       0.91 |
| 2019 | August      | Brooklyn      |                 169 |                       0.93 |
| 2019 | August      | Manhattan     |                 155 |                       0.70 |
| 2019 | August      | Queens        |                  93 |                       0.87 |
| 2019 | August      | Staten Island |                   5 |                       1.00 |
| 2019 | September   | Bronx         |                  40 |                       0.82 |
| 2019 | September   | Brooklyn      |                 176 |                       0.89 |
| 2019 | September   | Manhattan     |                 151 |                       0.72 |
| 2019 | September   | Queens        |                  75 |                       0.80 |
| 2019 | September   | Staten Island |                   7 |                       0.71 |
| 2019 | October     | Bronx         |                  38 |                       0.71 |
| 2019 | October     | Brooklyn      |                 133 |                       0.83 |
| 2019 | October     | Manhattan     |                 151 |                       0.72 |
| 2019 | October     | Queens        |                  77 |                       0.88 |
| 2019 | October     | Staten Island |                   1 |                       1.00 |
| 2019 | November    | Bronx         |                  19 |                       0.89 |
| 2019 | November    | Brooklyn      |                  93 |                       0.74 |
| 2019 | November    | Manhattan     |                 107 |                       0.74 |
| 2019 | November    | Queens        |                  50 |                       0.82 |
| 2019 | November    | Staten Island |                   1 |                       1.00 |
| 2019 | December    | Bronx         |                  21 |                       0.71 |
| 2019 | December    | Brooklyn      |                  70 |                       0.87 |
| 2019 | December    | Manhattan     |                  69 |                       0.67 |
| 2019 | December    | Queens        |                  31 |                       0.84 |
| 2019 | December    | Staten Island |                   4 |                       1.00 |
| 2020 | January     | Bronx         |                  16 |                       0.94 |
| 2020 | January     | Brooklyn      |                  65 |                       0.83 |
| 2020 | January     | Manhattan     |                  80 |                       0.71 |
| 2020 | January     | Queens        |                  43 |                       0.91 |
| 2020 | February    | Bronx         |                  23 |                       0.96 |
| 2020 | February    | Brooklyn      |                  58 |                       0.83 |
| 2020 | February    | Manhattan     |                  75 |                       0.68 |
| 2020 | February    | Queens        |                  31 |                       0.87 |
| 2020 | March       | Bronx         |                  19 |                       0.89 |
| 2020 | March       | Brooklyn      |                  88 |                       0.98 |
| 2020 | March       | Manhattan     |                  75 |                       0.73 |
| 2020 | March       | Queens        |                  30 |                       0.83 |
| 2020 | March       | Staten Island |                   5 |                       0.80 |
| 2020 | April       | Bronx         |                  14 |                       0.71 |
| 2020 | April       | Brooklyn      |                  46 |                       0.74 |
| 2020 | April       | Manhattan     |                  28 |                       0.89 |
| 2020 | April       | Queens        |                  19 |                       0.79 |
| 2020 | April       | Staten Island |                   4 |                       0.75 |
| 2020 | May         | Bronx         |                  36 |                       0.86 |
| 2020 | May         | Brooklyn      |                  88 |                       0.90 |
| 2020 | May         | Manhattan     |                  72 |                       0.81 |
| 2020 | May         | Queens        |                  59 |                       0.85 |
| 2020 | May         | Staten Island |                   5 |                       0.40 |
| 2020 | June        | Bronx         |                  67 |                       0.88 |
| 2020 | June        | Brooklyn      |                 150 |                       0.91 |
| 2020 | June        | Manhattan     |                 116 |                       0.78 |
| 2020 | June        | Queens        |                 107 |                       0.95 |
| 2020 | June        | Staten Island |                  11 |                       1.09 |
| 2020 | July        | Bronx         |                  56 |                       0.80 |
| 2020 | July        | Brooklyn      |                 181 |                       0.88 |
| 2020 | July        | Manhattan     |                 113 |                       0.85 |
| 2020 | July        | Queens        |                 103 |                       1.01 |
| 2020 | July        | Staten Island |                   5 |                       1.00 |
| 2020 | August      | Bronx         |                  70 |                       0.94 |
| 2020 | August      | Brooklyn      |                 207 |                       0.93 |
| 2020 | August      | Manhattan     |                 102 |                       0.78 |
| 2020 | August      | Queens        |                 108 |                       0.86 |
| 2020 | August      | Staten Island |                   8 |                       0.62 |
| 2020 | September   | Bronx         |                  47 |                       0.91 |
| 2020 | September   | Brooklyn      |                 182 |                       0.91 |
| 2020 | September   | Manhattan     |                 127 |                       0.81 |
| 2020 | September   | Queens        |                 124 |                       0.84 |
| 2020 | September   | Staten Island |                   6 |                       0.83 |
| 2020 | October     | Bronx         |                  36 |                       0.89 |
| 2020 | October     | Brooklyn      |                 162 |                       0.89 |
| 2020 | October     | Manhattan     |                 113 |                       0.80 |
| 2020 | October     | Queens        |                  82 |                       0.85 |
| 2020 | October     | Staten Island |                   6 |                       0.67 |

``` r
bike_num %>%
  mutate(date = as.yearmon(paste(year, month, sep = "-"))) %>%
  ggplot(aes(x = date, y = number_of_crashes, fill = borough)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Number of Bike Crashes by Borough",
    x = "Month",
    y = "Number of Crashes"
  ) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  facet_grid(. ~ borough)
```

![](20201125_InjuryPlots_files/figure-gfm/histogram%20for%20n%20bike%20crashes-1.png)<!-- -->

**It is again apparent that there are very low numbers of crashes when
stratifying by both month and borough.**

**Below, I extract the rate ratio estimates for the bike data by borough
(as in the microvehicle data).**

``` r
fit_injuries_bike = crash_dat_tidy %>%
  filter_for("bike") %>% 
  mutate(borough = str_to_title(borough)) %>%
  filter(year %in% c(2019, 2020)) %>%
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

fit_injuries_bike %>% 
  filter(term != "(Intercept)" & month != "November" & month != "December" & p.value < .05)
```

    ## # A tibble: 1 x 5
    ##   month term                            estimate std.error p.value
    ##   <ord> <chr>                              <dbl>     <dbl>   <dbl>
    ## 1 March 2020 v. 2019, Borough: Brooklyn    0.275     0.129  0.0330

**Below, I plot the estimates for the bike data (as in the microvehicle
data).**

``` r
fit_injuries_bike %>% 
  filter(term != "(Intercept)" & month != "November" & month != "December") %>%
  mutate(term = str_replace(term, "2020 v. 2019, ", "")) %>%
  ggplot(aes(x = month, y = exp(estimate), color = term)) + 
  geom_point(show.legend = FALSE, aes(size = estimate, alpha = .7)) +
  geom_errorbar(aes(ymin = exp(estimate - (1.96*std.error)), 
                    ymax = exp(estimate + (1.96*std.error)))) +
  geom_hline(yintercept = 1, linetype="dashed", 
                color = "darkred", size = 1, alpha = .7) +
  labs(
    title = "Difference in Rate of Injuries Per Crash Involving a Bike in 2020 v. 2019",
    x = "Month",
    y = "2020 v. 2019 Difference"
  ) +
  ylim(0, 5) +
  theme(legend.position="right", legend.title = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + 
  facet_grid(. ~ term)
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](20201125_InjuryPlots_files/figure-gfm/plot%20bike%20injuries%20by%20month-1.png)<!-- -->

**There is no discernable pattern in the figure, suggesting that rates
of injury per bike crash did not differ between 2020 and 2019.**
