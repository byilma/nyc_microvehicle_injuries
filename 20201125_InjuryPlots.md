Pull and Tidy
================
11/6/2020

``` r
#cleaning - transpose so vehicle types are listed in one column
crash_dat_tidy = 
  crash_dat %>% 
  mutate(
    date = lubridate::date(crash_date)
  ) %>% 
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
    values_to = "vehicle_options"
   ) %>%
  drop_na(vehicle_options) %>%
  select(date, everything())
```

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
    ## 10 October   2020 v. 2019  -0.0584     0.118   0.622

**Although there was no statistically significant evidence for a
different rate of injury in 2020 v. 21 in any month, I proceeded to
examine this stratified by borough.**

**Below, I extract the rate ratio estimates for the microvehicle data by
borough.**

``` r
fit_injuries_micro = crash_dat_tidy %>%
  filter_for("micro") %>% 
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
fit_injuries_micro_all = crash_dat_tidy %>%
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

fit_injuries_micro_all
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

**Below, I extract the rate ratio estimates for the bike data by borough
(as in the microvehicle data).**

``` r
fit_injuries_bike = crash_dat_tidy %>%
  filter_for("bike") %>% 
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
    ## 1 March 2020 v. 2019, Borough: BROOKLYN    0.275     0.129  0.0330

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
