source("get_google_sheet.R")

station_based_plans = list(
  open = list(
    name = "Open",
    hourly_rate = 14.25,
    first_daily_max = 55,
    additional_day = 50,
    km_interval_1 = 75,
    km_interval_rate_1 = 0,
    km_interval_rate_2 = 0.31,
    monthly_fee = 0,
    weekend_hourly_surcharge = 0.35,
    weekend_daily_surcharge = 3.50
  ),
  open_plus = list(
    name = "Open Plus",
    hourly_rate = 7.65,
    first_daily_max = 50,
    additional_day = 36.50,
    km_interval_1 = 50,
    km_interval_rate_1 = 0.30,
    km_interval_rate_2 = 0.29,
    monthly_fee = 5,
    weekend_hourly_surcharge = 0.35,
    weekend_daily_surcharge = 3.50
  ),
  value = list(
    name = "Value",
    hourly_rate = 4.65,
    first_daily_max = 36.50,
    additional_day = 36.50,
    km_interval_1 = 50,
    km_interval_rate_1 = 0.51,
    km_interval_rate_2 = 0.38,
    monthly_fee = 5,
    weekend_hourly_surcharge = 0.35,
    weekend_daily_surcharge = 3.50
  ),
  value_plus = list(
    name = "Value Plus",
    hourly_rate = 4.05,
    first_daily_max = 30.50,
    additional_day = 30.50,
    km_interval_1 = 50,
    km_interval_rate_1 = 0.43,
    km_interval_rate_2 = 0.34,
    monthly_fee = 12.50,
    weekend_hourly_surcharge = 0.35,
    weekend_daily_surcharge = 3.50
  ),
  value_extra = list(
    name = "Value Extra",
    hourly_rate = 3.75,
    first_daily_max = 26.50,
    additional_day = 26.50,
    km_interval_1 = 0,
    km_interval_rate_1 = 0.34,
    km_interval_rate_2 = 0.34,
    monthly_fee = 30,
    weekend_hourly_surcharge = 0.35,
    weekend_daily_surcharge = 3.50
  ),
  value_extra_workday = list(
    name = "Value Extra (Workday)",
    hour_limit = 10,
    daily_rate = 25,
    km_interval_1 = 40,
    km_interval_rate_1 = 0,
    km_interval_rate_2 = 0.39,
    monthly_fee = 30
  ),
  flex = list(
    name = "Flex",
    minutely_rate = 0.43,
    hourly_max_ = 14.25,
    first_daily_max = 50,
    additional_day = 50,
    km_interval_1 = 75,
    km_interval_rate_1 = 0,
    km_interval_rate_2 = 0.31,
    weekend_hourly_surcharge = 0,
    weekend_daily_surcharge = 0
  )
)

long_distance_rates = list(
  low_season = list(
    first_day = 46,
    additional_days = 37,
    hourly_rate = 15,
    weekly_rate = 220,
    km_interval_1 = 300,
    km_interval_rate_1 = 0.29,
    km_interval_rate_2 = 0.19
  ),
  high_season = list(
    first_day = 61,
    additional_days = 52,
    hourly_rate = 15,
    weekly_rate = 290,
    km_interval_1 = 300,
    km_interval_rate_1 = 0.29,
    km_interval_rate_2 = 0.19
  )
)

time_cost_fn <- function(
  hours,
  plan
) {
  time_cost <- 0

  for (
    day in seq(
      0,
      hours %/% 24 + 0
    )
  ) {
    if (
      day == 0
    ) {
      time_cost <- time_cost + min(
        plan$hourly_rate*(
          hours - 24*day
        ),
        plan$first_daily_max
      )
    } else {
      time_cost <- time_cost + min(
        plan$hourly_rate*(
          hours - 24*day
        ),
        plan$additional_day
      )
    }
  }
  
  return(
    time_cost
  )
}

weekend_time_cost_fn <- function(
  hours,
  plan
) {
  weekend_time_cost <- 0

  for (
    day in seq(
      0,
      hours %/% 24 + 0
    )
  ) {
    if (
      day == 0
    ) {
      weekend_time_cost <- weekend_time_cost + min(
        (
          plan$hourly_rate + plan$weekend_hourly_surcharge
        )*(
          hours - 24*day
        ),
        plan$first_daily_max + plan$weekend_daily_surcharge
      )
    } else {
      weekend_time_cost <- weekend_time_cost + min(
        (
          plan$hourly_rate + plan$weekend_hourly_surcharge
        )*(
          hours - 24*day
        ),
        plan$additional_day + plan$weekend_daily_surcharge
      )
    }
  }
  
  return(
    weekend_time_cost
  )
}

distance_cost_fn <- function(
  distance,
  plan
) {
  distance_cost <- 0

  distance_cost <- plan$km_interval_rate_1*min(
    distance,
    plan$km_interval_1
  )

  if (
    distance > plan$km_interval_1
  ) {
    distance_cost <- distance_cost + plan$km_interval_rate_2*(
      distance - plan$km_interval_1
    )
  }

  return(
    distance_cost
  )
}

get_costs <- function(
  plan,
  hours,
  distance
) {
  data <- expand.grid(
    hours=hours,
    distance=distance
  )

  data <- data %>%
    mutate(
      time_cost = lapply(
        .$hours,
        time_cost_fn,
        plan = plan
      ) |> as.numeric(),
      weekend_time_cost = lapply(
        .$hours,
        weekend_time_cost_fn,
        plan = plan
      ) |> as.numeric(),
      distance_cost = lapply(
        .$distance,
        distance_cost_fn,
        plan = plan
      ) |> as.numeric(),
      total_cost = time_cost + distance_cost,
      weekend_total_cost= weekend_time_cost + distance_cost,
      plan_name = plan$name,
      tooltip = paste0(
        "$",
        total_cost,
        "\nTime (",
        hours,
        "h): $",
        time_cost,
        "\nDistance (",
        distance,
        "km): $",
        distance_cost
      )
    )

    return(data)
}

make_best_plan_heatmap <- function(
  best_cost_df,
  plan_colours
) {

  x_diff <- diff(
    sort(
      unique(
        best_cost_df$hours
      )
    )
  )[1]

  y_diff <- diff(
    sort(
      unique(
        best_cost_df$distance
      )
    )
  )[1]

  plot_data <- best_cost_df |>
    mutate(
      x = as.numeric(hours),
      y = as.numeric(distance),
      ymin = y - .5*y_diff,
      ymax = y + .5*y_diff
    ) |>
    mutate(
      n = n(),
      xmin = x + scales::rescale(
        row_number(),
        from = c(1, unique(n) + 1),
        to = .5 *x_diff*c(-1, 1)
      ),
      xmax = x + scales::rescale(
        row_number() + 1,
        from = c(1, unique(n) + 1),
        to = .5 *x_diff* c(-1, 1)
      ),
      .by = c(
        hours,
        distance
      )
    )

  ggplot(
    plot_data,
    aes(
      x = hours,
      y = distance,
      fill = plan_name
    )
  ) +
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      color = "black", 
      linewidth = 0.5
    ) +
    labs(
      fill = "Plan:",
      x = "Hours",
      y = "Distance (km)"
    ) +
    scale_fill_manual(
      values=plan_colours
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )

  # ggplotly(
  #   p,
  #   tooltip="tooltip"
  # )
}

make_cost_per_plan_line_chart <- function(
  hours,
  cost_dfs,
  average_driving_speed,
  plan_colours
) {

  distance <- average_driving_speed*hours

  average_cost_per_duration <- lapply(
    names(costs_dfs),
    \(x) costs_dfs[[x]] |>
      semi_join(
        tibble(
          hours = hours,
          distance = distance
        ),
        by = c(
          "hours",
          "distance"
        )
      ) |>
      select(
        hours,
        total_cost,
        Colour
      ) |>
      mutate(
        plan = x
      )
  ) |>
  bind_rows()

  ggplot(
    average_cost_per_duration,
    aes(
      x = hours,
      y = total_cost,
      group = plan
    )
  ) +
    geom_line(
      aes(
        colour = plan
      ),
      linewidth = 1
    ) +
    labs(
      x = "Hours",
      y = "Total Cost ($)"
    ) +
    scale_colour_manual(
      values = plan_colours
    ) + 
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
}
