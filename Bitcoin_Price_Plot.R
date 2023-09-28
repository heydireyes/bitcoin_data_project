library(ggplot2)

average_close_price_by_year <- data.frame(
    year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
    Average_Close_Price = c(1000, 2000, 3000, 4000, 3000, 60000, 40000, 50000)
)

trading_volume_data <- data.frame(
    year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
    Volume = c(100000, 200000, 300000, 400000, 350000, 800000, 600000, 700000)
)

smoothed_data <- predict(loess(Average_Close_Price ~ year, data = average_close_price_by_year))

smoothed_df <- data.frame(year = average_close_price_by_year$year, Average_Close_Price = smoothed_data)

confidence_interval_data <- data.frame(
    year = average_close_price_by_year$year,
    ci_lower = smoothed_data - runif(length(smoothed_data), 500, 2000),
    ci_upper = smoothed_data + runif(length(smoothed_data), 500, 2000)
)

plot <- ggplot() +
    geom_line(data = average_close_price_by_year, aes(x = year, y = Average_Close_Price), color = "dodgerblue3", size = 1.5) +
    geom_point(data = average_close_price_by_year, aes(x = year, y = Average_Close_Price), color = "dodgerblue3", size = 3) +
    geom_line(data = smoothed_df, aes(x = year, y = Average_Close_Price), color = "red", size = 0.5) +
    geom_vline(xintercept = 2017, linetype = "dashed", color = "gray", size = 1) +
    annotate("text", x = 2017, y = max(average_close_price_by_year$Average_Close_Price) + 3000,
             label = "Significant Event", color = "gray") +
    geom_line(data = trading_volume_data, aes(x = year, y = Volume), color = "orange", size = 1.5) +
    scale_y_continuous(name = "Average Close Price (USD)", sec.axis = sec_axis(~., name = "Trading Volume")) +
    annotate("text", x = 2021, y = 55000, label = "All-time High", size = 4, color = "red") +
    annotate("text", x = 2017, y = 18000, label = "Previous High", size = 4, color = "darkgreen") +
    labs(x = "Year", y = "Average Close Price (USD)", title = "Average Bitcoin Closing Price Over the Years") +
    scale_color_manual(values = c("dodgerblue3")) +
    scale_fill_manual(values = c("dodgerblue3")) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")
    )

price_low <- average_close_price_by_year[which.min(average_close_price_by_year$Average_Close_Price), ]
price_high <- average_close_price_by_year[which.max(average_close_price_by_year$Average_Close_Price), ]

plot +
    geom_text(data = price_low, aes(label = paste("Lowest Price: $", format(price_low$Average_Close_Price, big.mark = ",", scientific = FALSE), " USD"), x = price_low$year, y = price_low$Average_Close_Price + 5000), vjust = -0.5, hjust = -0.5, color = "darkred") +
    geom_text(data = price_high, aes(label = paste("Highest Price: $", format(price_high$Average_Close_Price, big.mark = ",", scientific = FALSE), " USD"), x = price_high$year - 4, y = price_high$Average_Close_Price), vjust = -0.5, hjust = -1, color = "darkgreen") +
    ylim(min(average_close_price_by_year$Average_Close_Price) - 2000, max(average_close_price_by_year$Average_Close_Price) + 2000)
