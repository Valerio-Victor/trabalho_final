



# Pacotes ------------------------------------------------------------------------------------------
`%>%` <- magrittr::`%>%`




# Importação ---------------------------------------------------------------------------------------
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv')




dplyr::glimpse(data)




# Arrumação ----------------------------------------------------------------------------------------




# Tratamento ---------------------------------------------------------------------------------------
pensions <- data %>%

  dplyr::mutate(productivity = round(pop_65_percent*(1/gov_spend_percent_gdp), 2),

         efficiency = round(productivity/max(productivity)*100, 2)) %>%

  dplyr::mutate(country = forcats::fct_reorder(country, -efficiency),

                group = dplyr::case_when(efficiency < 50 ~ 'Low',

                                        dplyr::between(efficiency, 51, 75) ~ 'Medium',

                                         efficiency > 75 ~ 'High'))




temp_sep <- data.frame(

  country = c(NA, NA, NA),

  pop_65_percent = c(NA, NA, NA),

  gov_spend_percent_gdp = c(NA, NA, NA),

  productivity = c(NA, NA, NA),

  efficiency = c(NA, NA, NA),

  group = c('Low', 'Medium', 'High'))




pensions <- dplyr::bind_rows(pensions, temp_sep)



pensions <- pensions %>%

  dplyr::group_by(group) %>%

  dplyr::arrange(group, efficiency) %>%

  dplyr::ungroup() %>%

  dplyr::mutate(id = seq(1, nrow(pensions), 1))




labels <- pensions

number_of_bar <- nrow(labels)

angle <- 90 - 360 * (labels$id - 0.5) / number_of_bar

labels$hjust <- ifelse(angle < -90, 1, 0)

labels$angle <- ifelse(angle < -90, (angle + 180), angle)




rm(temp_sep)




# Visualização -------------------------------------------------------------------------------------

pensions %>%

  ggplot2::ggplot(mapping = ggplot2::aes(x = as.factor(id), y = efficiency, fill = -efficiency)) +

  ggplot2::geom_bar(stat = 'identity') +

  ggplot2::ylim(-120,120) +

  ggplot2::coord_polar() +

  ggplot2::theme_minimal() +

  ggplot2::theme(

    axis.text = ggplot2::element_blank(),

    axis.title = ggplot2::element_blank(),

    panel.grid = ggplot2::element_blank(),

    plot.margin = ggplot2::unit(rep(-1,4), "cm")) +

  ggplot2::geom_text(data = labels,

            mapping = ggplot2::aes(x = id, y = efficiency + 3, label = country, hjust = hjust),

            color = 'black', fontface = 'bold', alpha = 1, size = 4,

            angle = labels$angle, inherit.aes = FALSE, na.rm = TRUE)





