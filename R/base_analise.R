



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




# legend_plot1 <- data.frame(
#
#   type = c('Less Efficient', 'Target Efficiency'),
#
#   value = c('1', '1'))




rm(data, temp_sep, number_of_bar, angle)




# Visualização -------------------------------------------------------------------------------------
plot1 <- pensions %>%

  ggplot2::ggplot(mapping = ggplot2::aes(x = as.factor(id), y = efficiency, fill = efficiency)) +

  ggplot2::geom_bar(stat = 'identity') +

  ggplot2::geom_text(data = labels,

                     mapping = ggplot2::aes(x = id, y = efficiency + 3, label = country, hjust = hjust),

                     color = 'white', alpha = 1, size = 4, family = 'oswald', fontface = 'bold',

                     angle = labels$angle, inherit.aes = FALSE, na.rm = TRUE) +

  ggplot2::coord_cartesian(clip = 'on') +

  ggplot2::ylim(-120,120) +

  ggplot2::coord_polar() +

  ggplot2::scale_fill_gradient(low = '#F25D27', high = '#03A696') +

  ggplot2::theme_minimal() +

  ggplot2::labs(title = 'Efficiency Scores',

                fill = 'Target \nEfficiency') +

  ggplot2::theme(

    title = ggplot2::element_text(color = 'white', size = 30,

                                  face = 'bold', family = 'oswald'),

    legend.position = 'left',

    legend.title = ggplot2::element_text(color = 'white', size = 15,

                                         face = 'bold', family = 'oswald'),

    legend.text = ggplot2::element_text(color = 'grey20'),

    axis.text = ggplot2::element_blank(),

    axis.title = ggplot2::element_blank(),

    panel.grid = ggplot2::element_blank(),

    panel.background = ggplot2::element_rect(fill = 'grey20', color = NA),

    plot.margin = ggplot2::unit(rep(-1,4), 'cm'))






plot2 <- pensions %>%

  ggplot2::ggplot() +

  ggplot2::geom_point(mapping = ggplot2::aes(x = pop_65_percent, y = gov_spend_percent_gdp,

                                             color = efficiency), size = 3) +

  ggplot2::scale_color_gradient(low = '#F25D27', high = '#03A696') +

  ggplot2::labs(title = 'Base Data',

                x = 'Population aged 65 years and over, % of total',

                y = 'Government speding on pension benefits % of GDP') +

  ggplot2::xlim(0,30) +

  ggplot2::ylim(0,20) +

  ggplot2::theme_minimal() +

  ggplot2::theme(

    title = ggplot2::element_text(color = 'white', size = 30,

                                  face = 'bold', family = 'oswald'),

    legend.position = 'none',

    axis.text = ggplot2::element_text(color = 'white', size = 15, face = 'bold', family = 'oswald'),

    axis.title = ggplot2::element_text(color = 'white', size = 20, face = 'bold', family = 'oswald'),

    panel.background = ggplot2::element_rect(fill = 'grey20', color = NA))

plot2


final <- plot2 / plot1 +

  patchwork::plot_layout(ncol = 2) +

  patchwork::plot_annotation(

    title = "Brazil's Golden Oldie Blowout",

    subtitle = 'Latest Avaiable',

    caption =

      'Author: Victor Valerio (@victor_dmv) | Data: The Economist',

    theme = ggplot2::theme(

      plot.background = ggplot2::element_rect(fill = 'grey20', color = NA),

      plot.title = ggplot2::element_text(family = 'oswald', size = 30, color = 'white',

                                         hjust = 0.5, face = 'bold'),

      plot.subtitle = ggplot2::element_text(family = 'oswald', color = 'white', size = 30,

                                            hjust = 0.5, margin = ggplot2::margin(10,0,15,0)),

      plot.caption = ggplot2::element_text(family = 'techmono', color = 'grey80', size = 15,

                                           hjust = 0.98)))


final
























