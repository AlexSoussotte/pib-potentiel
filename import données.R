library(insee)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(FactoMineR)
library(factoextra)

#Données trimestrielles
conj_ind <- get_insee_idbank("001585942", "010758406", "010758350", "001586762","001586739")
conj_con <- get_insee_idbank("001586807", "001586918")

pivot_trim <- function(conj_ind) {
  
  # Pivote le df
  conj_pivot <- conj_ind %>%
    select(DATE, TITLE_FR, OBS_VALUE) %>%
    pivot_wider(
      names_from = TITLE_FR,
      values_from = OBS_VALUE
    ) %>%
    arrange(DATE)
  
  # Colonnes temp pour aggrégation
  conj_quarterly <- conj_pivot %>%
    mutate(
      year = year(DATE),
      month = month(DATE),
      quarter_month = case_when(
        month %in% 1:3 ~ 1,
        month %in% 4:6 ~ 4,
        month %in% 7:9 ~ 7,
        month %in% 10:12 ~ 10
      ),
      quarter_date = as.Date(paste0(year, "-", sprintf("%02d", quarter_month), "-01"))
    )
  
  # Aggrégation
  conj_quarterly_avg <- conj_quarterly %>%
    group_by(quarter_date) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    rename(DATE = quarter_date) %>%
    arrange(DATE)%>%
    select(-year, -month, -quarter_month)
  
  return(conj_quarterly_avg)
}


data_ind <- pivot_trim(conj_ind)
data_con <- pivot_trim(conj_con)



data_all <- full_join(data_ind, data_con, by = "DATE")

data_complete <- data_all %>%
  filter(if_all(-DATE, ~ !is.na(.)))


data_scaled <- data_complete %>%
  select(-DATE) %>%
  scale(center = TRUE, scale = TRUE)

pca_result <- prcomp(data_complete, center = FALSE, scale. = FALSE)
first_component <- pca_result$x[, 1]

df_plot <- data.frame(
  DATE = data_complete$DATE,
  PC1 = first_component
)

ggplot(df_plot, aes(x = DATE, y = PC1)) +
  geom_line() +
  labs(title = "Premier facteur (ACP)", x = "Trimestre", y = "Valeur normalisée") +
  theme_minimal()
