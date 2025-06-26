library(insee)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(readxl)


liste <- get_dataset_list()


#Données trimestrielles
conj_ind <- get_insee_idbank("001585942", "010758406", "010758350", "001586762","001586739")
conj_con <- get_insee_idbank("001586807", "001586918")
act_emploi <- get_insee_idbank("010605834", "001688527")



pivot_trim <- function(conj_ind, freq = c("q", "y")) {
  freq <- match.arg(freq)  # Ensure it's either "q" or "y"
  
  # Pivot the dataframe
  conj_pivot <- conj_ind %>%
    select(DATE, TITLE_FR, OBS_VALUE) %>%
    pivot_wider(
      names_from = TITLE_FR,
      values_from = OBS_VALUE
    ) %>%
    arrange(DATE)
  
  conj_mutated <- conj_pivot %>%
    mutate(
      year = year(DATE),
      month = month(DATE),
      quarter_month = case_when(
        month %in% 1:3 ~ 1,
        month %in% 4:6 ~ 4,
        month %in% 7:9 ~ 7,
        month %in% 10:12 ~ 10
      ),
      quarter_date = as.Date(paste0(year, "-", sprintf("%02d", quarter_month), "-01")),
      year_date = as.Date(paste0(year))
    )
  
  # Aggregation
  conj_aggregated <- conj_mutated %>%
    group_by(period = if (freq == "q") quarter_date else year_date) %>%
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rename(DATE = period) %>%
    arrange(DATE)%>%
    select(-year, -month, -quarter_month)
  
  return(conj_aggregated)
}

data_ind <- pivot_trim(conj_ind, freq = "q")
data_con <- pivot_trim(conj_con, freq = "q")
data_bit <- pivot_trim(act_emploi,freq = "y")







act_emploi <- get_insee_idbank("010605834", "001688527")
data_bit <- pivot_trim(act_emploi)

comptes_emploi_durée <- read_excel("C:/Users/fauxa/OneDrive - Fédération Française du Bâtiment - On-premise/PIB potentiel/comptes_trim_recap_branches.xlsx",
                             sheet = "CpteEmploiDurée", col_names = FALSE)


emploi_durée <- comptes_emploi_durée %>%
  select(dates = 1, `emploi physique` = 3, `heures emploi` = 153) %>%   
  na.omit() %>%                                   
  mutate(
    year = substr(dates, 1, 4),
    `emploi physique` = as.numeric(`emploi physique`),
    `heures emploi` = as.numeric(`heures emploi`)
  ) %>%
  group_by(year) %>%
  summarise(
    emploi_physique = sum(`emploi physique`, na.rm = TRUE),
    heures_emploi = sum(`heures emploi`, na.rm = TRUE),
    .groups = "drop"
  )





















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
