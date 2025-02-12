data <- read.csv("../Werkgelegenheid_Twente_Nieuw.csv", sep = ";")

data$Peildatum <- as.Date(data$Peildatum, format = "%d-%m-%Y")
data$Jaar <- as.numeric(format(data$Peildatum, "%Y"))
data$Gebied <- iconv(data$Gebied, from = "latin1", to = "UTF-8")
data$Type <- iconv(data$Type, from = "latin1", to = "UTF-8")

# Alleen de relevante kolommen behouden
data <- data %>%
  select(
    Jaar,
    Gebied,
    Type,
    Werkzame_personen_totaal,
    Werkzame_personen_fulltime,
    Werkzame_personen_parttime
  )

saveRDS(data, "data.RDS")