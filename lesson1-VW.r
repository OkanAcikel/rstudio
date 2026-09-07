# ==============================================================================
# PORTFOLIO-AUFGABE 1
# Testung des Portrait Values Questionnaire (PVQ) nach Schwartz
# Konfirmatorische Faktorenanalyse (CFA) mit dem ESS-9-Datensatz
#
# Autor: Okan Acikel
#
# Ziel dieses Skripts:
# Dieses Skript prueft, ob die theoretisch angenommenen zehn Wertedimensionen
# nach Schwartz durch die 21 beobachtbaren PVQ-Items des ESS-9-Datensatzes
# empirisch angemessen erklaert werden koennen.
#
# Didaktischer Aufbau:
#  1. Pakete vorbereiten
#  2. Datensatz einlesen
#  3. Benoetigte Variablen pruefen
#  4. Daten kontrollieren und vorbereiten
#  5. CFA-Modell formulieren
#  6. Modell schaetzen
#  7. Fit-Indizes auswerten
#  8. Faktorladungen und Korrelationen untersuchen
#  9. Modell grafisch darstellen
# 10. Ergebnisse speichern
# ==============================================================================


# ==============================================================================
# 1. BENOETIGTE PAKETE INSTALLIEREN UND LADEN
# ==============================================================================

# lavaan  -> berechnet die konfirmatorische Faktorenanalyse
# semPlot -> stellt das CFA-Modell grafisch dar

pakete <- c("lavaan", "semPlot")

# Es werden nur Pakete installiert, die noch nicht vorhanden sind.
neue_pakete <- pakete[!(pakete %in% installed.packages()[, "Package"])]

if (length(neue_pakete) > 0) {
  install.packages(neue_pakete)
}

library(lavaan)
library(semPlot)

# MERKHILFE:
# Ein Paket muss normalerweise nur EINMAL installiert werden.
# Mit library(...) wird es dagegen in jeder neuen R-Sitzung erneut geladen.


# ==============================================================================
# 2. DATENSATZ EINLESEN
# ==============================================================================

# Voraussetzung:
# Die Datei "ess_pvq_numeric.csv" sollte im selben Ordner liegen wie dieses
# R-Skript.
#
# In RStudio kann der Skriptordner als Arbeitsordner gesetzt werden ueber:
# Session -> Set Working Directory -> To Source File Location

Dateiname <- "ess_pvq_numeric.csv"

# Zuerst wird geprueft, ob die Datei wirklich vorhanden ist.
if (!file.exists(Dateiname)) {
  stop(
    paste0(
      "Die Datei '", Dateiname, "' wurde im aktuellen Arbeitsordner nicht gefunden.\n",
      "Bitte lege die CSV-Datei in denselben Ordner wie dieses Skript oder ",
      "passe die Variable 'Dateiname' an."
    )
  )
}

# CSV-Datei einlesen.
ess <- read.csv(Dateiname, stringsAsFactors = FALSE)

cat("\nDatensatz erfolgreich eingelesen.\n")
cat("Anzahl der Faelle:", nrow(ess), "\n")
cat("Anzahl der Variablen:", ncol(ess), "\n")

# MERKHILFE:
# Zeilen = Untersuchungspersonen bzw. Faelle
# Spalten = Variablen bzw. Items


# ==============================================================================
# 3. DIE 21 PVQ-ITEMS FESTLEGEN
# ==============================================================================

# Der Portrait Values Questionnaire des ESS verwendet hier 21 Items.
# Diese Items werden spaeter den zehn Wertedimensionen nach Schwartz zugeordnet.

pvq_items <- c(
  "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe",
  "impdiff", "ipfrule", "ipudrst", "ipmodst", "ipgdtim",
  "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt",
  "ipbhprp", "iprspot", "iplylfr", "impenv", "imptrad", "impfun"
)

# Pruefen, ob wirklich alle benoetigten Variablen vorhanden sind.
fehlende_variablen <- setdiff(pvq_items, names(ess))

if (length(fehlende_variablen) > 0) {
  stop(
    paste(
      "Folgende benoetigte PVQ-Variablen fehlen im Datensatz:",
      paste(fehlende_variablen, collapse = ", ")
    )
  )
}

# Fuer die weitere Analyse werden nur die 21 relevanten PVQ-Items verwendet.
ess_pvq <- ess[, pvq_items]

# MERKHILFE:
# Eine CFA prueft eine VORHER theoretisch festgelegte Struktur.
# Deshalb legen wir bereits vor der Analyse fest, welche Items zum Modell gehoeren.


# ==============================================================================
# 4. DATENKONTROLLE
# ==============================================================================

cat("\n============================================================\n")
cat("DATENKONTROLLE\n")
cat("============================================================\n")

cat("Anzahl der Faelle:", nrow(ess_pvq), "\n")
cat("Anzahl der PVQ-Items:", ncol(ess_pvq), "\n\n")

# Fehlende Werte je Item zaehlen.
cat("Fehlende Werte pro Item:\n")
print(colSums(is.na(ess_pvq)))

# Wertebereich jedes Items kontrollieren.
cat("\nWertebereiche der Items:\n")
print(sapply(ess_pvq, function(x) range(x, na.rm = TRUE)))

# Die PVQ-Items sollen nur die Antwortkategorien 1 bis 6 enthalten.
ungueltige_werte <- sapply(
  ess_pvq,
  function(x) any(!is.na(x) & !(x %in% 1:6))
)

if (any(ungueltige_werte)) {
  warning(
    paste(
      "In folgenden Variablen gibt es Werte ausserhalb des Bereichs 1 bis 6:",
      paste(names(ungueltige_werte)[ungueltige_werte], collapse = ", ")
    )
  )
} else {
  cat("\nKontrolle erfolgreich: Alle beobachteten Werte liegen im Bereich 1 bis 6.\n")
}

# MERKHILFE:
# Bevor ein statistisches Modell berechnet wird, immer zuerst die Daten pruefen.
# Ein technisch korrekt ausgefuehrtes Modell kann trotzdem falsche Ergebnisse
# liefern, wenn die Ausgangsdaten falsch codiert sind.


# ==============================================================================
# 5. ITEMS ALS ORDINALE VARIABLEN BEHANDELN
# ==============================================================================

# Die Antwortmoeglichkeiten des PVQ besitzen eine natuerliche Reihenfolge.
# Es handelt sich deshalb um ordinale Variablen.
#
# Beispiel:
# 1 = sehr aehnlich
# ...
# 6 = ueberhaupt nicht aehnlich
#
# Die Abstaende zwischen den Kategorien muessen jedoch nicht exakt gleich gross
# sein. Deshalb behandeln wir die Items nicht einfach wie metrische Variablen.

# Umwandlung in geordnete Faktoren.
ess_pvq[pvq_items] <- lapply(
  ess_pvq[pvq_items],
  function(x) ordered(x, levels = 1:6)
)

# MERKHILFE:
# ORDINAL bedeutet:
# Die Kategorien besitzen eine Reihenfolge, aber die Abstaende zwischen ihnen
# sind nicht zwingend gleich gross.
# Genau deshalb wird spaeter WLSMV als Schaetzverfahren verwendet.


# ==============================================================================
# 6. SCHWARTZ-MODELL ALS CFA FORMULIEREN
# ==============================================================================

# Das theoretische Modell besteht aus zehn LATENTEN Faktoren.
# Latent bedeutet: Die Werteorientierung kann nicht direkt beobachtet werden.
# Sie wird indirekt ueber mehrere beobachtbare Items gemessen.
#
# Schreibweise in lavaan:
# Faktor =~ Item1 + Item2
#
# Das Zeichen "=~" kann man lesen als:
# "Der latente Faktor wird durch diese Items gemessen."

schwartz_modell <- '
  # 1. Selbstbestimmung / Self-Direction
  SelfDirection =~ ipcrtiv + impfree

  # 2. Stimulation
  Stimulation   =~ impdiff + ipadvnt

  # 3. Hedonismus
  Hedonism      =~ ipgdtim + impfun

  # 4. Leistung / Achievement
  Achievement   =~ ipshabt + ipsuces

  # 5. Macht / Power
  Power         =~ imprich + iprspot

  # 6. Sicherheit / Security
  Security      =~ impsafe + ipstrgv

  # 7. Konformitaet / Conformity
  Conformity    =~ ipfrule + ipbhprp

  # 8. Tradition
  Tradition     =~ ipmodst + imptrad

  # 9. Wohlwollen / Benevolence
  Benevolence   =~ iphlppl + iplylfr

  # 10. Universalismus / Universalism
  Universalism  =~ ipeqopt + ipudrst + impenv
'

# MERKHILFE:
# Beobachtbare Variable = ein konkret beantwortetes Item.
# Latente Variable      = theoretisches Merkmal hinter mehreren Items.
# CFA                   = prueft, ob diese theoretisch festgelegte Zuordnung
#                         zu den beobachteten Daten passt.


# ==============================================================================
# 7. KONFIRMATORISCHE FAKTORENANALYSE BERECHNEN
# ==============================================================================

# WLSMV eignet sich gut fuer ordinale Antwortkategorien.
#
# std.lv = TRUE:
# Die Varianz jeder latenten Variable wird zur Identifikation auf 1 gesetzt.
# Dies ist insbesondere bei Faktoren mit nur zwei Indikatoren hilfreich.

fit <- cfa(
  model = schwartz_modell,
  data = ess_pvq,
  ordered = pvq_items,
  estimator = "WLSMV",
  std.lv = TRUE
)

# MERKHILFE:
# "Fit" bedeutet Modellpassung.
# Die zentrale Frage lautet nicht:
# "Ist das Modell perfekt?"
# sondern:
# "Wie gut kann das theoretische Modell die empirischen Daten erklaeren?"


# ==============================================================================
# 8. VOLLSTAENDIGE MODELLZUSAMMENFASSUNG AUSGEBEN
# ==============================================================================

cat("\n============================================================\n")
cat("CFA-ERGEBNIS\n")
cat("============================================================\n")

print(
  summary(
    fit,
    fit.measures = TRUE,    # Fit-Indizes anzeigen
    standardized = TRUE,    # standardisierte Schaetzungen anzeigen
    rsquare = TRUE          # erklaerte Varianz der Items anzeigen
  )
)


# ==============================================================================
# 9. ZWEI FIT-INDIZES FUER DIE AUFGABENSTELLUNG AUSWAEHLEN
# ==============================================================================

# Die Aufgabenstellung verlangt die Beurteilung anhand von zwei selbst
# gewaehlten Fit-Indizes.
#
# Hier verwenden wir:
#   1. CFI   = Comparative Fit Index
#   2. RMSEA = Root Mean Square Error of Approximation
#
# Orientierende Faustregeln:
# CFI   >= .95 -> gute Passung
# CFI   >= .90 -> haeufig noch akzeptable Passung
#
# RMSEA <= .06 -> gute Passung
# RMSEA <= .08 -> haeufig noch akzeptable Passung
#
# WICHTIG:
# Diese Grenzwerte sind Orientierungshilfen und keine unveraenderlichen Gesetze.
# Fit-Indizes sollten immer gemeinsam und im inhaltlichen Kontext interpretiert
# werden.

fit_indizes <- fitMeasures(fit, c("cfi", "rmsea"))

cat("\n============================================================\n")
cat("AUSGEWAEHLTE FIT-INDIZES\n")
cat("============================================================\n")
print(round(fit_indizes, 3))

# Zur zusaetzlichen Kontrolle werden weitere uebliche Kennwerte ausgegeben.
weitere_fitwerte <- fitMeasures(
  fit,
  c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi", "tli", "rmsea", "srmr")
)

cat("\nWeitere Fit-Kennwerte zur Einordnung:\n")
print(round(weitere_fitwerte, 3))

# MERKHILFE:
# CFI:   Je naeher an 1, desto besser.
# RMSEA: Je naeher an 0, desto besser.
#
# Kurzform:
# CFI   -> HOCH ist gut.
# RMSEA -> NIEDRIG ist gut.


# ==============================================================================
# 10. CFI UND RMSEA AUTOMATISCH INTERPRETIEREN
# ==============================================================================

cfi_wert <- unname(fit_indizes["cfi"])
rmsea_wert <- unname(fit_indizes["rmsea"])

# CFI interpretieren.
interpretation_cfi <- if (cfi_wert >= .95) {
  "Der CFI spricht fuer eine gute Modellpassung."
} else if (cfi_wert >= .90) {
  "Der CFI spricht fuer eine noch akzeptable, aber nicht sehr gute Modellpassung."
} else {
  "Der CFI liegt unter .90 und weist auf eine eher unzureichende Modellpassung hin."
}

# RMSEA interpretieren.
interpretation_rmsea <- if (rmsea_wert <= .06) {
  "Der RMSEA spricht fuer eine gute Modellpassung."
} else if (rmsea_wert <= .08) {
  "Der RMSEA spricht fuer eine akzeptable Modellpassung."
} else {
  "Der RMSEA weist auf eine eher unzureichende Modellpassung hin."
}

cat("\n============================================================\n")
cat("INTERPRETATION DER BEIDEN FIT-INDIZES\n")
cat("============================================================\n")

cat(sprintf("CFI = %.3f: %s\n", cfi_wert, interpretation_cfi))
cat(sprintf("RMSEA = %.3f: %s\n", rmsea_wert, interpretation_rmsea))

# Gemeinsame Interpretation beider Kennwerte.
cat("\nGesamtbeurteilung:\n")

if (cfi_wert >= .90 && rmsea_wert <= .08) {
  cat(
    "Beide ausgewaehlten Fit-Indizes sprechen insgesamt fuer eine mindestens\n",
    "akzeptable Passung des postulierten Zehn-Faktoren-Modells an die ESS-9-Daten.\n"
  )
} else if (cfi_wert < .90 && rmsea_wert <= .08) {
  cat(
    "Die Fit-Indizes ergeben ein gemischtes Bild: Der RMSEA spricht fuer eine\n",
    "akzeptable Passung, waehrend der CFI auf Modellabweichungen hinweist.\n",
    "Das theoretische Schwartz-Modell wird damit nur teilweise durch die Daten\n",
    "gestuetzt.\n"
  )
} else {
  cat(
    "Die ausgewaehlten Fit-Indizes sprechen insgesamt nicht fuer eine\n",
    "zufriedenstellende Passung des postulierten Modells.\n"
  )
}

# MERKHILFE:
# Ein einzelner Fit-Index entscheidet nicht allein ueber die Qualitaet eines
# Modells. Entscheidend ist das Gesamtbild aus mehreren Kennwerten und der
# theoretischen Plausibilitaet des Modells.


# ==============================================================================
# 11. STANDARDISIERTE FAKTORLADUNGEN UNTERSUCHEN
# ==============================================================================

# Die Faktorladung beschreibt, wie stark ein Item mit seinem zugeordneten
# latenten Faktor zusammenhaengt.

standardisierte_loesung <- standardizedSolution(fit)

faktorladungen <- subset(
  standardisierte_loesung,
  op == "=~"
)

cat("\n============================================================\n")
cat("STANDARDISIERTE FAKTORLADUNGEN\n")
cat("============================================================\n")

print(
  faktorladungen[, c("lhs", "rhs", "est.std", "pvalue")]
)

# MERKHILFE:
# Je hoeher der Betrag einer standardisierten Faktorladung, desto staerker
# repraesentiert das jeweilige Item seinen latenten Faktor.
# Die Ladungen sollten jedoch immer zusammen mit Theorie und Gesamtmodell
# betrachtet werden.


# ==============================================================================
# 12. KORRELATIONEN ZWISCHEN DEN LATENTEN FAKTOREN
# ==============================================================================

# Schwartz geht nicht davon aus, dass alle Wertedimensionen vollkommen
# unabhaengig voneinander sind. Deshalb sind die Korrelationen zwischen den
# latenten Faktoren inhaltlich interessant.

latente_faktoren <- c(
  "SelfDirection", "Stimulation", "Hedonism", "Achievement", "Power",
  "Security", "Conformity", "Tradition", "Benevolence", "Universalism"
)

latente_korrelationen <- subset(
  standardisierte_loesung,
  op == "~~" &
    lhs != rhs &
    lhs %in% latente_faktoren &
    rhs %in% latente_faktoren
)

cat("\n============================================================\n")
cat("KORRELATIONEN ZWISCHEN DEN LATENTEN FAKTOREN\n")
cat("============================================================\n")

print(
  latente_korrelationen[, c("lhs", "rhs", "est.std", "pvalue")]
)

# MERKHILFE:
# Eine Korrelation nahe +1 bedeutet einen starken positiven Zusammenhang.
# Eine Korrelation nahe  0 bedeutet kaum einen linearen Zusammenhang.
# Eine Korrelation nahe -1 bedeutet einen starken negativen Zusammenhang.
# Sehr hohe Korrelationen zwischen zwei Faktoren koennen darauf hinweisen,
# dass sich diese empirisch nur schwer voneinander unterscheiden lassen.


# ==============================================================================
# 13. CFA-MODELL GRAFISCH DARSTELLEN
# ==============================================================================

# Die folgende Darstellung erscheint im Plot-Fenster von RStudio.
# Kreise bzw. Ellipsen repraesentieren latente Faktoren.
# Rechtecke repraesentieren beobachtbare Items.

semPaths(
  fit,
  what = "std",
  whatLabels = "std",
  layout = "tree2",
  style = "lisrel",
  residuals = FALSE,
  intercepts = FALSE,
  thresholds = FALSE,
  edge.label.cex = 0.65,
  sizeLat = 8,
  sizeMan = 5,
  nCharNodes = 0
)

# MERKHILFE:
# Im Pfaddiagramm gilt:
#   Kreis/Ellipse = latente Variable
#   Rechteck       = beobachtbares Item
#   Pfeil          = modellierter Zusammenhang


# ==============================================================================
# 14. ERGEBNISSE ALS CSV-DATEIEN SPEICHERN
# ==============================================================================

# Dadurch koennen die wichtigsten Ergebnisse spaeter leicht in Excel,
# Word oder einer Tabellenkalkulation weiterverwendet werden.

write.csv(
  data.frame(
    Fit_Index = names(fit_indizes),
    Wert = as.numeric(fit_indizes)
  ),
  "CFA_Fit_Indizes.csv",
  row.names = FALSE
)

write.csv(
  faktorladungen[, c("lhs", "rhs", "est.std", "se", "z", "pvalue")],
  "CFA_Faktorladungen.csv",
  row.names = FALSE
)


# ==============================================================================
# 15. ABSCHLUSSMELDUNG
# ==============================================================================

cat("\n============================================================\n")
cat("ANALYSE ABGESCHLOSSEN\n")
cat("============================================================\n")
cat("Die konfirmatorische Faktorenanalyse wurde berechnet.\n")
cat("Die wichtigsten Ergebnisse wurden in der Konsole ausgegeben.\n")
cat("\nZusaetzlich wurden folgende Dateien gespeichert:\n")
cat("  - CFA_Fit_Indizes.csv\n")
cat("  - CFA_Faktorladungen.csv\n")
cat("\nBitte interpretiere insbesondere CFI und RMSEA fuer die Portfolio-Aufgabe.\n")

# ==============================================================================
# ENDE DES SKRIPTS
# ==============================================================================
