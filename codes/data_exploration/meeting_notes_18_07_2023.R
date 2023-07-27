# Species level approach


# ---- Species level legacy effects ----
# presence_absence_speciesA_2019 ~ speciesA_abundance_2001 + (1| ecoregion)
# RQ: Does the abundance in year1 influence the local extinction (presence/absence --> species not observed in 2017-2019)
#     of a species in a segment in the last year?
# Here we look at random extinctions, only based on the abundance.
# Investigate all the years leading up and try to find legacy effect for species:
# local extinctions per species calculated with moving window across all years --> always find endpoint for species

# Presence of legacy effects:
# abudance_year2001 explains local extinction of a species best, abundance_year2018 does not explain local extintion


# ----- Life history effects on the legacy effects ----
# presence_absence_speciesA_2019 ~ speciesA_abundance_2001 * generation_time_speciesA
# In the next part investigate what influences the legacy effect


# ---- Land use change effects on legacy effects ----
# presence_absence_speciesA_2019 ~ speciesA_abundance_2001 + land_use_change (classification 0/1 has the segment experienced land use change)
# presence_absence_speciesA_2019 ~ speciesA_abundance_2001 + land_use_change (percentage)


# ---- Combined effects of legacy and life history -----
# presence_absence_speciesA_2019 ~ speciesA_abundance_2001 + generation_time_speciesA + land_use_change


# ---- multi-species approach ----
# extinction_risk <- ifelse()

# presence_absence2019 ~ generation_time + abudance_beginning + (1|phylogeny) + (1|species) + (1|segment:cluster)
# RF: species, phylogeny
