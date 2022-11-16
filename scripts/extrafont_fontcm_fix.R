# ============================================================================ #
# ---- extrafont("fontcm") issue ----
# ============================================================================ #  
# extrafont::font_install("fontcm")
# extrafont:::type1_import()
# pfbdata <- data.frame(fontfile = pfbfile, base = sub("\\.pf?$", # <- issue 
#                                                      "", basename(pfbfile)))
# 
# pfbdata <- data.frame(fontfile = pfbfile, base = sub("\\.pf[a-z]?$", # <- fix
#                                                      "", basename(pfbfile)))