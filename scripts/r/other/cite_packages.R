# Generate a bibliography of all packages used in the analysis

library(grateful)

# File

cite_packages(output = "file",
              out.dir = "output",
              citation.style = "chicago-author-date",
              out.format = "md",
              out.file = "econ899_R_packages",
              passive.voice = FALSE,
              omit = NULL) 

# R Session Info

sink("output/econ899_R_session_info.txt")

sessionInfo()

sink()
