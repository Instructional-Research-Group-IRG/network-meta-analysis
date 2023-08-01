# NMA and CNMA using netmeta package

# Install 'devel' version of metafor package
  #install.packages("remotes")
  #remotes::install_github("wviechtb/metafor")

# Load required packages
  #install.packages("pacman")
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr)

# Load (read in) data (i.e., copy data to 'dat')
  dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0")
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")

#Prepare data for NMA and CNMAs  
  
  ## Retain only studies meeting standards
  ratings_dnms <- dat %>% filter(wwc_rating == "dnm")
  ratings_dnms 
  dat_nodnm <- dat %>% filter(wwc_rating != "dnm") #Drop measure entries rated DNM
  dat_nodnm %>% group_by(wwc_rating) %>% count()
  
  ## Review number of outcome measures in each domain
  dat_nodnm %>% group_by(record_id, domain) %>% count() %>% print(n = Inf) #Many studies have multiple outcome measures within a domain.
  dat_nodnm %>% group_by(record_id, contrast, domain) %>% count() %>% print(n = Inf) #Many studies have multiple outcome measures within a contrast and domain.
  dat_nodnm %>% group_by(record_id, contrast, domain, measure_name) %>% count() %>% print(n = Inf) #Many studies have multiple outcome measures within a contrast and domain.
  
# Standard random effects NMA model (with placebo as reference treatment) using the netmeta package
  #NOTE: Neta-meta cannot handle multi-outcome dependencies: lines 19-24 using the full data will not run and thus have been blocked out.
  
  # trts1 <- c("V", "E", "X", "VN", "NE", "VNE", "BAU") #Define order of treatments in printouts and forest plots
  # net1 <- netmeta(effect_size, standard_error, intervention, comparison, record_id,
  #                 data = dat_nodnm, reference.group = "bau",
  #                 sm = "SMD", comb.fixed = FALSE, comb.random = TRUE, #sm= SMD- standardized mean difference (?)
  #                 seq = trts1, nchar.trts = 8, n1 = intervention_n, n2 = comparison_n)
  # net1
  
  #Subset data to a domain with only one outcome measure to see if netmeta will run the NMA with the removal of the multi-outcome dependencies.
  dat2 <- dat_nodnm %>% filter(domain == "Whole Number Knowledge")
  trts2 <- c("V", "NE", "BAU") #Define order of treatments in printouts and forest plots
  net2 <- netmeta(effect_size, standard_error, intervention, comparison, record_id,
                  data = dat2, reference.group = "bau",
                  sm = "SMD", comb.fixed = FALSE, comb.random = TRUE, #sm= SMD- standardized mean difference (?)
                  seq = trts2, nchar.trts = 8, n1 = intervention_n, n2 = comparison_n)
  net2

# Additive CNMA model with
# - BAU as inactive component and reference
# - automatically generated C matrix
#

  # nc1 <- netcomb(net1, inactive = "bau")
  # nc1$C.matrix
  # nc1
  
  nc2 <- netcomb(net2, inactive = "bau")
  nc2$C.matrix
  nc2

# Interaction CNMA model with
#
# C matrix with definition of interaction(s) has to be provided:
# - add column to C matrix with 1 in the row 'NE + V'
#   and zeros otherwise
#
  #NOTE: Cannot get the syntax to work for this yet but punt on this for now until we resolve our multi-outcome dependency issue in the netmeta NMA above.
  # C2.int <- cbind(nc2$C.matrix, NE.V = 0)
  # C2.int
  # C2.int["NE + V", "NE.V"] <- 1
  # C2.int
  # nc2.int <- netcomb(net2, C.matrix = C2.int, inactive = "bau")
  # nc2.int

