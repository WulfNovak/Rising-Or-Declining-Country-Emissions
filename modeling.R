# ##############################################################################
# Author: Wulf Novak
#
# Project: Development Indicators of Rising 
#          and Declining Country Emissions
#
# Date: 2024-05-24
#
# R version 4.4.0 (2024-04-24 ucrt)
# ------------------------------------------------------------------------------
# Sections:
# - Model Preparation
# - Read in Data
# - Etc
# ##############################################################################

# Setup
source('./R Projects/Emissions-Data-Analysis/Setup.R')

# Ideas / Methodology

# 1. Country Level Models
#   Rising / Declining Emissions
# 2. Data from 2000 Onwards
#   limit to countries whose emissions have risen / declined since 2000
#   *Note countries that are excluded
#   Count of NAs per indicator per country (All NA # - 3 = Min Data Needed?) 
#   eliminate variables with too many NAs. Filter at the country level.
# 3. Variables correlated with emissions? (difficult with NAs)
# 4. AIC / AUC variable selection, at the country level 
#   *** Save Results / ranking for each country
#   For both rising and declining countries emissions
# 5. Show affect of variables across the models, calculate impact of variables
# on rising and declining emissions
# 6. Repeat for coal, oil, and gas

# Parameters to consider for function:
# 1. Year of data
# 2. Num NA allowed per indicator
# 3. y var: total, coal, oil, gas - need to then create data set for (rising/declining) (coal/oil/gas)
# 4. 

# Feature Reduction -------------------------------------------------------

# 

# Model Preparation -------------------------------------------------------

## Modeling Data

# Need to group by indicator and normalize variables



# CV ----------------------------------------------------------------------

# how to measure if there is a significant difference between the variables?

# The coefficients of those indicators will be helpful
  # accurate, fast, difficult to assess results

# Fixed effects -> Total? --- Need to think about this more
# ** Random effects model for each country / country as subgroup?

# Could average time values to 1 value (and take sd)
# Mixed effects models - subgroups = countries












