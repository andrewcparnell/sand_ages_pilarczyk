# Model for Jessica Pilarczyk with separate ages for sand A and sand B
# This implements a shortcut method that calibrates all dates and then calculates the likely ages of the sand layers between them

# Clear the workspace
rm(list = ls())

# Load packages
library(auf)
packages('tidyverse', 'reshape2', 'Bchron', 'hdrcde')

# Load in data
site = 'Hasunuma'#'Ichinomiya'# ###'Both'#
pil_data_raw = read_csv('pilarczyk_data/pilarczyk_data.csv')
pil_data = pil_data_raw %>% 
  filter(`Used` == 'Yes')
if(site!='Both') pil_data = pil_data %>% filter(`Site` == site)

# A quick plot of just the 14C ages
ggplot(pil_data, aes(x = C14_age, y = Comment)) +
  geom_point() #+ 
  #facet_grid(Site ~ .)

# Calibrate all the dates
n_dates = nrow(pil_data)
pil_cal_store = vector('list', length = n_dates)
num_sample_ages = 50000

for (i in 1:n_dates) {
  pil_cal_store[[i]] = with(pil_data[i,], 
                            BchronCalibrate(ages = C14_age,
                                            ageSds = C14_sigma,
                                            ids = `Code`,
                                            calCurves = 'intcal13'))
  # Correct the first density measurement for plotting purposes
  pil_cal_store[[i]][[1]]$densities[1] = 0
  n_dens = length(pil_cal_store[[i]][[1]]$densities)
  pil_cal_store[[i]][[1]]$densities[n_dens] = 0
  pil_cal_store[[i]][[1]]$type = pil_data[i,'Comment']
  pil_cal_store[[i]][[1]]$layer = substr(pil_data[i,'Comment'],1,1)
  
  # Sample some ages
  pil_cal_store[[i]][[1]]$samples = sampleAges(pil_cal_store[[i]],
                                               n_samp = num_sample_ages)
}

# Collect all the ages_younger_A
A_younger_cols = which(pil_data$Comment=='A_younger')
A_younger = matrix(NA, ncol = length(A_younger_cols), nrow = num_sample_ages)
for(i in 1:ncol(A_younger)) {
  A_younger[,i] = pil_cal_store[[A_younger_cols[i]]][[1]]$samples
}
A_older_cols = which(pil_data$Comment=='A_older')
A_older = matrix(NA, ncol = length(A_older_cols), nrow = num_sample_ages)
for(i in 1:ncol(A_older)) {
  A_older[,i] = pil_cal_store[[A_older_cols[i]]][[1]]$samples
}
B_younger_cols = which(pil_data$Comment=='B_younger')
B_younger = matrix(NA, ncol = length(B_younger_cols), nrow = num_sample_ages)
for(i in 1:ncol(B_younger)) {
  B_younger[,i] = pil_cal_store[[B_younger_cols[i]]][[1]]$samples
}
B_older_cols = which(pil_data$Comment=='B_older')
B_older = matrix(NA, ncol = length(B_older_cols), nrow = num_sample_ages)
for(i in 1:ncol(B_older)) {
  B_older[,i] = pil_cal_store[[B_older_cols[i]]][[1]]$samples
}

# Simulate ages for A and B separately
A_younger_single_col = apply(A_younger, 1,  'max')
A_older_single_col = apply(A_older, 1,  'min')
ages_A = runif(num_sample_ages, A_younger_single_col, 
               A_older_single_col)
ages_A = ages_A[!is.na(ages_A)]

# Do the same for B
B_younger_single_col = apply(B_younger, 1,  'max')
B_older_single_col = apply(B_older, 1,  'min')
ages_B = runif(num_sample_ages, B_younger_single_col, 
               B_older_single_col)
ages_B = ages_B[!is.na(ages_B)]
min_length = min(length(ages_A), length(ages_B))
sand_ages = round(cbind(ages_A[1:min_length], ages_B[1:min_length]))
colnames(sand_ages) = c('A', 'B')

# Now sub-sample only those where A > B
B_gt_A = function(x) x[2] > x[1]
good_order = apply(sand_ages, 1, B_gt_A)
sand_ages_final = sand_ages[good_order,]

# Create hdrs for tables
hdr_fun = function(x, p) {
  hdr = hdr.den(x, prob = p)$hdr
  cat(max(ceiling(hdr)),'-',min(floor(hdr)),'\n')
  cat(1950 - max(ceiling(hdr)),'-', 1950 - min(floor(hdr)),'\n')
}

# Now calc
hdr_fun(sand_ages_final[,'A'], 68.2)
# 714 - 290, 1236 - 1660  - Hasa
# 581 - 523, 1369 - 1427- Ich
hdr_fun(sand_ages_final[,'A'], 95.4)
# 1014 - 224, 936 - 1726  - Hasa
# 618 - 516, 1332 - 1434 - Ich
hdr_fun(sand_ages_final[,'B'], 68.2)
# 1194 - 766, 756 - 1184 - Hasa
# 945 - 673, 1005 - 1277  - Ich
hdr_fun(sand_ages_final[,'B'], 95.4)
# 1253 - 476, 697 - 1474  - Hasa
# 1016 - 602, 934 - 1348  - Ich

# Create a quick ggplot of the ages
sa = melt(sand_ages_final)
# p = ggplot(data = sa, aes(x = value)) +
#   geom_histogram(aes(fill = Var2)) + 
#   facet_grid(Var2 ~ .)
# #print(p)

# Create a better plot that includes the calibrated dates

# First create a bigger data frame with columns for 
# age, type, sample, ???
foo_fun = function(x) return(data.frame(samples = x[[1]]$samples, name = names(x), type = x[[1]]$type, layer = x[[1]]$layer))
pil_cal_df = melt(lapply(pil_cal_store, foo_fun))[,c('name', 'value','Comment', 'layer')]
colnames(pil_cal_df)[1] = 'Code'

# Add in the estimated ages of sand A and B
sand_A = data.frame(Code = 'Sand A', 
                    value = sand_ages_final[,'A'],
                    Comment = 'Sand_A',
                    layer = 'A')
sand_B = data.frame(Code = 'Sand B', 
                    value = sand_ages_final[,'B'],
                    Comment = 'Sand_B',
                    layer = 'B')
pil_final = rbind(pil_cal_df, sand_A, sand_B)

# Final plot
sec_breaks = sort(c(seq(0, 2000, by = 200),
                    869, 1454, 1611, 1677))
# Remove 1600
sec_breaks = sec_breaks[-which(sec_breaks==1600)]

p = ggplot(pil_final, aes(x = value, colour = Code, fill = Code)) + 
  geom_density(alpha = 0.5) + 
  facet_grid(layer ~ ., scales = 'free_y', 
             switch = 'both') +
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_y_continuous(breaks = NULL) +
  #theme(legend.position = 'None') +  
  labs(y = 'Probability density', x = 'Years before 1950',
       title = site) + 
  #scale_fill_viridis(option = 'D', discrete=TRUE) +
  #scale_color_viridis(option = 'D', discrete=TRUE) +
  geom_vline(xintercept = 1950 - 869, col = 'red') +
  geom_vline(xintercept = 1950 - 1454, col = 'red') +
  geom_vline(xintercept = 1950 - 1611, col = 'red') +
  geom_vline(xintercept = 1950 - 1677, col = 'red') +
  scale_x_reverse(breaks = seq(0,1500, by = 100),
                  limits = c(1500, 0),
                  sec.axis = sec_axis(~ -. +1950, name = 'CE',
                                      breaks = sec_breaks))
print(p)
#ggsave(p, file = paste0(site, '_sand_ages_20180713.pdf'))

