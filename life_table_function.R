###############################################
###--- Life expectancy function 
###############################################
###--- for 20 age groups: 0, 1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85-89, 90+ 
###--- built based on PHE LifeExpectancy calculator downloaded from https://fingertips.phe.org.uk/documents/PHE%20Life%20Expectancy%20Calculator.xlsm 


###--- First wwe calculate life expectacy for a single area and we show every stage of the calculations.
###--- The function takes as arguments the deaths and the population for the area of interest in a form of vector of length 19 representing the different age groups starting from age group 0-4.

# B: age interval ID, i
# C: age at start of interval, xi
# D: age interval, xi <= x<= x1+1
# E: interval width, ni
# F: fraction of last age interval survived, ai
# G: population years at risk, Pi ------------------------------------>>>>>>> input
# H: number of deaths in interval, Di ------------------------------------->>>>>>>>>>>>> input
# I: death rate in interval, Mi
# J: probability of dying in interval, qi
# K: number alive at start of interval, Ii
# L: number dying in interval, di
# M: person years lived in interval, Li
# N: person years lived beyond start of interval, Ti
# O: observed life expectancy at start of interval, ei
# P: sample variance of of proportion surviving in interval, Spi2
# Q: weighted variance of proportion surviving in interval, weighted Spi2
# R: sample variance of person years lived beyond start of interval, STi2
# S: sample variance of observed life expectancy at start of interval, Sei2
# T: 95% confidence interval for observed life expectancy at start of interval, Lower
# U: 95% confidence interval for observed life expectancy at start of interval, Upper


##-- example data
##---- data need to be in a sequence order from age group 0 to age group 90+.
example.population <- c(50698, 215400, 280458, 258105, 282062, 329060, 306097, 274544, 260415, 267450, 
                        311314, 324311, 296825, 271339, 284608, 228062, 162785, 111263, 58987, 26016)
example.deaths <- c(206, 37, 23, 23, 105, 162, 268, 314, 413, 584, 954, 1359,
                    1912, 2824, 4507, 5851, 7117, 8192, 7745, 6442)
##--

loopK <- function(Ki, Bi, Ji){
        column.initial <- ifelse(Bi == 1, 100000, NA)
        for(row in 2:20){
                column.initial[row] <-  (1-Ji[row-1])*column.initial[row-1]
        }
        return(round(column.initial, 0))
}  ## function to create K column which makes use of previous entry of same column

loopN.R <- function(Bi, Mi, Ni){
        column.initial <- ifelse(Bi == 20, Mi, NA)
        for(row in 19:1){
                column.initial[row] <- column.initial[row+1]+Mi[row]
        }
        return(round(column.initial, 0))
}  ## function to create N and R columns which make use of folllowing entry of same column

lifeTable <- function(data.deaths = example.deaths, data.population = example.population) {
        abridged.LT <- data.frame(Bi = seq(1, 20),
                                  Ci = c(0, 1, seq(5, 90, 5)),
                                  Di = c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                                  Fi = c(0.1, rep(0.5, 19)),
                                  Gi = data.population,    ## data input
                                  Hi = data.deaths) %>%    ## data input
                mutate(Ii = pmin(1, round(Hi/Gi, 4)),    ## function of G and H
                     Ei = ifelse(Bi == 20, round(1/Fi/Ii, 1), c(1, 4, rep(5, 17))),    ## function of F and I
                     Ji = ifelse(Bi == 20, 1, ifelse(Hi > Gi/Ei/Fi, 1, round(Ei*Ii/(1+Ei*(1-Fi)*Ii), 4))),    ## function of H, G, E, F, and I
                     Ki = loopK(Ki, Bi, Ji),    ## function of J and K
                     Li = ifelse(Bi == 20, Ki, round(Ki-lead(Ki), 0)),    ## function of Ki
                     Mi = ifelse(Bi == 20, round(Ki/Ii, 0), round(Ei*(lead(Ki)+Fi*Li), 0)),    ## function of Ki, Ei, Fi and Li
                     Ni = loopN.R(Bi, Mi, Ni),    ## function of Bi, Mi and Ni
                     Oi = ifelse(Ki == 0, 0, round(Ni/Ki, 1)),    ## function of Ni and Ki
                     Pi = ifelse(Bi == 20, 4/Hi/Ii^2, ifelse(Hi == 0, 0, Ji^2*(1-Ji)/Hi)),
                     Qi = ifelse(Bi == 20, round((Ki/2)^2*Pi, 0), round(Ki^2*((1-Fi)*Ei+lead(Oi))^2*Pi, 0)),    ## function of Ki, Pi, Fi, Ei and Oi
                     Ri = loopN.R(Bi, Qi, Ri),    ## function of Bi, Qi and Ri
                     Si = Ri/Ki^2,    ## function of Ri and Ki
                     Ti = round(Oi-1.96*sqrt(Si), 1),    ## function of Qi and Si
                     Ui = round(Oi+1.96*sqrt(Si), 1))    ## function of Qi and Si
                return(abridged.LT[abridged.LT$Bi==1,]$Oi)
}


############################# validate 
#lifeTable(example.deaths, example.population)
#lifeTable()

######################### example

# males

#test.males <- deaths_ijt %>% 
 #       mutate(mx = Cases/Population) %>%
  #      group_by(Year) %>%
   #     summarise_at(c("Cases", "Population"), sum) %>%
    #    mutate(mx = Cases/Population) %>%
     #   mutate(weights = Population/sum(Population), mx_adj = mx*weights) 


#lifeTable(data.deaths = test.males$Cases, data.population = test.males$Population)




