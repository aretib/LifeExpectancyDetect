
library(INLA)

load("all_data_males.rds")
len_i <- length(unique(data_ijtd$OfficialCode)) # 315
len_t <- length(unique(data_ijtd$Period)) # 18
len_j <- length(unique(data_ijtd$AgeGroup)) # 20
len_d <- length(unique(data_ijtd$Decile)) # 10

decile_tot <- as.vector(table(data_ijtd$Decile)) # nb of obsns in each decile
decile_areas <- decile_tot/(len_t*len_j) # nb of LAs in each decile

data_ijtd <- data_ijtd %>%
        # main effects
        arrange(Period) %>% 
        mutate(ID_t = rep(1:len_t, each = len_i*len_j)) %>%
        arrange(match(OfficialCode, england_lads$lad19cd)) %>%   #####!!!!!!!!!!!!!!!!!!!!  CARE
        mutate(ID_i = rep(1:len_i, each = len_t*len_j)) %>%  
        arrange(AgeGroup) %>%
        mutate(ID_j = rep(1:len_j, each = len_t*len_i)) %>%
        arrange(Decile) %>%
        mutate(ID_d = rep(1:10, times = decile_tot)) %>%
        # interactions
        arrange(ID_i, ID_j) %>%
        mutate(ID_ij = rep(1:(len_i*len_j), each = len_t)) %>%
        arrange(ID_i, ID_t) %>%
        mutate(ID_it = rep(1:(len_i*len_t), each = len_j)) %>%
        arrange(ID_j, ID_t) %>%
        mutate(ID_jt = rep(1:(len_j*len_t), each = len_i)) %>%
        arrange(ID_d, ID_t) %>%
        mutate(ID_dt = rep(1:(len_d*len_t), times = rep(decile_areas*len_j, each=len_t))) %>%
        arrange(ID_j, ID_i, ID_t) %>%
        glimpse()

data_ijtd_F$ID_j1 <- data_ijtd_F$ID_j
data_ijtd_F$ID_t2 <- data_ijtd_F$ID_t
data_ijtd_F$ID_t3 <- data_ijtd_F$ID_t
data_ijtd_F$ID_d1 <- data_ijtd_F$ID_d

formula_males <- data_ijtd$ObservedCases ~ 
        f(ID_j, model = "rw1") + 
        f(ID_i, model = "iid") + 
        f(ID_d, model = "rw1") +
        f(ID_t2, model = "rw1") + 
        f(ID_t, model = "rw1", group = ID_j1, constr = TRUE, rankdef = 1) + 
        f(ID_it, model = "iid") + 
        f(ID_ij, model = "iid") +
        f(ID_t3, model = "rw1", group = ID_d1, constr = TRUE, rankdef = 1)

model_males <- inla(formula_males,
                        family = "poisson",
                        data = data_ijtd_M,
                        E = data_ijtd_M$Population,
                        verbose = TRUE,
                        control.compute = list(dic = TRUE, config = TRUE),
                        control.inla = list(strategy = "gaussian", h = 0.05, numint.maxfeval = 80000000),
                        num.threads = 2)

