
# Using BHPS

# What are the types of data needed? 

# We want to know the age, gender, and egoalt relationships of women



# Data - individual  ------------------------------------------------------

# To strat with, let's find the age of females in each wave 

dta_path <- "E:/data/bhps/unzipped/UKDA-5151-tab/tab/"
dta_files <- list.files(path = "E:/data/bhps/unzipped/UKDA-5151-tab/tab/", pattern = "indresp\\.tab")

all_inds <- map(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t"
)

search_patterns <- c(
  "^PID$",
  "^[A-Z]{1}SEX",
  "^[A-Z]{1}AGE$",
  "^[A-Z]HHTYPE", #household type 
  "^[A-Z]TENURE" # housing tenure
) %>% paste( collapse = "|")


# Pos. = 629	Variable = AHHTYPE	Variable label = Household Type  
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for AHHTYPE
# Value = -9	Label = Missing
# Value = 1	Label = Single Non-Elderly
# Value = 2	Label = Single Elderly
# Value = 3	Label = Couple No Children
# Value = 4	Label = Couple: dep children
# Value = 5	Label = Couple: non-dep children
# Value = 6	Label = Lone par: dep children
# Value = 7	Label = Lone par: non-dep children
# Value = 8	Label = 2+ Unrelated adults
# Value = 9	Label = Other Households


# Pos. = 630	Variable = ATENURE	Variable label = Housing tenure  
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for ATENURE
# Value = -9	Label = Missing
# Value = 1	Label = Owned Outright
# Value = 2	Label = Owned with Mortgage
# Value = 3	Label = Local Authority rented
# Value = 4	Label = Housing Assoc. rented
# Value = 5	Label = Rented from Employer
# Value = 6	Label = Rented private unfurnished
# Value = 7	Label = Rented private furnished
# Value = 8	Label = Other rented

# Pos. = 680	Variable = ANJBSP	Variable label = No. employment spells: year to Sept 1   
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for ANJBSP
# Value = -9	Label = Missing or wild
# Value = -8	Label = Inapplicable
# Value = -7	Label = Proxy respondent
# Value = 0	Label = None
# 
# Pos. = 681	Variable = ANJUSP	Variable label = No. unemployment spells: year to Sept 1 
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for ANJUSP
# Value = -9	Label = Missing or wild
# Value = -8	Label = Inapplicable
# Value = -7	Label = Proxy respondent
# Value = 0	Label = None
# 
# Pos. = 682	Variable = ANJISP	Variable label = No. inactive spells: year to Sept 1 
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for ANJISP
# Value = -9	Label = Missing or wild
# Value = -8	Label = Inapplicable
# Value = -7	Label = Proxy respondent
# Value = 0	Label = None


# Pos. = 714	Variable = ASPJB	Variable label = Whether spouse/partner employed now 
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for ASPJB
# Value = -9	Label = Missing or wild
# Value = -8	Label = No spouse/partner
# Value = 0	Label = No
# Value = 1	Label = Yes

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms ,
    pattern = search_patterns
  )
  
  out <- x[,selection]
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}SEX")]  %>% str_replace(., "SEX", "")
  PID <- out$PID
  out$PID <- NULL
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(PID = PID, WAVE = WAVE, out)
  return(out)
}

all_inds_ss <- map(
  all_inds,
  fn
) %>% 
  bind_rows() %>% 
  as_data_frame()

all_inds_ss %>% 
  mutate(sex = car::recode(
    SEX, 
    "1 = 'male'; 2 = 'female'; else = NA"
  )) %>% 
  select(-SEX) %>% 
  filter(sex == "female") -> all_females_in_bhps

rm(all_inds_ss)

rm(all_inds)


# DONE : ADD LOOKUP codes (See above) for hhtype, tenure, etc
# DONE : add wave as number 

tmp <- 1:20
names(tmp) <- LETTERS[1:20]
all_females_in_bhps %>% 
  mutate(wave = tmp[WAVE]) %>% 
  mutate(    
    hhtype = car::recode(
      HHTYPE, 
      "
        -9 = NA;
        1 = 'Single Non-Elderly';
        2 = 'Single Elderly';
        3 = 'Couple No children';
        4 = 'Couple dep children';
        5 = 'Couple non-dep children';
        6 = 'Lone parent - dep children';
        7 = 'Lone parent - non-dep children';
        8 = '2+ Unrelated adults';
        9 = 'Other households'
        
      
      "
    ),
    tenure = car::recode(
      TENURE,
      "
    -9	= NA;
    1	= 'Owned Outright';
    2	= 'Owned with Mortgage';
    3	= 'Local Authority rented';
    4	= 'Housing Assoc. rented';
    5	= 'Rented from Employer';
    6	= 'Rented private unfurnished';
    7	= 'Rented private furnished';
    8 = 'Other rented'
      "
      
    )
  ) -> all_females_in_bhps
    

write_rds(all_females_in_bhps, path = "data/all_females_in_bhps.rData")

    # Pos. = 630	Variable = ATENURE	Variable label = Housing tenure  
    # This variable is  numeric, the SPSS measurement level is nominal.
    # Value label information for ATENURE
    # Value = -9	Label = Missing
    # Value = 1	Label = Owned Outright
    # Value = 2	Label = Owned with Mortgage
    # Value = 3	Label = Local Authority rented
    # Value = 4	Label = Housing Assoc. rented
    # Value = 5	Label = Rented from Employer
    # Value = 6	Label = Rented private unfurnished
    # Value = 7	Label = Rented private furnished
    # Value = 8	Label = Other rented


# Pos. = 629	Variable = AHHTYPE	Variable label = Household Type  
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for AHHTYPE
# Value = -9	Label = Missing
# Value = 1	Label = Single Non-Elderly
# Value = 2	Label = Single Elderly
# Value = 3	Label = Couple No Children
# Value = 4	Label = Couple: dep children
# Value = 5	Label = Couple: non-dep children
# Value = 6	Label = Lone par: dep children
# Value = 7	Label = Lone par: non-dep children
# Value = 8	Label = 2+ Unrelated adults
# Value = 9	Label = Other Households


# Pos. = 630	Variable = ATENURE	Variable label = Housing tenure  
# This variable is  numeric, the SPSS measurement level is nominal.
# Value label information for ATENURE
# Value = -9	Label = Missing
# Value = 1	Label = Owned Outright
# Value = 2	Label = Owned with Mortgage
# Value = 3	Label = Local Authority rented
# Value = 4	Label = Housing Assoc. rented
# Value = 5	Label = Rented from Employer
# Value = 6	Label = Rented private unfurnished
# Value = 7	Label = Rented private furnished
# Value = 8	Label = Other rented



# # Extract relationship table  ---------------------------------------------
# 
# # Data - egoalt files -----------------------------------------------------
# 
# 
# dta_path <- "E:/data/bhps/unzipped/ukda-5151-tab/tab/"
# dta_files <- list.files(path = dta_path, pattern = "egoalt\\.tab")
# 
# all_egoalts <- map(
#   paste0(dta_path, dta_files),
#   read_delim,
#   delim = "\t"
# ) 
# 
# change_to_long <- function(DTA){
#   pid <- DTA$PID
#   DTA$PID <- NULL
#   nms <- names(DTA)
#   nm1 <- nms[1]
#   wave <- str_sub(nm1, 0, 1)
#   wave_num <- which(LETTERS[1:26] == wave)
#   nms <- str_replace(nms, wave, "")
#   names(DTA) <- nms
#   DTA %>% 
#     mutate(wave = wave_num) %>% 
#     mutate(PID = pid) %>% 
#     select(PID, wave, everything())-> DTA
#   DTA
# }
# 
# all_egoalts <- map(
#   all_egoalts,
#   change_to_long
# ) %>% 
#   bind_rows() 
# 
# 
# # First challenge: for each woman, how many alters by wave? 
# 
# all_females_in_bhps %>%  
#   mutate(alter_df = map2(PID, wave, ~ all_egoalts %>% filter(PID == .x, wave == .y))) %>% 
#   mutate(n_alters = map_int(alter_df, nrow)) -> females_with_alters
# 
# # Then work out if alter includes a natural child 
# 
# females_with_alters %>% 
#   mutate(n_children = map_int(alter_df, ~ length(which(.x$REL == 4)))) -> females_with_alters 
# 

# Save this 

# write_rds(x = females_with_alters, path = "data/females_with_alters.rData")

females_with_alters <- read_rds("data/females_with_alters.rData")
# Now to show number of children by wave and age 

females_with_alters %>% 
  select(wave, age = AGE, n_children) %>% 
  group_by(wave, age) %>% 
  summarise(mean_children = mean(n_children)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = age, fill = mean_children)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("red", "green", "blue")) + 
  coord_fixed()


# Now, similarly, let's work out the proportion of females living with at 
# least one parent by age 

females_with_alters %>% 
  mutate(n_parents = map_int(alter_df, ~ length(which(.x$REL == 13)))) %>% 
  mutate(live_with_parents = n_parents > 0) -> females_with_alters 


# Now to visualise this 

females_with_alters %>% 
  select(wave, age = AGE, live_with_parents) %>% 
  group_by(wave, age) %>% 
  summarise(mean_with_parents = mean(live_with_parents)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = age, fill = mean_with_parents)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("red", "green", "blue")) + 
  coord_fixed()

# Issue with age missing? 

# females_with_alters %>% 
#   xtabs(~ wave + AGE, .)

females_with_alters %>% 
  mutate(AGE = ifelse(AGE == -9, NA, AGE)) -> females_with_alters

females_with_alters %>% 
  select(wave, age = AGE, live_with_parents) %>% 
  group_by(wave, age) %>% 
  summarise(mean_with_parents = mean(live_with_parents)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = age, fill = mean_with_parents)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("red", "green", "blue")) + 
  coord_fixed()


# Now, live with partner

females_with_alters %>% 
  mutate(
    live_with_partner = map_int(alter_df, ~ length(which(.x$REL == 3)) > 0),
    live_with_spouse  = map_int(alter_df, ~ length(which(.x$REL == 2)) > 0)
         ) -> females_with_alters


females_with_alters %>% 
  select(wave, age = AGE, live_with_spouse) %>% 
  group_by(wave, age) %>% 
  summarise(mean_with_spouse = mean(live_with_spouse)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = age, fill = mean_with_spouse)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("red", "green", "blue")) + 
  coord_fixed()


females_with_alters %>% 
  select(wave, age = AGE, live_with_partner) %>% 
  group_by(wave, age) %>% 
  summarise(mean_with_partner = mean(live_with_partner)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = age, fill = mean_with_partner)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("red", "green", "blue")) + 
  coord_fixed()


# write_rds(x = females_with_alters, path = "data/females_with_alters.rData")


# now to flag a woman as having at least one baby 

females_with_alters %>% 
  group_by(PID) %>% 
  arrange(wave) %>% 
  mutate(n_children_last = lag(n_children)) %>% 
  select(PID, wave, age = AGE, n_children, n_children_last) %>% 
  ungroup() %>% 
  mutate(n_new_child = n_children - n_children_last) %>% 
  mutate(childless = n_children == 0) %>% 
  group_by(wave, age) %>% 
  summarise(prop_childless = mean(childless)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = age, fill = prop_childless)) +
  geom_tile() + coord_fixed()

# Other variables are needed.





####################################################################
####################################################################

# Lookups  
rel_lookup <- read_csv("data/lookups/egoalt_rel.csv")
rel_lookup$label <- str_trim(rel_lookup$label)
rel_lookup2 <- rel_lookup$label
names(rel_lookup2) <- rel_lookup$value
simple_rel_lookup <- rel_lookup$simple_label
names(simple_rel_lookup) <- rel_lookup$value

rm(rel_lookup)
rel_lookup <- rel_lookup2
rm(rel_lookup2)



#tasks 

 # 1 ) replace prefix with wave 
 # 2 ) link rel to lookup description 
 # 3) other tasks 

# 1 ) replace prefix with wave



search_patterns <- c(
  "^[A-Z]{1}HID",
  "PNO$",
  "REL$",
  "PID$"
) %>% paste(collapse = "|")

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    search_patterns
  )
  
  out <- x[,selection]
  PID <- out$PID
  out <- out %>% select_(quote(-PID))
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}HID")]  %>% str_replace(., "HID", "")
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(WAVE = WAVE, PID = PID, out)
  return(out)
}

all_egoalts_ss <- map(
  all_egoalts,
  fn
) %>% 
  bind_rows(all_inds_ss) %>% 
  as_data_frame()

# Variable with relationships defined
fn <- function(x){
  out <- x %>% select_(
    hid = ~HID, pid = ~PID, pno = ~PNO, opno = ~OPNO, opid = ~OPID
  )
  
  out$rel <- rel_lookup[as.character(x$REL)]
  
  out$rel_simple <- simple_rel_lookup[as.character(x$REL)]  
  
  
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select_(~hid, ~wave, ~pid, ~pno, ~opno, ~opid,
                         relation = ~rel,
                         simple_relation = ~rel_simple
  ) %>% as_data_frame
  return(out)
}

all_egoalts <- map_df(all_egoalts_ss, fn) 

rm(all_egoalts_ss, rel_lookup, simple_rel_lookup)



# Household composition ---------------------------------------------------


hh_composition <- all_egoalts %>% 
  group_by(hid, wave) %>% 
  summarise(
    num_hh_members = length(unique(pid)), 
    num_children = length(unique(pid[simple_relation == "child"]))
  ) %T>% print %>%  # Tee operator, operates but returns its input
  mutate(num_adults = num_hh_members - num_children)





# Now to add gender details to each 



# variable with pid, wave, sex, car_driver (derived), age, ghq

fn <- function(x){
  out <- x %>% select_(pid = ~PID, hid = ~HID, sex = ~SEX, age = ~AGE, ghq = ~HLGHQ2)
  out <- out %>% mutate(
    sex = car::recode(
      sex, 
      "1 = 'male'; 2 = 'female'; else = NA"
      ),
    ghq = ifelse(ghq < 0, NA, ghq),
    age = ifelse(age < 0, NA, age)
  )
  
  out$neigh <- NA
  if ("NEIGH" %in% names(x)){
    out$neigh <- car::recode(
      x$NEIGH,
      "
      1 = 'yes';
      2 = 'no';
      3 = 'mixed';
      else = NA
      ")
  }
  out$isced <- car::recode(
    x$ISCED,
    "
    0 = 'not defined';
    1 = 'primary';
    2 = 'low secondary';
    3 = 'low sec-voc';
    4 = 'hisec mivoc';
    5 = 'higher voc';
    6 = 'first degree';
    7 = 'higher degree';
    else = NA
    "
  )

  out$highqual <- car::recode(
    out$isced,
    "
    c('not defined', 'primary', 'secondary') = 'no further';
    c('low sec-voc', 'hisec mivoc', 'higher voc') = 'further vocational';
    c('first degree', 'higher degree') = 'further non-vocational';
    else = NA
    "
  )

  # dlo: driving licence ownership
  # co: car ownership


  out$dlo <- NA
  if (x$WAVE[1] %in% c("A", "B")){
    out$dlo[x$DRIVER==1] <- "yes"
    out$dlo[x$DRIVER==2] <- "no"
  } else {
    out$dlo[x$CARUSE==3] <- "no"
    out$dlo[x$CARUSE==1 | x$CARUSE == 2] <- "yes"
  }

  out$cu <- NA  # car use
  if (x$WAVE[1] %in% c("A", "B")){
    out$cu[x$CARUSE==1] <- "yes"
    out$cu[x$CARUSE==3] <- "no"
  } else {
    out$cu[x$CARUSE==1] <- "yes"
    out$cu[x$CARUSE==2] <- "no"

  }

  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% 
    select_(~pid, ~hid, ~wave, ~sex, ~age, ~dlo, ~cu, ~ghq, ~neigh, ~isced, ~highqual) %>% 
    as_data_frame
  return(out)
  }

all_inds_drvs <- map_df(all_inds_ss, fn) 


# # Variation of above for looking at neighbourhood characteristics ---------
# 
# # variable with pid, wave, sex, car_driver (derived), age, ghq
# 
# fn <- function(x){
#   out <- x %>% select_(
#     pid = ~PID, hid = ~HID, sex = ~SEX, age = ~AGE, ghq = ~HLGHQ2
#                        ) %>% 
#     mutate(
#       opngbha = NA, 
#       opngbhb = NA,
#       opngbhc = NA,
#       opngbhd = NA,
#       opngbhe = NA,
#       opngbhf = NA,
#       opngbhg = NA,
#       opngbhh = NA
#       
#            )
#   
#   if ("OPNGBHA" %in% names(x)) {out$opngbha = x$OPNGBHA}
#   if ("OPNGBHB" %in% names(x)) {out$opngbhb = x$OPNGBHB}
#   if ("OPNGBHC" %in% names(x)) {out$opngbhc = x$OPNGBHC}
#   if ("OPNGBHD" %in% names(x)) {out$opngbhd = x$OPNGBHD}
#   if ("OPNGBHE" %in% names(x)) {out$opngbhe = x$OPNGBHE}
#   if ("OPNGBHF" %in% names(x)) {out$opngbhf = x$OPNGBHF}
#   if ("OPNGBHG" %in% names(x)) {out$opngbhg = x$OPNGBHG}
#   if ("OPNGBHH" %in% names(x)) {out$opngbhh = x$OPNGBHH}
#   
# #   ~OPNGBHA, # feels belongs to neighbourhood
# #   ~OPNGBHB, # local friends mean a lot
# #   ~OPNGBHC, # advice obtanable locally
# #   ~OPNGBHD, # can borrow things from neighbours
# #   ~OPNGBHE, # willing to improve neighbourhood
# #   ~OPNGBHF, # plan to stay in neighbourhood
# #   ~OPNGBHG, # am similar to others in neighbourhood
# #   ~OPNGBHH # talk regularly to neighbourhood
#   
#   
#   out <- out %>% mutate(
#     sex = recode(sex, "1 = 'male'; 2 = 'female'; else = NA"),
#     ghq = ifelse(ghq < 0, NA, ghq),
#     age = ifelse(age < 0, NA, age)
#   )
#   out$neigh <- NA
#   if ("NEIGH" %in% names(x)){
#     out$neigh <- recode(
#       x$NEIGH, 
#       "
#       1 = 'yes';
#       2 = 'no'; 
#       3 = 'mixed';
#       else = NA
#       ")
#   }
#   out$isced <- recode(
#     x$ISCED, 
#     "
#     0 = 'not defined';
#     1 = 'primary'; 
#     2 = 'low secondary';
#     3 = 'low sec-voc';
#     4 = 'hisec mivoc';
#     5 = 'higher voc';
#     6 = 'first degree';
#     7 = 'higher degree';
#     else = NA
#     "
#   )
#   
#   out$highqual <- recode(
#     out$isced,
#     "
#     c('not defined', 'primary', 'secondary') = 'no further';
#     c('low sec-voc', 'hisec mivoc', 'higher voc') = 'further vocational';
#     c('first degree', 'higher degree') = 'further non-vocational';
#     else = NA
#     "
#   )
#   
# 
#   
#   out$wave <- which(LETTERS %in% x$WAVE)
#   return(out)
#   }
# 
# all_inds_nhds <- ldply(all_inds_ss, fn) %>% tbl_df
# 
# all_inds_nhds <- all_inds_nhds %>% filter(wave %in% c(8, 13, 18))
# 
# 
# write_csv(x = all_inds_nhds, path = "nhd_bhps_for_johanna.csv")




# Data - households -------------------------------------------------------




dta_path <- "E:/data/bhps/unzipped/UKDA-5151-tab/tab/"
dta_files <- list.files(path = "E:/data/bhps/unzipped/UKDA-5151-tab/tab/", pattern = "hhresp\\.tab")

all_hhlds <- map(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t"
)


search_patterns <- c(
  "^[A-Z]{1}HID",
  "^[A-Z]{1}REGION$",
  "^[A-Z]{1}LADIST",
  "^[A-Z]{1}TENURE$",
  "^[A-Z]{1}HSFLOOR$",
  "^[A-Z]{1}HSTYPE$",
  "^[A-Z]{1}HSROOM$",
  "^[A-Z]{1}HSGDN$", # accomm has terrace /garden
  "^[A-Z]{1}HSPRBH$", # noise from neighbours
  "^[A-Z]{1}HSPRBI$", # street noise 
  "^[A-Z]{1}HSPRBP$", # pollution and enviornmental problems 
  "^[A-Z]{1}HSPRBQ$", # vandalism or crime 
  "^[A-Z]{1}HSCTAX$", # council tax band 
  "^[A-Z]{1}FIEQFCB$", # Equivalised household income before housing costs 
  "^[A-Z]{1}FIEQFCA$" # Equivalised household income after housing costs 
) %>% paste(collapse = "|")

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    pattern = search_patterns
  )
  
  out <- x[,selection]
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}HID")]  %>% str_replace(., "HID", "")
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(WAVE = WAVE, out)
  return(out)
}

all_hhlds_ss <- map(
  all_hhlds,
  fn
)


# variable with pid, wave, sex, car_driver (derived), age, ghq

fn <- function(x){
  out <- x %>% select_(
    hid = ~HID, hstype = ~HSTYPE, region = ~REGION, tenure = ~TENURE,
    hh_income_before_hcosts = ~FIEQFCB,
    hh_income_after_hcosts = ~FIEQFCA
  )
  
  out <- out %>% mutate(
    hstype = car::recode(
      hstype,
      "
      0 = 'other';
      1 = 'detd house or bungalow';
      2 = 'semi detd house or bungalow';
      3 = 'end terraced house';
      4 = 'terraced house';
      5 = 'purpose built flat';
      6 = 'converted flat';
      7 = 'includes business premis';
      8 = 'bedsit multi occup';
      9 = 'bedsit other';
      else = NA
      "              
    ),
    region = car::recode(
      region, 
      "
      1 = 'inner london';
      2 = 'outer london'; 
      3 = 'rest of south east';
      4 = 'south west';
      5 = 'east anglia';
      6 = 'east midlands';
      7 = 'west midlands conurb';
      8 = 'rest of west midlands';
      9 = 'greater manchester';
      10 = 'merseyside';
      11 = 'rest of north west';
      12 = 'south yorkshire';
      13 = 'west yorkshire';
      14 = 'rest of yorkshire and humberside';
      15 = 'tyne and wear';
      16 = 'rest of north';
      17 = 'wales';
      18 = 'scotland';
      else = NA
      "
    ),
    tenure = car::recode(
      tenure, 
      "
      1 = 'owned outright';
      2 = 'owned with mortgage';
      3 = 'local authority rent';
      4 = 'housing assoc rent';
      5 = 'rented from employer';
      6 = 'rented private unfurnished';
      7 = 'rented private furnished';
      8 = 'rented other';
      else = NA
      "
    ),
    simpletenure = car::recode(
      tenure, 
      "
      c('owned outright', 'owned with mortgage') = 'owner';
      c('rented from employer', 'rented private unfurnished', 'rented private furnished') = 'private renter';
      c('local authority rent', 'housing assoc rent') = 'social renter';
      else = NA
      "
    ),
    simplehstype = car::recode(
      hstype, 
      "
      c('end terraced house','terraced house') = 'terraced';
      c(
      'detd house or bungalow',
      'semi detd house or bungalow'
      ) = 'detached';
      c(
      'purpose built flat',
      'converted flat',
      'bedsit multi occup',
      'bedsit other'
      ) = 'flat_hmo';
      'includes business premis' = 'mixed';
      else = NA
      "              
      
    )
    )
  
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select_(~hid, ~wave, ~hstype, ~simplehstype, ~region, ~tenure, ~simpletenure,
                        ~hh_income_before_hcosts,
                        ~hh_income_after_hcosts
  ) %>% as_data_frame
  return(out)
}

all_hhlds <- map_df(all_hhlds_ss, fn)



# Data - urban/rural indicator household link -------------------------------------------------------


dta_path <- "E:/data/bhps/urban_rural/UKDA-6032-tab/tab/"
dta_files <- list.files(path = dta_path, pattern = "[a-z]{1}ur01ind_protect\\.tab")

all_urbrur <- map(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t",
  col_types = "ic"
)
search_patterns <- c(
  "^[a-z]{1}hid",
  "^[a-z]{1}ur01ind$"
) %>% paste(collapse = "|")

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    search_patterns
  )
  
  out <- x[,selection]
  tmp <- names(out)
  wave <- tmp[str_detect(tmp, pattern = "^[a-z]{1}hid")]  %>% str_replace(., "hid", "")
  wave <- which(letters %in% wave)
  names(out) <- names(out) %>% str_replace_all("^[a-z]{1}", "")
  out <- data.frame(wave, out)
  return(out)
}

all_urbrur <- map_df(
  all_urbrur,
  fn
)


# join urbrur to all_hhlds 

all_hhlds <- all_hhlds %>% left_join(all_urbrur)


# recode to three states 

fn <- function(x){
  out <- x
  out$ur_group <- NA
  
  ur_scot <- car::recode(
    x$ur01ind,
    "
    c(1, 2) = 'urban';
    c(3, 4, 5, 6, 7, 8) = 'nonurban';
    else = NA
    "
  )
  
  ur_enw <- car::recode(
    x$ur01ind,
    "
    c(1, 5) = 'urban';
    c(2, 3, 4, 6, 7, 8) = 'nonurban';
    else = NA
    "
  )
  
  is_scot <- which(out$region == "scotland")
  is_engwales <- which(out$region != "scotland"  & !is.na(out$region))
  out$ur_group[is_scot] <- ur_scot[is_scot]
  out$ur_group[is_engwales] <- ur_enw[is_engwales]
  
  out$ur01ind <- NULL
  
  return(out)
}


all_hhlds <- fn(all_hhlds)
all_inds_drvs <- all_inds_drvs %>% plyr::join(all_hhlds) %>% tbl_df

all_inds_drvs <- all_inds_drvs %>% left_join(hh_composition)


write_csv(all_inds_drvs, "data/derived/bhps_driver.csv")
