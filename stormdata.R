stormdata <- data.table::fread("StormData.csv")
stormdata <- stormdata %>% select(-c(STATE__,
                       COUNTY,
                       BGN_RANGE:BGN_LOCATI,
                       COUNTY_END:F,
                       WFO:LONGITUDE_))
names(stormdata) <- c('Begin_Date','Begin_Time','Time_Zone',
                      'County_Name','State','Event_Type',
                      'End_Date','End_Time','Magnitude',
                      'Fatalities','Injuries','Property_Damage_Raw',
                      'Property_Damage_Multiplier', 'Crop_Damage_Raw',
                      'Crop_Damage_Multiplier', 'Remarks',
                      'Reference_No')

 
yearly <- stormdata %>% 
    mutate(year=lubridate::year(lubridate::mdy_hms(Begin_Date))) %>% 
    count(year)

recent <- stormdata %>% 
    filter(lubridate::year(lubridate::mdy_hms(Begin_Date))>=2000) %>%
    mutate(Begin_Year=lubridate::year(lubridate::mdy_hms(Begin_Date)),
           indirect=grepl('indirect', Remarks))

multiplier <- data.frame(Code=c('K','M','B'),
                         Multiplier=c(10^3,10^6, 10^9),
                         stringsAsFactors = FALSE)

recent <- recent %>% left_join(multiplier,
                               by=c('Property_Damage_Multiplier'=
                                        'Code')) %>%
    rename(Property_Multiplier=Multiplier) %>%
    left_join(multiplier,
              by=c('Crop_Damage_Multiplier'=
                       'Code')) %>%
    rename(Crop_Multiplier=Multiplier) %>%
    mutate(Property_Multiplier=ifelse(is.na(Property_Multiplier),
                                      1,
                                      Property_Multiplier),
           Crop_Multiplier=ifelse(is.na(Crop_Multiplier),
                                  1,
                                  Crop_Multiplier),
           Property_Damage=Property_Damage_Raw*Property_Multiplier,
           Crop_Damage=Crop_Damage_Raw*Crop_Multiplier)
    

evtype<- recent %>% mutate(Event_Type=gsub(',','',
                                           tolower(recent$Event_Type))) %>%
    count(Event_Type)

in_join <- function (x, y, by = NULL, mode = "inner", ignore_case = FALSE) {
    match_fun <- function(v1, v2) {
        if (ignore_case) {
            v1 <- stringr::str_to_lower(v1)
            v2 <- stringr::str_to_lower(v2)
        }
        spl1 <- strsplit(v1, " +")
        spl2 <- strsplit(v2, " +")
        
        # if we are looking for exact match, then the words in spl1
        # must have at least as many words as in spl2
        match.count <- sapply(seq_along(spl1), function(i) {
            if (length(spl1[[i]])  <= length(spl2[[i]])) {
                sum(
                    spl1[[i]] %in% spl2[[i]],
                    na.rm=TRUE)
            } else {
                0
            }
        })
    
        word.count <- sapply(seq_along(spl1), function(i) {
            length(spl1[[i]])  
        })
        
        word.count[word.count==0] <- 1
        ret <- dplyr::data_frame(include = 
                                     (match.count/word.count == 1))
        ret
    }
    fuzzy_join(x, y, by = by, mode = mode, match_fun = match_fun)
}
source("aswap_join.R")

EVTYPE <- evtype
evtype <- evtype %>% filter(n>100)

a <- evtype %>% in_join(evtype, by=c('Event_Type'='Event_Type')) %>%
    filter(Event_Type.x!=Event_Type.y)
b <- evtype %>% aswap_join(evtype, by=c('Event_Type'='Event_Type'),
                           maxDist=1) %>%
    filter(Event_Type.x!=Event_Type.y)
c <- dplyr::setdiff(b,a)

txt <- pdf_text("StormData.pdf")

myfun <- function(x) do.call("rbind", lapply(x, list))

toc <- sapply(2:4, function (i) {
    split <- stringr::str_split(txt[i],'(\r\n)')
    sapply(split, myfun)
                       })




