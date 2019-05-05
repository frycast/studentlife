
EMA <- c("Activity",
         "Administration's response",
         "Behavior",
         "Boston Bombing",
         "Cancelled Classes",
         "Class",
         "Class 2",
         "Comment",
         "Dartmouth now",
         "Dimensions",
         "Dimensions protestors",
         "Dining Halls",
         "Do Campbell's jokes suck_",
         "Events",
         "Exercise",
         "Green Key 1",
         "Green Key 2",
         "Lab",
         "Mood",
         "Mood 1",
         "Mood 2",
         "PAM",
         "QR_Code",
         "Sleep",
         "Social",
         "Stress",
         "Study Spaces")

sensing <- c("activity",
             "audio",
             "bluetooth",
             "conversation",
             "dark",
             "gps",
             "phonecharge",
             "phonelock",
             "wifi",
             "wifi_location")

other <- c("app_usage",
           "calendar",
           "call_log",
           "dining",
           "sms")

education <- c("class",
               "deadlines",
               "grades",
               "piazza")

survey <- c("BigFive",
            "FlourishingScale",
            "LonelinessScale",
            "panas",
            "PerceivedStressScale",
            "PHQ-9",
            "psqi",
            "vr_12")

# Tables that have a start_time and end_time (or 'start' and 'end') timestamp
interval <- c("conversation", "dark", "phonecharge", "phonelock")

# Tables that have a timestamp
timestamp <- c(other,
               EMA,
               sensing[-which(sensing %in% interval)])

# Tables that only have a date
dateonly <- c("deadlines")

# Tables that have no indication of time
dateless <- c(education[-which(education %in% dateonly)],
              survey)


#timestamp_names <- c("timestamp", "date-time", "resp_time")

time_opt_list1 <- list("interval"  = c("sensing"),
                       "timestamp" = c("other", "EMA", "sensing"),
                       "dateonly"  = c("education"),
                       "dateless"  = c("education", "survey"))

time_opt_list2 <- list("interval" = interval,
                       "timestamp" = timestamp,
                       "dateonly" = dateonly,
                       "dateless" = dateless)

# These are the schemas
menu1_choices <- c("sensing", "EMA", "education", "survey", "other")

# List of tables by schema
menu2_list <- list("sensing" = sensing,
                   "EMA/response" = EMA,
                   "education" = education,
                   "survey" = survey,
                   "other" = other)


## In dataset/

# Open to wide csv
wide_csv <- c(paste0("survey/",survey),
              paste0("education/", education))

# Open to txt
txt <- c("dinning")

# Open to long format student csv
long_csv <- c("sms", "call_log", "calendar", "app_usage",
              paste0("sensing/", menu2_list[["sensing"]]))

# Open to student json
EMA_json <- paste0("EMA/response/", menu2_list[["EMA/response"]])


menu_data <- list("EMA" = EMA,
                  "sensing" = sensing,
                  "other" = other,
                  "education" = education,
                  "survey" = survey,
                  "interval" = interval,
                  "timestamp" = timestamp,
                  "dateonly" = dateonly,
                  "dateless" = dateless,
                  "time_opt_list1" = time_opt_list1,
                  "time_opt_list2" = time_opt_list2,
                  "menu1_choices" = menu1_choices,
                  "menu2_list" = menu2_list,
                  "wide_csv" = wide_csv,
                  "txt" = txt,
                  "long_csv" = long_csv,
                  "EMA_json" = EMA_json)

usethis::use_data(menu_data, internal = TRUE, overwrite = TRUE)

SL_tables <- menu2_list
names(SL_tables) <- menu1_choices
usethis::use_data(SL_tables, internal = FALSE, overwrite = TRUE)
