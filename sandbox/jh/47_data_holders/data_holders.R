tar_load(data_salary)
keep <- which(grepl("RES", data_salary$level_display, ignore.case=TRUE) && data_salary$region_id == 2)
staff_names <- sort(unique(data_salary$smart_name[keep]))
staff_names[-(which(staff_names %in% c("0", "Post Doc", "Post Doc - Term Res 1", "Post-doctoral research associate", "Not Known",
                         "Post Doc - term RES-01","Postdoctoral - to hire -","Post Doc - Term Res-01 + 27% O+M conversion", "TBD")))]
