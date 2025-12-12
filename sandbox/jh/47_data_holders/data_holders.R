tar_load(salary)
keep <- which(grepl("RES", salary$level_display, ignore.case=TRUE) && salary$region_id == 2)
staff_names <- sort(unique(salary$smart_name[keep]))
staff_names[-(which(staff_names %in% c("0", "Post Doc", "Post Doc - Term Res 1", "Post-doctoral research associate", "Not Known",
                         "Post Doc - term RES-01","Postdoctoral - to hire -","Post Doc - Term Res-01 + 27% O+M conversion", "TBD")))]
