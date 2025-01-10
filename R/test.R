data <- source("data.R")$value

f1 <- zzfig(data, change ~ week | arm,
        ytype = "both",
        ylab = "PSPRS score",
        ylab2 = "PSPRS change score",
        title = "PSPRS", title2 = ""
)
