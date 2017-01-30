# Rscript extract-data.R

# reads ibex output from local file "results", writes anonymised .csv to
# "../shetland-online.csv" while printing non-anonymised participant information
# and qualitative data (i.e. any free text comments) to the screen/output

rawdata <- read.table("results", FALSE, ",", "", row.names=NULL, stringsAsFactors=FALSE, strip.white=TRUE, col.names=c("timestamp", "ip", "cont", "item", "elem", "type", "group", "field", "value"))

locations <- NULL
locations[c("lerwick", "gulberwick")] <- "Lerwick"
locations[c("ness", "cunningsburgh", "dunrossness", "sumburgh", "scousburgh", "sandwick")] <- "South"
locations[c("tingwall", "whiteness", "scalloway", "central mainland.", "burra", "outside lerwick")] <- "Central"
locations["bressay"] <- "Bressay"
locations["skeld"] <- "West"
locations[c("brae", "vidlin", "ollaberry", "hillswick", "yell", "yell/mossbank/brae", "where i stay")] <- "North" # 'where i stay' == Delting
locations["whalsay"] <- "Whalsay"

newdata <- matrix(nrow=0, ncol=13)
for (id in unique(rawdata$timestamp)) {
  pt <- subset(rawdata, timestamp==id)
  ptinfo <- c(id=id, age=subset(pt, field=="age")$value, gender=toupper(substr(subset(pt, field=="gender")$value,0,1)))
  cat("-----\n", ptinfo, subset(pt, field=="occupation")$value, "in", subset(pt, field=="current_location")$value, "grew up in", subset(pt, field=="grewup_location")$value, "parents from", subset(pt, field=="parents_location")$value, "\n")
  ptinfo["loc"] <- locations[tolower(subset(pt, field=="grewup_location")$value)]
  firstvar <- subset(pt, field=="firstvar")$value
  firstolder <- subset(pt, field=="firstolder")$value
  firstage <- subset(pt, field=="firstage")$value
  for (var in c("imp", "neg", "ynq", "whq")) {
    dt <- subset(pt, type==var)
    comment <- subset(dt, field=="comments")$value
    if (comment != "")
      cat(var, ":", comment, "\n")
    newdata <- rbind(newdata, c(ptinfo, var=var, firstvar=firstvar, self=subset(dt, field=="own_use")$value, other=subset(dt, field=="people_use")$value, firstolder=firstolder, oldervar=subset(dt, field=="older_variant")$value, firstage=firstage, young=subset(dt, field=="young")$value, old=subset(dt, field=="old")$value))#, comments=subset(dt, field=="comments")$value))
  }
  generalcomments <- subset(pt, type=="exit" & field=="comments")$value
  if (generalcomments != "")
    cat("general:", generalcomments, "\n")
}
write.csv(newdata, "../shetland-online.csv", row.names=F, quote=F)#14
#read.csv("../shetland-online.csv")
