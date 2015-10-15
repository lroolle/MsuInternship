
View(e$cln_data)

#** Brand

brdck <- filter(e$cln_data,is.na(BRAND))
View(brdck)
View(brdck[!duplicated(brdck$PRODUCT_ORI),])
View(brdck[!duplicated(brdck$BRAND_ORI),])
unique(e$cln_data$BRAND[which(e$cln_data$BRD_NUM > 1)])
brdd <- filter(e$cln_data,BRD_NUM > 1)
View(brdd[!duplicated(brdd$BRAND),])
brddna <- brdd[which(is.na(brdd$BRAND)),]
View(brddna[!duplicated(brddna$BRAND_ORI),])
View(brdd[!duplicated(brdd$PRODUCT_ORI),])

write.csv(brdck[!duplicated(brdck$PRODUCT_ORI),],"CN_brdnf.csv")

brdd <- filter(e$cln_data,BRD_NUM > 1)
View(brdd[which(is.na(brdd$BRAND)),])
View(brdd[!duplicated(brdd$PRODUCT_ORI),])
brdnf <- filter(e$cln_data,BRD_NUM == 0)
View(brdnf[!duplicated(brdnf$PRODUCT_ORI),])

brdck <- filter(e$cln_data,BRAND == "")
e$cln_data$BRAND[which(e$cln_data$BRAND == "")] <- NA

#** Model
mdlck <- filter(e$cln_data,MDL_NUM == 0)
mdlck <- filter(e$cln_data,is.na(MODEL))
mdlck <- filter(e$cln_data,is.na(MODEL) & !is.na(BRAND))
mdlck <- filter(mdlck,MDL_NUM == 0)
View(mdlck)
View(mdlck[!duplicated(mdlck$BRAND_ORI),])
View(mdlck[!duplicated(mdlck$PRODUCT_ORI),])
mdlck <- mdlck[!duplicated(mdlck$PRODUCT_ORI),]
write.csv(mdlck[!duplicated(mdlck$PRODUCT_ORI),],"CN_mdlnf.csv",row.names = F)

write.csv(mdlck[!duplicated(mdlck$PRODUCT_ORI),],"CN_mdlnf.csv")

mdlnf <- filter(e$cln_data,MDL_NUM > 1 & is.na(MODEL))
View(mdlnf)
View(mdlnf[!duplicated(mdlnf$BRAND_ORI),])
View(mdlnf[!duplicated(mdlnf$PRODUCT_ORI),])



mdld <- filter(e$cln_data,MDL_NUM > 1)
View(mdld)
View(mdld[!duplicated(mdld$PRODUCT_ORI),])
mdlck <- filter(e$cln_data,MODEL == "")
View(mdlck)
#** Family

fmlck <- filter(e$cln_data,is.na(FAMILY))
View(fmlck)
View(fmlck[!duplicated(fmlck$BRAND),])
View(fmlck[!duplicated(fmlck$BRAND_ORI),])
View(fmlck[!duplicated(fmlck$PRODDUCT_ORI),])

#** Segment

segck <- filter(e$cln_data,is.na(LOCAL.SEG) | is.na(REGIONAL.SEG))
View(segck)

e$cln_data$REGIONAL.SEG[index9] <- sapply(brdmdl, (function(x) {
  rgnseg <-
    unique(basepool1$REGIONAL.SEGMENT[which(paste(basepool1$BRAND, basepool1$MODEL, sep = " ") == x)])
  if (length(rgnseg) == 1)
    return(rgnseg)
  else if(length(rgnseg) >= 1)
    return(unique(rgnseg))
  else
    return(NA)
}))
#** Channel 

chl <- filter(e$cln_data,is.na(CHANNEL))
if(nrow(chl) > 0){View(chl)}

#** BDR

bdr <- filter(e$cln_data,is.na(BDR))
if(nrow(bdr) > 0 ){View(bdr)}








