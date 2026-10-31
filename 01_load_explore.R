# 01_load_explore.R — load M5, dataset overview, pre-launch vs on-sale zeros
# Memory-light: keep sales as an integer matrix (30,490 series x 1,941 days)
suppressMessages(library(data.table))
sales <- fread("data/sales_train_evaluation.csv")
meta  <- sales[, .(id,item_id,dept_id,cat_id,store_id,state_id)]
M <- as.matrix(sales[, grep("^d_",names(sales)), with=FALSE]); storage.mode(M) <- "integer"
rm(sales); invisible(gc())
nd <- ncol(M)
first_sale <- apply(M > 0, 1, function(r) which(r)[1])
onsale <- col(M) >= first_sale            # logical matrix: item launched
meta[, first_sale := first_sale]
meta[, onsale_days := nd - first_sale + 1L]
meta[, zero_days_onsale := rowSums(M == 0 & onsale)]
meta[, units := rowSums(M)]
cat("Overall zero share:", round(mean(M==0),3),
    " | pre-launch share of all days:", round(1 - sum(meta$onsale_days)/length(M),3),
    " | zero share after launch:", round(sum(meta$zero_days_onsale)/sum(meta$onsale_days),3), "\n")
summ <- function(by) meta[, .(series=.N, zero_share_after_launch=round(sum(zero_days_onsale)/sum(onsale_days),3),
                      avg_units_per_day=round(sum(units)/sum(onsale_days),2)), by=by]
print(summ("cat_id")); s1 <- summ("store_id")[order(store_id)]; print(s1)
fwrite(s1,"out/by_store.csv"); fwrite(summ("cat_id"),"out/by_cat.csv")
# save pilot slice: CA_1, all categories (wide), for later scripts
keep <- meta$store_id=="CA_1"
saveRDS(list(meta=meta[keep], M=M[keep,]), "out/ca1.rds")
