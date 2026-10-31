# 04_baselines.R — baseline forecasts for CA_1, last 28 days held out (d_1914–d_1941)
# Compares: seasonal naive, 28-day mean, and a 28-day mean that ignores days flagged as stockouts
suppressMessages(library(data.table))
d <- readRDS("out/ca1.rds"); meta <- d$meta; M <- d$M
H <- 28; nd <- ncol(M); T0 <- nd - H            # train through day 1913
cal <- fread("data/calendar.csv")
closed <- which(colSums(M) == 0)
# re-run detector on TRAINING data only (no peeking at the test window)
source_detect <- function() {
  e <- new.env(); lines <- readLines("R/02_stockout_detect.R")
  s <- grep("^detect <- function", lines); f <- grep("^}", lines); f <- f[f > s][1]
  eval(parse(text = c("WIN<-56;MINHIST<-28", lines[s:f])), envir = e); e$closed <- closed; e$detect }
detect <- source_detect()
keep <- which(meta$first_sale <= T0 - 56)         # items with enough history
Mt <- M[keep, 1:T0]; Y <- M[keep, (T0+1):nd]; m <- meta[keep]
mask <- matrix(FALSE, nrow(Mt), ncol(Mt))
for (k in seq_len(nrow(Mt))) {
  r <- detect(Mt[k,], m$first_sale[k])
  if (!is.null(r)) { r <- r[p < 1e-4 & L <= 60]; for (q in seq_len(nrow(r))) mask[k, r$start[q]:r$end[q]] <- TRUE }
}
last <- (T0-27):T0
snaive <- Mt[, (T0-6):T0][, rep(1:7, 4)]                         # same weekday last week
ma28   <- matrix(rowMeans(Mt[, last]), nrow(Mt), H)
tmp <- Mt[, last]; tmp[mask[, last] | matrix(last %in% closed, nrow(Mt), 28, byrow=TRUE)] <- NA
ma28c  <- matrix(rowMeans(tmp, na.rm=TRUE), nrow(Mt), H); ma28c[is.na(ma28c)] <- ma28[is.na(ma28c)]
score <- function(F, nm) data.table(method=nm, MAE=mean(abs(Y-F)), RMSE=sqrt(mean((Y-F)^2)), bias=mean(F-Y),
                                    total_fcst=round(sum(F)), total_actual=sum(Y))
aff <- rowSums(mask[, last]) > 0
res <- rbind(score(snaive,"Seasonal naive (same weekday last week)"), score(ma28,"28-day average"),
             score(ma28c,"28-day average, stockout days removed"))
Y_all <- Y; Y <- Y[aff,,drop=FALSE]
res2 <- rbind(score(ma28[aff,,drop=FALSE],"28-day average"), score(ma28c[aff,,drop=FALSE],"28-day average, stockout days removed"))
Y <- Y_all
num <- function(x) { x[, c("MAE","RMSE","bias") := lapply(.SD, round, 3), .SDcols=c("MAE","RMSE","bias")]; x }
cat("Items evaluated:", nrow(Mt), "\n"); print(num(res)[])
cat("\nItems with a flagged stockout in the last 28 training days:", sum(aff), "\n"); print(num(res2)[])
fwrite(res,"out/baselines_all.csv"); fwrite(res2,"out/baselines_affected.csv")

# ---- Fair scoring: the test window itself contains stockouts (observed sales = 0 because shelf empty).
# Score only test days NOT inside a flagged stockout run (detector run on the full series).
tmask <- matrix(FALSE, nrow(Mt), H)
for (k in seq_len(nrow(Mt))) {
  r <- detect(M[keep[k], ], m$first_sale[k])
  if (!is.null(r)) { r <- r[p < 1e-4 & L <= 60]
    for (q in seq_len(nrow(r))) { dd <- intersect(r$start[q]:r$end[q], (T0+1):nd) - T0; if (length(dd)) tmask[k, dd] <- TRUE } }
}
fair <- function(F, nm, rows) { ok <- !tmask[rows,,drop=FALSE]; E <- (F[rows,,drop=FALSE] - Y[rows,,drop=FALSE])
  data.table(method=nm, MAE=round(mean(abs(E[ok])),3), RMSE=round(sqrt(mean(E[ok]^2)),3), bias=round(mean(E[ok]),3), days_scored=sum(ok)) }
cat("\nTest days that are themselves flagged stockouts:", sum(tmask), "of", length(tmask), "\n")
allr <- seq_len(nrow(Mt)); affr <- which(aff)
f1 <- rbind(fair(snaive,"Seasonal naive",allr), fair(ma28,"28-day average",allr), fair(ma28c,"28-day average, stockout days removed",allr))
f2 <- rbind(fair(ma28,"28-day average",affr), fair(ma28c,"28-day average, stockout days removed",affr))
cat("All items, non-stockout test days:\n"); print(f1)
cat("Affected items, non-stockout test days:\n"); print(f2)
fwrite(f1,"out/fair_all.csv"); fwrite(f2,"out/fair_affected.csv")
