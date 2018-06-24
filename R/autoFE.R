#' @title autoFE
#' @description autoFE
#' @param train data.table object
#' @param valid data.table object
#' @param test data.table object
#' @param savePath save path
#' @examples
#' library(data.table)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' savePath <- "c:/tmp"
#' dir.create(savePath)
#' data(churn)
#' churn <- as.data.table(churn)
#' churn[, Area.Code:=as.factor(Area.Code)]
#' splits <- splitFrame(dt=churn, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' y = "Churn."
#' x = colnames(train)[colnames(train)!=y]
#' h2o.init()
#' dataset <- autoFE(train=train, valid=valid, test=test, x=x, y=y, savePath=savePath, verbose=TRUE)
#' @export
autoFE <- function(train, valid, test, x, y, savePath, verbose=FALSE){

  if(verbose==FALSE) h2o.no_progress()

  # 1.1 reduceDimCat
  vars_w_highlevels <- names(which(sapply(train, nlevels)>30))
  fit_reduceDimCat <- rAutoFE::reduceDimCat_fit(data=train, column_name=vars_w_highlevels, min_percentage=0.01, max_numOflevel=30)
  train <- rAutoFE::reduceDimCat_transform(data = train, fit = fit_reduceDimCat)
  valid <- rAutoFE::reduceDimCat_transform(data = valid, fit = fit_reduceDimCat)
  test  <- rAutoFE::reduceDimCat_transform(data = test,  fit = fit_reduceDimCat)
  saveRDS(fit_reduceDimCat, file.path(savePath, "fit_reduceDimCat.rda"))
  cat(">> [PART1] auto feature engineering start! \n")
  cat(">> 1.1 reduceDimCat done! \n")

  # 1.2 frequencyEncoding
  vars_w_multilevels <- names(which(sapply(train, nlevels)>2))
  fit_frequencyEncoding <- frequencyEncoding_fit(dt = train, x = vars_w_multilevels)
  train <- frequencyEncoding_transform(dt = train, fit = fit_frequencyEncoding)
  valid <- frequencyEncoding_transform(dt = valid, fit = fit_frequencyEncoding)
  test  <- frequencyEncoding_transform(dt = test, fit = fit_frequencyEncoding)
  saveRDS(fit_frequencyEncoding, file.path(savePath, "fit_frequencyEncoding.rda"))
  cat(">> 1.2 frequencyEncoding done! \n")

  # 1.3 targetEncoding
  vars_w_multilevels <- names(which(sapply(train, nlevels)>2))
  fit_targetEncoding <- targetEncoding_fit(dt = train, x = vars_w_multilevels, y = y)
  train <- targetEncoding_transform(dt = train, fit = fit_targetEncoding)
  valid <- targetEncoding_transform(dt = valid, fit = fit_targetEncoding)
  test  <- targetEncoding_transform(dt = test, fit = fit_targetEncoding)
  saveRDS(fit_targetEncoding, file.path(savePath, "fit_targetEncoding.rda"))
  cat(">> 1.3 targetEncoding done! \n")

  # 2.1 extract vi
  train_hex <- as.h2o(train)
  valid_hex <- as.h2o(valid)
  test_hex  <- as.h2o(test)
  aml <- h2o.automl(
    x = x, y = y,
    training_frame = h2o.rbind(train_hex, valid_hex),
    nfolds = 3,
    leaderboard_frame = test_hex,
    max_runtime_secs = 60*60,
    max_models = 60,
    exclude_algos = c("DeepLearning"),
    seed = 1234
  )
  baseline_ml <- h2o.getModel(model_id=as.data.frame(aml@leaderboard$model_id)[,1][1])
  baseline_vi <- rAutoFS::automlVarImp(lb=aml@leaderboard, num_of_model=5, num_of_vi=10)
  saveRDS(baseline_ml, file.path(savePath, "baseline_ml.rda"))
  saveRDS(baseline_vi, file.path(savePath, "baseline_vi.rda"))
  cat(">> [PART2] auto feature engineering with variable importance start! \n")
  cat(">> 2.1 extract vi done! \n")

  # 2.2 bestNormalize for vi
  tryCatch(
    {
      fit_bestNormalize <- rAutoFE::bestNormalize_fit(dt=train, vi=baseline_vi$top_vi)
      train <- rAutoFE::bestNormalize_transform(dt=train, fit=fit_bestNormalize)
      valid <- rAutoFE::bestNormalize_transform(dt=valid, fit=fit_bestNormalize)
      test  <- rAutoFE::bestNormalize_transform(dt=test, fit=fit_bestNormalize)
      saveRDS(fit_bestNormalize, file.path(savePath, "fit_bestNormalize.rda"))
      cat(">> 2.2 bestNormalize for vi done! \n")
    }, error = function(e) print(">> 2.2 error! skip this process! \n")
  )

  # 2.3 bin4numeric for vi
  tryCatch(
    {
      fit_bin4numeric <- bin4numeric_fit(dt=train, vi=baseline_vi$top_vi)
      train <- bin4numeric_transform(dt=train, fit=fit_bin4numeric)
      valid <- bin4numeric_transform(dt=valid, fit=fit_bin4numeric)
      test <- bin4numeric_transform(dt=test, fit=fit_bin4numeric)
      saveRDS(fit_bin4numeric, file.path(savePath, "fit_bin4numeric.rda"))
      cat(">> 2.3 bin4numeric for vi done! \n")
    }, error = function(e) print(">> 2.3 error! skip this process! \n")
  )

  # 2.4 featureCombination for vi
  tryCatch(
    {
      train <- featureComb_fit_transform(dt=train, vi=baseline_vi$top_vi)
      valid <- featureComb_fit_transform(dt=valid, vi=baseline_vi$top_vi)
      test <- featureComb_fit_transform(dt=test, vi=baseline_vi$top_vi)
      cat(">> 2.4 featureCombination for vi done! \n")
    }, error = function(e) print(">> 2.4 error! skip this process! \n")
  )

  # 2.5 weightOfEvidence for vi
  tryCatch(
    {
      fit_woe <- WoE_fit(data=train, target.var=y, pred.var=baseline_vi$top_vi)
      train <- WoE_transform(data = train, fit = fit_woe)
      valid <- WoE_transform(data = valid, fit = fit_woe)
      test <- WoE_transform(data = test, fit = fit_woe)
      saveRDS(fit_woe, file.path(savePath, "fit_woe.rda"))
      cat(">> 2.5 weightOfEvidence for vi done! \n")
    }, error = function(e) print(">> 2.5 error! skip this process! \n")
  )

  # 3.1 reduce high-levels
  tryCatch(
    {
      vars_w_highlevels2 <- names(which(sapply(train, nlevels)>30))
      fit_reduceDimCat2 <- rAutoFE::reduceDimCat_fit(data=train, column_name=vars_w_highlevels2, min_percentage=0.01, max_numOflevel=30)
      train <- rAutoFE::reduceDimCat_transform(data = train, fit = fit_reduceDimCat2)
      valid <- rAutoFE::reduceDimCat_transform(data = valid, fit = fit_reduceDimCat2)
      test  <- rAutoFE::reduceDimCat_transform(data = test,  fit = fit_reduceDimCat2)
      saveRDS(fit_reduceDimCat2, file.path(savePath, "fit_reduceDimCat2.rda"))
      cat(">> [PART3] final step for auto feature engineering start! \n")
      cat(">> 3.1 reduce high-levels done! \n")
    }, error = function(e) print(">> 3.1 error! skip this process! \n")
  )

  # 3.2 convert all char to factor
  tryCatch(
    {
      changeCols = names(which(sapply(train, is.character)))
      train <- train[, (changeCols):=lapply(.SD, as.factor), .SDcols=changeCols]
      valid <- valid[, (changeCols):=lapply(.SD, as.factor), .SDcols=changeCols]
      test  <- test[, (changeCols):=lapply(.SD, as.factor), .SDcols=changeCols]
      cat(">> 3.2 convert all char to factor done! \n")
    }, error = function(e) print(">> 3.2 error! skip this process! \n")
  )

  # output
  output <- list(train=train, valid=valid, test=test)
  return(output)
}










