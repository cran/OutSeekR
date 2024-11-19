## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----setup--------------------------------------------------------------------
library(OutSeekR);

## ----view-data----------------------------------------------------------------
str(outliers, list.len = 5);
outliers[1:6, 1:6];
outliers[495:500, 45:50];

## ----run-1--------------------------------------------------------------------
# Set random seed for reproducibility.
set.seed(371892);

# Set up parallel processing.
future::plan(future::multisession);

outlier.test.run.1 <- detect.outliers(
    data = outliers,
    num.null = 1e3
    );

str(outlier.test.run.1, max.level = 2);

# Restore sequential processing.
future::plan(future::sequential);

## ----examine-p-values---------------------------------------------------------
head(outlier.test.run.1$p.values);
head(outlier.test.run.1$fdr);

## ----transcript-level-outlier-counts------------------------------------------
head(outlier.test.run.1$num.outliers);

## ----table--------------------------------------------------------------------
table(outlier.test.run.1$num.outliers);

## ----view-outlier-test-results-list-------------------------------------------
str(outlier.test.run.1$outlier.test.results.list);

## ----collapse-rounds----------------------------------------------------------
outlier.test.results.combined <- lapply(
    X = seq_along(outlier.test.run.1$outlier.test.results.list),
    FUN = function(i) {
        df <- outlier.test.run.1$outlier.test.results.list[[i]];
        df$round <- i;
        df <- df[, c(
            'round',
            colnames(outlier.test.run.1$outlier.test.results.list[[i]])
            )];
        }
    );
outlier.test.results.combined <- do.call(
    what = 'rbind',
    args = outlier.test.results.combined
    );
# Combining the data frames produces duplicates in the row names.  R
# will de-duplicate them, but as all the necessary information is
# included in the columns of the data frame (specifically, 'round' and
# 'transcript'), we'll simply discard the row names.
rownames(outlier.test.results.combined) <- NULL;
head(outlier.test.results.combined);

## ----distributions------------------------------------------------------------
head(outlier.test.run.1$distributions);
table(outlier.test.run.1$distributions);

## ----run-2--------------------------------------------------------------------
# Set up parallel processing.
future::plan(future::multisession);

outlier.test.run.2 <- detect.outliers(
    data = outliers,
    num.null = 1e3,
    initial.screen.method = 'fdr',
    p.value.threshold = 0.25,
    fdr.threshold = 0.05
    );

# Restore sequential processing.
future::plan(future::sequential);

str(outlier.test.run.2, max.level = 2);

# Examine p-value and FDR matrices.
head(outlier.test.run.2$p.values);
head(outlier.test.run.2$fdr);

# Check the distribution of number of outliers detected.
table(outlier.test.run.2$num.outliers);

