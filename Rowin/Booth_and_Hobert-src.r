
library(lme4)
library(purrr)  # For looping simultaneously over two lists
data(finance, package = "smdata")

(fit <- lme4::glmer(corr ~ targtop + cho + easyfoil + (cho + targtop - 1 | item), data = finance, family = binomial))

# Extract within-group fitted RE covariance matrix
Lambda_hat = getME(fit, "Tlist")[[1]]
Sigma_hat = Lambda_hat %*% t(Lambda_hat)
Sigma_hat_inv = solve(Sigma_hat)

# Divide data into groups
group_labels = getME(fit, "flist")[[1]]
design_matrix = model.frame(fit)
all_groups = split(design_matrix, group_labels)

# Also divide RE design matrix into groups
full_Z_mat = data.frame(getME(fit, "mmList")[[1]])
all_Zs = split(full_Z_mat, group_labels)


# Loop over groups
all_cov_mats = purrr::map2(all_groups, all_Zs, ~{
    this_group = .x
    this_Z = as.matrix(.y)

    mu_hat = predict(fit, newdata = this_group, type = "response")
    this_W = diag(mu_hat * (1 - mu_hat))

    this_A = t(this_Z) %*% this_W %*% this_Z

    this_pred_cov_inv = this_A + Sigma_hat_inv
    this_pred_cov = solve(this_pred_cov_inv)

    return(this_pred_cov)
})



# lme4 implementation
RE_preds = as.data.frame(ranef(fit))

