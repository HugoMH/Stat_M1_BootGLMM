QAIC = function(m){
    if(family(m)[[1]] != 'quasibinomial' & family(m)[[1]] != 'quasipoisson'){
      stop('family must be either "quasibinomial" ou "quasipoisson"')    }
    disp = summary(m)$dispersion
    fam = strsplit(family(m)[[1]],'quasi')[[1]][2]
    m_ = update(m,family=fam)
    -2 * logLik(m_)[[1]] / max(c(1,disp)) + 2*(length(coef(m))+1)
}