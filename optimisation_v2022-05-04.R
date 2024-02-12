#### Auteur : ROMANE POINSOT/MS-NUTRITION #######
#### DATE : 04 Mai 2022 ##########################

#'@author MS-Nutrition

#' @title Optimisation linéaire

#' @description  Optimisation linéaire à partir d'un modèle

#' @import dplyr
#' @import lpSolveAPI
#' @param model : Liste contenant les objets suivants :
#' const.mat : matrice des contraintes
#' const.dir : vecteurs des directions des contraintes
#' const.rhs : vecteur des valeurs des contraintes (reco)
#' f.obj : vecteur de taille y contenant les coefficients de la fonction objectif
#' direction : sens de l'optimisation (min ou max)
#' @param var_bounds : bornes inf et sup pour les variables
#' @param bounded_var : liste des numéros de colonnes (variables), qui seront bornés. Par défaut, correspond à toutes les variables
#' @param presolve: si une pré-résolution doit être effectuée ou non. Défaut : 0 (non)
#' @param compute.sens : si la sensibilité du modèle doit être calculée. Défaut : 0 (non)
#' @param save_model : enregistre toutes les contraintes et la fonction objectif (TRUE) ou pas (FALSE) dans un fichier text (format .lp)
#' @examples save_model_tf=TRUE
#'           opti1<-optimisation(model1,save_model=save_model_tf) 
#' @export


optimisation <- function (model,model_name="model",save_model=FALSE,var_bounds=NULL,bounded_var=NULL,presolve=0,compute.sens=0) {

  #Construction du modèle
  const.mat=model$const.mat
  const.dir=model$const.dir
  const.rhs=model$const.rhs
  obj=model$f.obj
  direction=model$direction
  nb_var=length(obj)
  
  #var_bounds=c(0.1,100)
  
  if (is.null(bounded_var)){
    bounded_var=seq(1:nb_var)
  }
  

  #m : nombre de contraintes = nb de lignes
  #n : nombre de variables (nombre d'ingrédients) = nb de col
  m <- dim(const.mat)[1]
  n <- dim(const.mat)[2]
  
  lprec <- make.lp(m,n)
  control <- lp.control(lprec,sense=direction)

  for (i in 1:n){
  set.column(lprec,i,const.mat[,i])
  }
  
  if(!is.null(var_bounds)){
  for (j in 1:length(bounded_var)){ #met des bornes seulement aux variables dans la liste
  set.bounds(lprec, lower =var_bounds[1], upper = var_bounds[2], columns = bounded_var[j])
  }
  }
  
  set.constr.type(lprec,const.dir)
  set.rhs(lprec,const.rhs)
  set.objfn(lprec,obj)

 # print(lprec)
  
  if(compute.sens > 0){
    control <- lp.control(lprec, presolve = "sensduals")}

if (save_model==TRUE){
write.lp(lprec, filename=paste("OUTPUT/SAVED MODELS/",model_name,".lp")) #enregistre le modèle dans un format texte pour pouvoir le visualiser
}  
  
  # Résolution du modèle
  
  status <-solve(lprec)
  
  optimisation.out <- list(direction = ifelse(direction == "min", 0, 1),
                 model=lprec,#modèle
                 x.count = n,
                 objective = obj,# vecteur de la fonction objective
                 const.count = m,
                 objval = get.objective(lprec),#valeur de la fonction objective
                 solution = get.variables(lprec), #valeurs des variables = ensemble des solutions
                 presolve = 0,
                 compute.sens = compute.sens,
                 sens.coef.from = NA,
                 sens.coef.to = NA,
                 duals = NA,
                 duals.from = NA,
                 duals.to = NA,
                 status = status) #
  

  
# Données sur la sensibilité
if(compute.sens > 0) {
  sens.obj <- get.sensitivity.obj(lprec)
  sens.rhs <- get.sensitivity.rhs(lprec)
  optimisation.out$sens.coef.from = as.double(sens.obj$objfrom)
  optimisation.out$sens.coef.to = as.double(sens.obj$objtill)
  optimisation.out$duals = as.double(sens.rhs$duals)
  optimisation.out$duals.from = as.double(sens.rhs$dualsfrom)
  optimisation.out$duals.to = as.double(sens.rhs$dualstill)
}
  optimisation.out
}
