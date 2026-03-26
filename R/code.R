
#' Graphique de performances standard
#'
#' Graphique ggplot2 observé / simulé avec indicateurs de performances.
#'
#' @param data `data.frame` des données en entrée ...
#' @param obs Colonne des observés.
#' @param sim Colonne des simulés.
#' @param .variable  Nom de la variable étudié, pour nommer les axes. facultatif.
#' @param .poids Poids de chaque mesure. facultatif.
#' @param .effet_dev Y a t'il un effet développement de la variable étudiée ?
#'   Utilisé pour le choix des indicateurs à afficher :
#'   * `FALSE` - RMSE, RRMSE, biais, N
#'   * `TRUE` - MAPE, N
#' @param .couleur couleur des points
#' @param .zoom tronquage des axes
#'
#' @returns Graphique ggplot2 observé / simulé avec indicateurs
#' @import ggplot2
#' @export
graph_perf <- function(data, obs, sim, .variable = NA, .poids = NULL, .effet_dev = FALSE, .couleur = 'blue', .zoom = FALSE){

  data <- as.data.frame(data)
  data$v_obs <- data[,obs]
  data$v_sim <- data[,sim]
  if(is.null(.poids)) data$poids <- 0.6 else data$poids <- data[,.poids]
  # 0.6 n'a pas d'influence sur le résultat vu que toutes les fiches ont le même poids.
  # Juste un bon compromis de transparence

  axes_min <- quantile(c(data$v_obs, data$v_sim),c(0.005, 0)[c(.zoom,!.zoom)], na.rm = TRUE)
  axes_max <- quantile(c(data$v_obs, data$v_sim),c(0.995, 1)[c(.zoom,!.zoom)], na.rm = TRUE)
  axes_diff <- axes_max - axes_min

  Graph <-
    ggplot(data, aes(v_sim, v_obs, alpha = 0.1+poids*0.65)) +
    geom_abline(color = 'grey70')+
    geom_point(#alpha = 0.5,
      shape = 16, size = 0.4 + 0.8/sqrt(sum(!is.na(data$v_obs))/10),
      na.rm = TRUE) +
    coord_cartesian(xlim = c(axes_min,axes_max),ylim = c(axes_min,axes_max)) +
    xlab(ifelse(is.na(.variable), sim, paste('Simulation', .variable))) +
    ylab(ifelse(is.na(.variable), obs, paste('Mesure', .variable))) +
    guides(alpha = 'none')

  # Pour un modèle de développement la plupart des indicateurs n'ont aucun sens
  if(.effet_dev){
    Graph <- Graph +
      annotate(geom = "text",
               size = 3,
               x = axes_min,
               y = axes_max,
               label= with(data,
                           paste0("MAPE = ",signif(ajorant.divers::indic(v_obs, v_sim, 'MAPE', .poids = poids),3), " %",
                                  "\nN = ",sum(!is.na(v_obs) & poids != 0))),
               hjust = "left",
               vjust = "top",
               lineheight = 0.8,
               color= .couleur)
  } else {
    Graph <- Graph +
      annotate(geom = "text",
               size = 3,
               x = axes_min,
               y = axes_max,
               label= with(data,
                           paste0("RMSE = ",signif(ajorant.divers::indic(v_obs, v_sim, 'RMSE', .poids = poids),3),
                                  "\nRRMSE = ",round(ajorant.divers::indic(v_obs, v_sim, 'RRMSE', .poids = poids),2),
                                  "\n",
                                  "\nbiais = ",signif(ajorant.divers::indic(v_obs, v_sim, 'biais', .poids = poids),2),
                                  "\nN = ",sum(!is.na(v_obs) & poids != 0))),
               hjust = "left",
               vjust = "top",
               lineheight = 0.8,
               color= .couleur)+
      annotate(geom = "text",
               size = 3,
               x = axes_min,
               y = axes_max,
               label= with(data,
                           paste0("\n\neff = ",round(ajorant.divers::indic(v_obs, v_sim, 'eff', .poids = poids),2))),
               hjust = "left",
               vjust = "top",
               lineheight = 0.8,
               color = with(data, ifelse(ajorant.divers::indic(v_obs, v_sim, 'eff', .poids = poids) < 0, 'red', .couleur)))
  }

  Graph
}


#' Graphique de performances ROC
#'
#' Figure performances adapté au analyse de type ROC. Calcul des indicateurs :
#' sensibilité, spécificité, taux de bien prédit et Youden Index (sens+spe-1)
#'
#' @param data `data.frame` des données en entrée ...
#' @param obs Colonne des observés.
#' @param sim Colonne des simulés.
#' @param seuil_obs Seuil a appliquer aux observées pour séparer positif et négatif
#' @param seuil_sim Seuil a appliquer aux simulées pour séparer positif et négatif
#'
#' Les indicateurs calculés sont :
#' sensibilité, spécificité, taux de bien prédit et Youden Index (sens+spe-1)
#' les % dans les angles correspondent à la part dans le cadrant
#'
#' @returns graphique ggplot2 avec indicateurs
#' @import ggplot2
#' @importFrom stats quantile
#' @export
graph_perf_ROC <- function(data, obs, sim, seuil_obs, seuil_sim){

  data <- as.data.frame(data)

  n_obs <- obs
  n_sim <- sim

  obs <- data[, obs] > seuil_obs
  sim <- data[, sim] > seuil_sim

  # ~~~~{    Indicateurs de performances    }~~~~
  N  <- length(obs)
  VN <- sum(!obs & !sim)
  VP <- sum(obs & sim)
  FN <- sum(obs & !sim)
  FP <- sum(!obs & sim)

  Ann_Indic <- paste('\n\nsens =',signif(VP/(FN+VP),2),
                     '\nspe =',signif(VN/(FP+VN),2),
                     '\ntbp =',signif((VN+VP)/N,2),  # taux de bien prédits
                     '\nJ =',signif(VP/(FN+VP) + VN/(FP+VN) -1,2), # sens+spe-1 Youden Index, ce qui est utilisé pour l'ajustement
                     '\nN =', N)


  # ~~~~{    proportion des points dans chaque cadran    }~~~~
  cadrans <- data.frame(x =c(-Inf,-Inf, Inf, Inf,seuil_sim, seuil_sim),
                        y1=c( Inf,seuil_obs, seuil_obs,-Inf,-Inf, Inf),
                        y2=c(-Inf,seuil_obs, seuil_obs, Inf, Inf,-Inf))

  annotations <- as.data.frame(matrix(c(VP/N, Inf, Inf,
                                        VN/N, -Inf,-Inf,
                                        FP/N, Inf, -Inf,
                                        FN/N, -Inf, Inf),
                                      byrow = TRUE, ncol = 3,
                                      dimnames = list(NULL, c('label', 'x', 'y'))))

  annotations$label <- paste(round(annotations$label,2)*100,'%')

  # ~~~~{    indicateurs ROC    }~~~~
  annotations[4,'label'] <- paste(annotations[4,'label'], Ann_Indic)


  # ~~~~{    Graphique    }~~~~
  Graph <- data |>
    ggplot(aes(.[,n_sim], .[,n_obs])) +

    # ~~~~{    Cadrans    }~~~~
    geom_polygon(data = cadrans, aes(x=x, y=y1), fill = 'red', alpha = 0.4) +
    geom_polygon(data = cadrans, aes(x=x, y=y2), fill = 'green', alpha = 0.4) +
    geom_label(data = annotations,
               aes(x, y, label = label, hjust= as.numeric(x > 0), vjust = as.numeric(y > 0)),
               label.size = 0, label.padding = unit(0.5,'lines'), fill = 'transparent') +

    geom_point(alpha = 0.6, shape = 16) +
    # coord_cartesian(xlim = c(min(df_obs_sim$INNsurINNmin), 1.1)) +
    labs(x = n_sim, y = n_obs)

  Graph
}


#' Multitude de graphiques sur une page
#'
#' Arrange et exporte plusieurs graphique en grille sur une page.
#'
#' @param list_graph Liste des graphiques à intégrer à la figure.
#' @param titre Titre de la figure.
#' @param fichier Fichier de destination de la figure.
#' @param ncol Nombre de colones de graphiques. Le nombre de ligne est déduit du nombre de graphiques à afficher.
#' @param .base_dim Dimension x, y des graphiques de base. (en inches)
#' @param .rel_widths Vecteur, largeur relative de chaque colonne.
#'   Le nombre d'éléments doit être égale au nombre de colonnes.
#' @param .rel_heights Vecteur, hauteur relative de chaque colonne.
#'   Le nombre d'éléments doit être égale au nombre de lignes.
#' @param .noms_lignes Vecteur. Label des lignes de graphique.
#' @param .noms_col Vecteur. Label des colonnes de graphiques.
#' @param .legende Graphique de égende, affiché sur la même ligne que le titre.
#'   A utiliser lorsque la légende est la même pour tous les graphiques.
#' @param .L_dim dimensions x, y de la légende (en inches).
#' @param .label_position alignement des titres de ligne et colonne: 0 à gauche, 0.5 au centre et 1 à droite.
#' @param .espace_insecable charactère utilisé comme espace insécable dans le titre.
#'
#' @section Erreurs connues:
#' une erreur a été observée dans plot_grid() pour des graph tels que :
#' `ggplot() +`
#'  `geom_point(data = df, aes(x = df[,var1], y = var2))`
#'
#'  => Les points étaient mélangés d'un graph à l'autre.
#'  Il faut privilégier `.data[[var]]` à `data.frame[[,var]]` ou `aes_string(var, 'RF_votes')`.
#'
#' @section Futurs évolutions:
#' Les rel_heights et rel_widths agrandissent la taille de chaque graph.
#' Il faudrait modifier les dims finales du graph car pour l'instant elle sont calculées pour tous les graphs à 1.
#'
#' @returns Graphique sauvegardé directement dans `fichier`.
#' @import ggplot2
#' @export
fig_grid <- function(list_graph, titre, fichier, ncol,
                     .base_dim = c(3, 2), .rel_widths = 1, .rel_heights = 1,
                     .noms_lignes = NULL, .noms_col = NULL,
                     .legende = NULL, .L_dim = c(2, 2),
                     .label_position = 1, .espace_insecable = '_'){


  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"cowplot\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }


  base_width  = .base_dim[1]
  base_height = .base_dim[2]

  nrow <- ceiling(length(list_graph)/ncol)
  nrow_vrai <- nrow
  ncol_vrai <- ncol

  # ligne de titres de colonnes
  position <- 0
  if(!is.null(.noms_col)){
    graphs_col <- list()
    for(noms_col_i in .noms_col){
      # print(position)
      position <- position +1
      graphs_col[[position]] <- cowplot::ggdraw() +
        cowplot::draw_label(noms_col_i,
                            fontface = 'bold',
                            size = base_height * 105/stringr::str_count(noms_col_i))

    }
    list_graph <- append(graphs_col, list_graph)

    if(length(.rel_heights) == 1){
      .rel_heights <- c(0.2, rep(.rel_heights, nrow))
    } else .rel_heights <- c(0.2, .rel_heights)

    nrow_vrai <- nrow+1
    if(!is.null(.noms_lignes)) .noms_lignes <- c('',.noms_lignes)

  }

  # colonne de titres de lignes
  position <- 0
  if(!is.null(.noms_lignes)){
    for(noms_ligne_i in .noms_lignes){
      # print(position)
      graph_ligne <- cowplot::ggdraw() +
        cowplot::draw_label(noms_ligne_i,
                            fontface = 'bold',
                            angle = 90,
                            size = base_height * 105/stringr::str_count(noms_ligne_i))

      list_graph <- append(list_graph,
                           list(graph_ligne),
                           position)

      position <- position + ncol +1
    }

    if(length(.rel_widths) == 1){
      .rel_widths <- c(0.2, rep(.rel_widths, ncol))
    } else .rel_widths <- c(0.2, .rel_widths)

    ncol_vrai <- ncol+1
  }

  # ~~~~{    ajout de 'marges' entre les lignes pour éviter les chevauchements    }~~~~
  for(position in 2*(1:nrow_vrai-1)*ncol_vrai){
    # graph_ligne <- ggdraw() + draw_label('test', fontface = 'bold', size= 10)

    list_graph <- append(list_graph,
                         # list(rep(ggdraw(), ncol)),

                         rep(list(NULL), ncol_vrai),
                         position)
  }

  rel_heights_ <- rep(0.05, 2*nrow_vrai)
  rel_heights_[2*(1:nrow_vrai)] <- .rel_heights

  # ~~~~{    position des titres    }~~~~
  label_X     <- c(0.01,0.5,0.99)[1+.label_position*2]
  label_hjust <- c('left',0,'right')[1+.label_position*2]

  # ~~~~{    Corps du graphique    }~~~~
  # assemblage des titres de lignes avec tous les graphs pour former le corps
  grid_graph_1 <- cowplot::plot_grid(plotlist = list_graph,
                                     ncol = ncol_vrai, .rel_widths = .rel_widths, .rel_heights = rel_heights_,
                                     labels = names(list_graph),
                                     label_size = 5*base_height,
                                     label_x = label_X,
                                     label_y = 1,
                                     hjust = label_hjust,
                                     vjust = 'bottom')

  # ~~~~{    Mise en forme du titre    }~~~~
  largeur_max <- (base_width * ncol - as.numeric(!is.null(.legende)) * .L_dim[1]) * 105
  taille_optimale <- base_width * sqrt(ncol)* sqrt(nrow) * 6.3

  titre2 <- titre
  # si le titre est vraiment trop long par rapport à la place disponible et la taille optimale, on le coupe en plusieurs lignes
  if(!grepl('\n',titre)){
    largeur_titre <- str_count(titre,'.')

    ratio_titre <- largeur_max /(taille_optimale*largeur_titre)

    # supérieur à 0.7 - tout sur la même ligne
    taille_titre <- taille_optimale * min(1, ratio_titre)

    # inférieur à 0.7 - on coupe en plusieurs lignes
    if(ratio_titre < 0.6)
      titre2 <- strwrap(titre, round(largeur_max/(0.6*taille_optimale))) |>
      paste(collapse = '\n')

  }

  nline <- stringr::str_count(titre2,'\n')+1

  if(grepl('\n',titre2)){
    # si il y a des mots longs ou des espaces insécables, il peut être impossible de faire suffisamment de lignes
    largeur_titre <- stringr::str_split(titre2, '\n') |>
      unlist() |>
      stringr::str_count('.') |>
      max()
    titre2 <- strwrap(titre, largeur_titre) |>
      paste(collapse = '\n') # ajustement de la taille des lignes en fonction de la plus grande
    nline <- stringr::str_count(titre2,'\n')+1

    ratio_titre <- largeur_max /(taille_optimale*largeur_titre)

    # taille mini entre le max qu'on peut mettre en largeur et le ce qui serra élégant en hauteur
    taille_titre <- taille_optimale * min(1, ratio_titre, (0.5 + 0.6 * nline)/nline * 0.9)

  }

  titre2 <- gsub(.espace_insecable, ' ', titre2)

  # place pour le titre en haut du graph. 1 = la hauteur d'un graph
  hauteur_titre <- 2*(taille_titre / 105 * nline)/base_height + min(0.4, 0.01 * nrow)
  # hauteur_titre <- taille_titre / 110 * nline + 0.1 + min(0.5, 0.01 * base_height * nrow)

  # le titre est un graphique
  graph_titre <- cowplot::ggdraw() + cowplot::draw_label(titre2, fontface='bold', size = taille_titre)

  # si il y a une légende, elle est sur la même ligne que le titre
  if(!is.null(.legende)){
    graph_titre <- cowplot::plot_grid(graph_titre, .legende, ncol=2, .rel_widths = c(largeur_max/105, .L_dim[1]))
    hauteur_titre <- max(hauteur_titre, .L_dim[2]/base_height)
  }

  # ~~~~{    Assemblage de la ligne titre avec le corps du graph    }~~~~
  grid_graph_2 <- cowplot::plot_grid(graph_titre, grid_graph_1, ncol=1, .rel_heights = c(hauteur_titre, nrow)) # plus il y a de colones, plus le titre est gros

  # ~~~~{    Enregistrement    }~~~~
  cowplot::save_plot(fichier, grid_graph_2,
                     ncol = 1, nrow = 1,
                     base_height=(hauteur_titre + nrow)*base_height, #pour que la hauteur standard corresponde bien à un graph
                     base_width = ncol*base_width,
                     limitsize  = FALSE)
}




#' Boxplot Pré-configuré
#'
#' Boxplot selon un facteur x avec test de Tukey, classés selon la médiane des groupes.
#'
#' @param data data.frame des données à représenter.
#' @param x Colonne de l'axe x: Catégories.
#' @param y Colonne de l'axe y: Valeurs.
#' @param n_min Les catégories pour lesquelles N < n_min ne sont pas représentées.
#' @param .keep_order Garde l'ordre des catégories (facteur).
#'   Sinon elles sont réorganisées par ordre décroissant de leur médiane
#' @param .log Échelle logarithmique
#'
#' @returns Graphique ggplot2.
#' @import ggplot2 dplyr stringr
#' @importFrom stats median aov as.formula reorder
#' @export
boxplot2 <- function(data, x, y, n_min = 5, .keep_order = FALSE, .log = FALSE){

  if (!requireNamespace("agricolae", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"agricolae\" pour réaliser le test de Tukey.",
      call. = FALSE
    )
  }

  data <- as.data.frame(data)
  data$x <- as.factor(data[,x])
  data$y <- data[,y]


  # ~~~~{    Classification en groupes distincts    }~~~~
  # library(agricolae)
  # library(tibble)
  ana_var <- aov(as.formula('y~x'), data = data)
  groupes_i <- agricolae::HSD.test(ana_var, 'x')$groups # test de Tukey
  groupes_i <- tibble::rownames_to_column(groupes_i, 'x')

  infos <- data |>
    filter(!is.na(y)) |>
    group_by(x) |>
    summarise(N = length(x),
              med = median(y, na.rm=TRUE)) |>
    filter(N > n_min) |>
    inner_join(groupes_i, by = 'x') |>
    select(-y)

  # ~~~~{    nombre de décimaux des médanes affichées    }~~~~
  med_s <- signif(mean(abs(infos$med)), 2)
  nbr_chiffres <- if(str_detect(med_s, '\\.')) str_count(med_s)-2 else 0

  # ~~~~{    Ordre d'affichage des classes    }~~~~
  data_graph <- data |>
    right_join(infos, by = 'x')  |>
    mutate(x = factor(x, levels = levels(data$x)))

  if(!.keep_order)
    data_graph <- data_graph |>
    #ordre en fonction de la médiane. Si les médiane sont identiques, en fonction de la moyenne
    mutate(x = reorder(x, y, mean, na.rm=TRUE)) |>
    mutate(x = reorder(x, y, median, na.rm=TRUE))

  # ~~~~{    Hauteur totale pour ajustement    }~~~~
  y_span <-  max(data_graph$y, na.rm = TRUE) - min(data_graph$y, na.rm = TRUE)
  Y_MIN <- min(data_graph$y, na.rm = TRUE)
  Y_MAX <- max(data_graph$y, na.rm = TRUE)

  if(.log){
    Y_MIN <- Y_MIN / 10^(y_span*0.005)
    Y_MAX <- Y_MAX * 10^(y_span*0.005)
  } else {
    Y_MIN <- Y_MIN - y_span/40
    Y_MAX <- Y_MAX + y_span/30
  }

  # ~~~~{    Figure    }~~~~
  figure <- ggplot(data_graph, aes(x, y)) +

    # annotation N en bas
    geom_label(data = infos, aes(x, label = N),
               y = -Inf, vjust = 'bottom',
               linewidth = 0, label.padding = unit(0.2,'lines'),
               fill = 'transparent',
               color = 'black') +
    annotate(geom = 'label',
             x = -Inf,
             y = ifelse(.log, 0,-Inf),
             hjust = 1,
             vjust = 'bottom',
             linewidth = 0,
             label.padding = unit(0.2,'lines'),
             fill = 'transparent',
             label = 'N',
             color = 'black') +

    # annotation médiane en haut
    geom_label(data = infos, aes(x, label = paste(round(med, nbr_chiffres), groups)),
               y = Inf, vjust = 'top',
               linewidth = 0,
               label.padding = unit(0.2,'lines'),
               fill = '#85e085',
               color = 'black') +
    annotate(geom = 'label',
             x = -Inf,
             y = Inf,
             hjust = 1,
             vjust = 'top',
             linewidth = 0,
             label.padding = unit(0.2,'lines'),
             fill = 'transparent',
             label = 'Médiane',
             color = '#85e085',
             fontface="bold") +

    # BOXPLOT (at last)
    geom_boxplot(na.rm = TRUE, fill = '#ffff99') +

    # mise en forme, marges, espacements, etc.
    coord_cartesian(ylim = c(Y_MIN, Y_MAX),
                    xlim = range(as.numeric(data_graph$x)),
                    clip = 'off') +
    theme(axis.text.x = element_text(angle=30, hjust = 1),
          plot.margin = unit(c(0.5,0.5,0.5,1), "lines"))+
    xlab(x)+ ylab(y)

  # échelle logarithmique
  if(.log)
    figure <- figure +  scale_y_log10()

  figure
}


#' Figure RandomForest
#'
#' Génère et enregistre la comparaison des %IncMSE et des IncNodePurity.
#'
#' @param random_forest Résultat de l'exécution de la fonction randomforest du package RandomForest.
#' @param titre Tite du Graphique.
#' @param fichier Fichier ou enregistre la figure.
#' @param .expl_var Doit on afficher le bas de page explicatif des variables représenté ?
#'
#' @returns Sauvegarde dan `fichier` la figure d'analyse de la RandomForest.
#' @import ggplot2 dplyr
#' @importFrom stats reorder
#' @export
rf_fig <- function(random_forest, titre = names(random_forest), fichier, .expl_var = FALSE){

  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"cowplot\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }
  imp <- randomForest::importance(random_forest) |> as.data.frame()
  imp <- imp[,ncol(imp)-1:0]
  name_indics <- names(imp) #pour une RF categoriel, le 2ème indicateur est IncNodePurity, pour une RF numériques, ce sera MeanDecreaseGini

  imp$var <- rownames(imp)
  names(imp) <- c("indic1","indic2","var") # % de %IncMSE pose problème

  imp2 <- imp |>
    arrange(-indic1) |>
    mutate(var = reorder(var, indic1))

  liste_graph_rf <- list()

  liste_graph_rf[[name_indics[1]]] <-
    ggplot(imp2[1:min(30,nrow(imp2)),],aes(indic1, var)) +
    geom_point(aes(color = grepl('aleatoire', var))) +
    scale_color_manual(values = c('TRUE' = 'red', 'FALSE' = 'black')) +
    guides(color = 'none') +
    ylab(NULL) +
    xlab(name_indics[1])

  liste_graph_rf[[name_indics[2]]] <-
    ggplot(imp2[1:min(30,nrow(imp2)),],aes(indic2, var)) +
    geom_point(aes(color = grepl('aleatoire', var))) +
    scale_color_manual(values = c('TRUE' = 'red', 'FALSE' = 'black')) +
    guides(color = 'none') +
    ylab(NULL) +
    xlab(name_indics[2]) +
    theme(axis.text.y = element_blank())

  if(.expl_var){
    liste_graph_rf[['A']] <- cowplot::ggdraw() + cowplot::draw_label("\nAmélioration des performances lorsque la variable est utilisée\n",
                                                                     size = 10, hjust = 0, vjust = 0, x = 0.1, y = 0.2)
    liste_graph_rf[['B']] <- cowplot::ggdraw() + cowplot::draw_label("Discrimination permise par la variable.\nBiais possible en faveur des variables\npermettant un grand nombre de classes.",
                                                                     size = 10, hjust = 0, vjust = 0, x = 0.1, y = 0.2)
    names(liste_graph_rf)[3:4] <- ''
  }


  cowplot::fig_grid(list_graph = liste_graph_rf,
                    titre      = titre,
                    fichier    = fichier,
                    ncol       = 2, rel_heights = c(1, if(.expl_var) 0.1 else NULL),
                    base_dim   = c(4.5, ifelse(.expl_var, 2.5, 4)), rel_widths = c(0.66,0.34))
}



#' Carto : dans quel département
#'
#' Identification du département à partir de coordonnées GPS.
#'
#' @param data Tableau de données.
#' @param long Colonne de longitude.
#' @param lat Colonne de lattitude.
#'
#' @returns liste avec :
#' * Le tableau `data` agrémenté d'une colonne `departement`.
#' * Un tableau répertoriant les centroïdes des départements.
#' @import dplyr
#' @export
quel_dep <- function(data, long = 'longitude', lat = 'latitude'){


  if (!requireNamespace("sp", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"sp\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"sf\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  if (!requireNamespace("maps", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"maps\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  # ~~~~{    reférentiel    }~~~~

  # fond de carte
  france_map <- ggplot2::map_data('france')


  # ~~~~{    Conversion en format sf    }~~~~

  # référentiel
  crs.lambert93 <- sp::CRS('+init=epsg:2154')


  # conversion du fond de carte
  list_ploygons <- list()
  for(reg_i in unique(france_map$region)){
    france_map_i <- filter(france_map, region == reg_i)
    list_ploygons[[reg_i]] <- sp::Polygons(list(sp::Polygon(france_map_i[,c('long','lat')])), reg_i)
  }

  st_map <- sp::SpatialPolygons(list_ploygons, proj4string=crs.lambert93)

  centroides <- sp::coordinates(st_map) |> data.frame()
  names(centroides) <- c('lat','long')
  centroides <- tibble::rownames_to_column(centroides, 'departement')

  sf_map <- sf::st_as_sf(st_map)

  # conversion des points
  data <- as.data.frame(data)
  data$x <- data[,long]
  data$y <- data[,lat]

  pnts_trans <- select(data, x, y) |>
    sp::SpatialPoints(proj4string = crs.lambert93) |>
    sf::st_as_sf()

  # ~~~~{    intersection    }~~~~
  data$departement <- apply(sf::st_intersects(sf_map, pnts_trans, sparse = FALSE), 2,
                            function(col) {
                              names(sf_map[col, ]$geometry)
                            })

  list(data = data, centroides = centroides)

}




#' Palette de couleurs extra-large
#'
#' Palette supportant jusqu'à 50 couleurs à peut près discernables.
#'
#' Attention: Non adapté aux daltoniens.
#' La distinguibilité depend fortement de l'écran et des paramètres d'affichage.
#'
#' @param N Le nombre couleurs nécessaires dans la palette.
#'
#' @returns palette de couleurs hexadécimal (vecteur).
#' @import dplyr
#' @importFrom stats approx
#' @export
#'
#' @examples
#' N_col = 4
#' barplot(rep(1,N_col), col = mega_Palette(N_col))
mega_Palette <- function(N){

  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"colorspace\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  cadre <-
    data.frame(X = c(      0,        30,     110,      140,         160,      210,        230,         280,        320,        360),
               Y = c(rouge=0, orange=25, anis=90, vert=140, glauque=160, cyan=210, violet=250, magenta=280, fuchia=320, rouge2=360))

  n_arc_en_ciel <- min(4,ceiling(N/10))


  # n1 <- ceiling(N/4)
  # n2 <- ceiling((N-n1)/3)
  # n3 <- ceiling((N-n1-n2)/2)
  # n4 <- N-n1-n2-n3


  LS_arc_en_ciels <- data.frame(Lightness = c(0.5,0.75, 0.5, 0.25)[1:n_arc_en_ciel],
                                Saturation = c(1,1,0.4,1)[1:n_arc_en_ciel])



  palette_HLS <- data.frame()
  N_restant <- N

  i=1
  for(i in 1:n_arc_en_ciel){

    n <- ceiling(N_restant/(n_arc_en_ciel-i+1))
    N_restant <- N_restant-n

    palette_HLS_i <- data.frame(Hue = approx(cadre$X, cadre$Y, xout = seq(0, 360, length.out = n+1))$y,
                                Lightness = LS_arc_en_ciels[i, 'Lightness'],
                                Saturation = LS_arc_en_ciels[i, 'Saturation'])

    palette_HLS <- bind_rows(palette_HLS, palette_HLS_i[1:n,])

  }

  with(palette_HLS, colorspace::HLS(Hue, Lightness, Saturation)) |> colorspace::hex()

}





#' Liste de marqueurs colorés pour Leaflet
#'
#' Édite une liste de marqueurs colorés utilisable dans leaflet
#'
#' @param palette Palette de couleur sous forme `list(name = couleur_hexadecimal)`
#'
#' @returns liste de forme `leaflet::iconList()`
#' @export
#'
#' @examples
#'
#' list_col <- list('rouge'="#FF0000",
#'                  'vert'='#C5FF00',
#'                  'bleu'='#00FFFF',
#'                  'violet'='#9100FF')
#'
#' my_markers <- leaf_colored_markers(list_col)
#'
#' leaflet::leaflet() |>
#' leaflet::addMarkers(lat = c(0,0,1,1),
#'                     lng = c(0,1,0,1),
#'                     icon = my_markers[names(list_col)])

leaf_colored_markers <- function(palette){

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop(
      "Vous devez installer le package \"leaflet\" pour utiliser cette fonction.",
      call. = FALSE
    )
  }

  path_ombre <- system.file(                                                      # chemin d'accès vers l'ombre du marqueur par défaut de Leaflet,
    "htmlwidgets/lib/leaflet/images/marker-shadow.png",                           #  dont la forme est suffisamment proche de celle du svg qu'on utilise
    package = "leaflet"                                                           #  (on ne peut pas utiliser le dit marqueur par défaut parce qu'il est en .png et donc que la colorisation serait beaucoup plus complexe qu'avec un .svg)
  )

  temp_dir <- tempdir()                                                           # Un emplacement temporaire pour stocker les icônes le temps de les utiliser.

  list_icons <- list()
  for (i in names(palette)) {

    path <- paste0(temp_dir, "/icon_", i, ".svg")                                 # La direction où on va enregistrer temporairement les icônes colorées.

    svg_color <- gsub("#000000", palette[i], svg_black)                           # Dans le fichier .svg on remplace la couleur noir par la couleur qu'on veut, en format hexadécimal également.
    writeLines(svg_color, path)                                                   # On enregistre le svg dans le dossier temporaire

    list_icons[[i]] <-                                                            # Les icônes sont dans une liste nommée dont le nom correspond à l'identifiant de l'exploitation
      leaflet::makeIcon(iconUrl = path,                                           # On lit le fichier qu'on vient d'écrire (ça a l'air très con mais la fonction permet uniquement d'aller chercher une image en dehors de l'appli)
                        iconWidth = 24,                                                    # On définit la taille et la position de l'image par rapport au point
                        iconHeight = 24,
                        iconAnchorX = 12,
                        iconAnchorY = 24,

                        shadowUrl = path_ombre,                                            # idem pour ajouter l'ombre
                        shadowWidth = 41,
                        shadowHeight = 41,
                        shadowAnchorX = 12,
                        shadowAnchorY = 41
      )
  }
  list_icons <- structure(list_icons, class = "leaflet_icon_set")                 # pour que la liste soit reconnue comme une liste d'icônes

}











#' Sauvegarde une figure en png et/ou svg
#'
#' @param graph figure
#' @param fichier emplacement ou enregistrer la figure
#'    .png ou .svg à la fin pour limiter l'enregistrement à un seul format
#' @param width largeur en inches
#' @param height hauteur en inches
#' @param .res résolution pour le png, en ppi
#' @param .silence Désactive le témoin d'activation "."
#'
#' @export
#'
sauv_graph <- function(graph, fichier, width, height, .res = 200, .silence = FALSE){

  go_png <- !stringr::str_detect(fichier, 'svg$')
  go_svg <- !stringr::str_detect(fichier, 'png$')

  if(go_png){
    fichier |>
      paste0('.png') |>
      stringr::str_remove('(?<=\\.png)\\.png') |>
      png(width, height, units='in', res = .res)

    suppressMessages(print(graph))

    dev.off()
  }

  if(go_svg){
    fichier |>
      paste0('.svg') |>
      stringr::str_remove('(?<=\\.svg)\\.svg') |>
      svg(width, height)

    suppressMessages(print(graph))

    dev.off()
  }

  if(!.silence) cat('.')
}








