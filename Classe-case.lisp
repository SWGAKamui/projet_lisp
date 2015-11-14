;;;***************Classe case***************;;;
;;**Définition de la classe
(defclass case ()
  ((val-init :initarg :val-init :reader val-init-of)
   (val-actu :initarg :val-actu :reader val-actu-of)
   (possibilite :initarg :possibilite :reader possibilite-of)
   )
  )
;;**Affichage de la classe
(defmethod print-object ((c case) stream)
  (format stream "case : ~%Valeur Initiale=~d Valeur Actuelle=~d possibilité=~d"
	  (val-init-of c)
	  (val-actu-of c)
	  (possibilite-of c)
	  )
  )
;;**Création d'une case
(defun make-case (&key (val-init 0) (val-actu 0) (possibilite '(1 2 3 4 5 6 7 8 9)))
  (make-instance 'case :val-init val-init :val-actu val-actu :possibilite possibilite)
  )
;;**Supprimer une possibilité d'une case 
(defun rem-possibilite (nombre case)
  (delete nombre (possibilite-of case))
  )
;;**Création d'une grille vide de facteur^4 ou taille^2 cases
;;Exemple de création de grille sudoku (defparameter *t* (make-grille :facteur 3))
(defun make-grille (&key (facteur 0) (taille 0))
  (if (zerop facteur)
      (if (zerop taille)
	  (format t "n'importe quoi")
	  (make-array (list taille taille) :initial-element (make-case))
	  )
      (let ((dim (* facteur facteur)))
	(make-array (list dim dim) :initial-element (make-case))
	)
      )
  )
;;Chargement d'une grille de sudoku a l'origine sous forme de tableau a 2 dimensions
(defun load-grille (grille)
  (let* ((taille (array-dimension grille 0))
	 (notre-grille (make-grille :taille taille)))
    ;cela va dépendre de comment sont donné les grilles si c'est des tableaux a deux dimensions, une liste a une dimension.... à vérifier
    )
  )

;;Solveur
;;Solveur assez simple au final
;;1 on parcours toute les cases
;;     lorsque valeur initiale différente de 0 on enlève sa possibilité des cases concerné
;;2ieme parcours tant qu'il y a des valeur actuelles=0
;;on parcours toute les cases
;;         lorsque (length possibilité=1) alors
;;                  valeur actuelle=first possibilité
;;                  rem possibilité
(defun solver (grille)
  )
