;;;***************Classe case***************;;;
;;**Définition de la classe
(defclass case ()
  ((val-init :initarg :val-init :accessor val-init-of)
   (val-actu :initarg :val-actu :accessor val-actu-of)
   (possibilite :initarg :possibilite :accessor possibilite-of)
   (nb-pos :initarg :nb-pos :accessor nb-pos-of)
   
   )
  )
;;**Affichage de la classe
(defmethod print-object ((c case) stream)
  (format stream "case : ~%Val-Init=~d Val-Actu=~d possibilité=~d Nb restante=~d"
	  (val-init-of c)
	  (val-actu-of c)
	  (possibilite-of c)
	  (nb-pos-of c)
	  )
  )
;;**Création d'une case
(defun make-case (&key (val-init 0) (val-actu 0) (possibilite '(1 2 3 4 5 6 7 8 9)) (nb-pos 9))
  (make-instance 'case
		 :val-init val-init
		 :val-actu val-actu
		 :possibilite possibilite
		 :nb-pos nb-pos)
  )
;;**Supprimer une possibilité d'une case 
(defun rem-possibilite (nombre case)
  (when (member nombre (possibilite-of case))
    (setf (nb-pos-of case) (1- (nb-pos-of case)))
    (delete nombre (possibilite-of case))
    )
  )
;;**Supprimer un nombre sur toute la ligne colonne et groupe de case.
(defun clean-possibilite (grille nombre lig col)
  
  )
;;**Création d'une grille vide de facteur^4 ou taille^2 cases
;;Exemple de crÃ©ation de grille sudoku (defparameter *t* (make-grille :facteur 3))
(defun make-grille (&key (facteur 0) (taille 0))
  (if (zerop facteur)
      (if (zerop taille)
	  (format t "N'importe quoi")
	  (make-array (list taille taille) :initial-element (make-case))
	  )
      (let ((dim (* facteur facteur)))
	(make-array (list dim dim) :initial-element (make-case))
	)
      )
  )
;;Chargement d'une grille de sudoku a l'origine sous forme de tableau a 2 dimensions et on en profite à chaque fois pour clean les possibilité
(defun load-grille (grille)
  (let* ((taille (array-dimension grille 0))
	 (notre-grille (make-grille :taille taille)))
					;cela va dépendre de comment sont données les grilles si c'est des tableaux a deux dimensions, une liste a une dimension.... A vérifier pour chaque vlaeur différente de 0 on les enlève des possibilité
    )
  )

;;Solveur
;;Solveur assez simple au final
;;2ieme parcours tant qu'il y a des valeur actuelles=0
;;on parcours toute les cases
;;         lorsque (nb-pos=1) alors
;;                  valeur actuelle=first possibilitÃ©
;;                  rem possibilitÃ©
(defun solver (grille)
  (let* ((taille (array-dimension grille 0))
	 (c (aref grille 0 0))
	 (fini 0))
    (loop while (zerop fini) do
      (setq fini 1)
      (dotimes (i taille t)
	(dotimes (j taille t)
	  (setq c (aref grille i j))
	  (when (and (zerop (val-init-of c)) (= 1 (nb-pos-of c)));il n'y a qu'une possibilité ?
	    (setf (val-actu-of c) (first (possibilite-of c)))
	    (setq fini 0)
	    (clean-possibilite grille (val-actu-of c) i j)
	    )
	  )
	)
      )
    )
  )
