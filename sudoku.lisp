(defparameter +n+ 3);;Facteur d'origine d'un sudoku
(defparameter +taille+ 9);;taille d'origine d'un sudoku
;;;*****************************************;;;
;;;***************CLASSE CASE***************;;;
;;;*****************************************;;;
;;**Définition de la classe**;;
(defclass class_case ()
  ((val-init :initarg :val-init :accessor val-init-of)
   (val-actu :initarg :val-actu :accessor val-actu-of)
   (possibilite :initarg :possibilite :accessor possibilite-of)
   (nb-pos :initarg :nb-pos :accessor nb-pos-of)
   
   )
  (:documentation "Classe case qui possède une valeur initiale, une valeur actuelle, une liste de possibilités restantes et le nombre de possibilités restantes")
  )

(defgeneric rem-possibilite (nombre c)
  (:documentation "Enlève un nombre de la liste des possibilités et réduit le nombre de possibilité (cette variable supplémentaire évite la perte de temps de l'appel de (length liste) à chaque fois que l'on veut vérifier le nombre d'élément")
  )
(defgeneric clear-possibilite (c)
  (:documentation "Enlève la liste des possibilités et met le nombre de possibilité à 0")
  )

(defgeneric print-object (object stream)
  (:documentation "Ecrit toute les valeurs de la classe case")
  )

;;**Affichage de la classe**;;
(defmethod print-object ((c class_case) stream)
  (format stream "case : ~%Val-Init=~d Val-Actu=~d possibilité=~d Nb restante=~d"
	  (val-init-of c)
	  (val-actu-of c)
	  (possibilite-of c)
	  (nb-pos-of c)
	  )
  )

;;;********************************************************;;;
;;;***************SUPPRESSION DE POSSIBILITE***************;;;
;;;********************************************************;;;
;;**Supprimer une possibilité d'une case**;;
;;On doit faire un setf pour le delete car si on supprime le premier élément delete n'est plus "destructif"
(defmethod rem-possibilite (nombre (c class_case))
  (when (member nombre (possibilite-of c))
    (setf (nb-pos-of c) (1- (nb-pos-of c)))
    (setf (possibilite-of c) (remove nombre (possibilite-of c)))
    )
  )

;;**Supprimer toutes les possibilités d'une case**;;
(defmethod clear-possibilite ((c class_case))
  (setf (possibilite-of c) nil)
  (setf (nb-pos-of c) 0)
  )

;;**Supprimer un nombre sur toute la ligne colonne et groupe de case.**;;
(defun clean-possibilite (grille nombre lig col)
  (let* ((modl (* (floor lig +n+) +n+))
	 (modc (* (floor col +n+) +n+))
	 )
    (dotimes (l +taille+ t)
      (rem-possibilite nombre (aref grille lig l))
      (rem-possibilite nombre (aref grille l col))
      )

    (dotimes (bl +n+ t)
      (dotimes (bc +n+ t)
	(rem-possibilite nombre (aref grille (+ bl modl) (+ bc modc)))
	)
      )
    )
  )

;;;*********************************************************;;;
;;;***************CREATIONS ET INITIALISATION***************;;;
;;;*********************************************************;;;
;;**Création d'une case**;;
(defun make-case (&key (val-init 0) (val-actu 0) (possibilite '(1 2 3 4 5 6 7 8 9)) (nb-pos 9))
  (make-instance 'class_case
		 :val-init val-init
		 :val-actu val-actu
		 :possibilite possibilite
		 :nb-pos nb-pos)
  )

;;**Création d'une grille vide de facteur^4 ou taille^2 cases**;;
;;Exemple de création de grille sudoku (defparameter *t* (make-grille :facteur 3))
(defun make-grille (&key (facteur 0) (taille 0))
  (unless (and (zerop facteur) (zerop taille))
    (if (zerop taille)
	(unless (= 3 facteur)
	  (setq +n+ facteur)
	  (setq +taille+ (* facteur facteur))
	  )
	(unless (= 9 taille)
	  (setq +n+ (isqrt taille))
	  (setq +taille+ taille)
	  )
	)
    (let ((grille (make-array (list +taille+ +taille+))))
      (dotimes (a +taille+ grille)
	(dotimes (b +taille+ t)
	  (setf (aref grille a b) (make-case))
	  )
	)
      )
    )
  )

;;**Chargement d'une grille de sudoku**;;
(defun load-grille (notre-grille grille)
  (let ((cur-val (aref grille 0 0)))
    
    (dotimes (u +taille+ t)
      (dotimes (v +taille+ t)
	(setq cur-val (aref grille u v))
	(unless (zerop cur-val)
	  (modif-valeur notre-grille cur-val u v)
	  (modif-valeur-init notre-grille cur-val u v)
	  (clean-possibilite notre-grille cur-val u v)
	  )
	)
      )
    ;;cela va dépendre de comment sont données les grilles si c'est des tableaux a deux dimensions, une liste a une dimension.... A vérifier pour chaque vlaeur différente de 0 on les enlève des possibilité
    )
  )

;;**Solveur**;;
;;Tant que l'on a pas fini (ou qu'il n'y a pas eu de modif durant un tour complet
;;Pour toutes les cases
;;Si la vlauer de départ vaut 0 et le nombre de possibilité vaut 1
;;On met la possibilité dans la valeur actuelle
;;on vide la liste de possibilité
;;on 
(defun solver (grille &key (once 0))
  (let* ((c (aref grille 0 0))
	 (fini 0))
    (loop while (zerop fini) do 
	 (setq fini 1)
	 (dotimes (i +taille+ t)
	   (dotimes (j +taille+ t)
	     (setq c (aref grille i j))
	     (when (and (zerop (val-init-of c)) (= 1 (nb-pos-of c)))
	       ;;Une seule possibilité et on ne prend pas en compte les modifs du joueur
	       (setf (val-actu-of c) (first (possibilite-of c)))
	       (clean-possibilite grille (val-actu-of c) i j)
	       (unless (= 1 once) (setq fini 0))
	       )
	     )
	   )
	 )
    )
  )
;;;**********************************************;;;
;;;***************FONCTIONS FINALE***************;;;
;;;**********************************************;;;
;;**Affiche la grille**;;
(defun print-grille (grille) ;(grille)
  ;;acces à une case : c = (aref grille ligne colonne)
  ;;acces à la valeur actuelle valeur = (val-actu-of c)
  ;;En une suele ligne (val-actu-of (aref grille lig col))
  
  (line-star)
  (format t "~C" #\linefeed)
  (dotimes (lig (* +n+ +n+) t)
    (line-number grille lig)
    (cond
      ((= lig 8) (line-star))
      ((= (1- (* 2 +n+)) lig)(line-star ))
      ((= (1- +n+) lig)(line-star ))
      (t (line-normal ))
      )
    (format t "~C" #\linefeed)
    )
  )

(defun line-number (grille lig)
  (let ((tmp (val-actu-of (aref grille lig 0))))
    (dotimes (col (* +n+ +n+) t)
      (setf tmp (val-actu-of (aref grille lig col)))
      (if (zerop tmp)
	  (cond
	    ((zerop col) (format t "***     |" ))
	    ((= (1- +n+) col) (format t "     ***" ))
	    ((= col (1- (* 2 +n+))) (format t "     ***"))
	    ((= col (1- (* +n+ +n+)))(format t "     ***" ))
	    (t (format t "     |" ))
	    )
	  (cond
	    ((zerop col)(format t "***  ~D  |"tmp))
	    ((= (1- +n+) col)(format t "  ~D  ***"tmp))
	    ((= col (1- (* 2 +n+)))(format t "  ~D  ***"tmp))
	    ((= col (1- (* +n+ +n+)))(format t "  ~D  ***"tmp))
	    (t (format t "  ~D  |"tmp)))
	  )
      )
    )
  (format t "~C" #\linefeed)
  )

(defun line-star ()
  (dotimes (i (* +n+ +n+) t)
    (format t "*******"))
  )
(defun line-normal()
  (dotimes (i (* +n+ +n+) t)
    (cond
      ((zerop i)(format t "***-----"))
      ((= +n+ i)(format t "***-----"))
      ((= i (* 2 +n+))(format t "***-----"))
      ((= i (1- (* +n+ +n+)))(format t "*-----***"))
      (t (format t "*-----") )))
  )

;;**Modifie une valeur actuelle à voir avec l'api**;;
(defun modif-valeur (grille nombre lig col)
  (setf (val-actu-of (aref grille lig col)) nombre)
  )
(defun modif-valeur-init (grille nombre lig col)
  (setf (val-init-of (aref grille lig col)) nombre)
  (clear-possibilite (aref grille lig col))
  )
;;**Sudoku créé la grille, charge la grille donnée et initialise les possibilités**;;
(defun sudoku (grille)
  (let* ((taille (array-dimension grille 0))
	 (notre-grille (make-grille :taille taille)))
    (unless (= 9 taille)
      (setq +n+ (isqrt taille))
      (setq +taille+ taille)
      )
    (load-grille notre-grille grille)
    )
  )

;;**Solve-grille lance la fonction solver sur la fonction sudoku à utiliser pour tester les grilles prédéfinis sans possibilité de jouer**;;
(defun solve-grille (grille)
  (solver (sudoku grille))
  )
