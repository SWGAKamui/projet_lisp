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
    (setf (possibilite-of c) (delete nombre (possibilite-of c)))
    )
  )

;;**Supprimer toutes les possibilités d'une case**;;
(defmethod clear-possibilite ((c class_case))
  (setf (possibilite-of c) nil)
  (setf (nb-pos-of c) 0)
  )

;;**Supprimer un nombre sur toute la ligne colonne et groupe de case.**;;
(defun clean-possibilite (grille nombre lig col)
  (let* ((taille (array-dimension grille 0))
	 (facteur (isqrt taille))
	 (modl (* (floor lig facteur) facteur))
	 (modc (* (floor col facteur) facteur))
	 )
    (dotimes (l taille t)
      (rem-possibilite nombre (aref grille lig l))
      (rem-possibilite nombre (aref grille l col))
      )
    (dotimes (bl (1- facteur) t)
      (dotimes (bc (1- facteur) t)
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

;;**Chargement d'une grille de sudoku**;;
(defun load-grille (grille)
  (let* ((taille (array-dimension grille 0))
	 (notre-grille (make-grille :taille taille)))
    (dotimes (u taille t)
      (dotimes (v taille t)
	;;unless (zerop grille[u][v])
	;;notre-grille[u][v]=grille[u][v]
	;;cleam-possibilite notre-grille grille[u][v] u v	 
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
  (let* ((taille (array-dimension grille 0))
	 (c (aref grille 0 0))
	 (fini 0))
    (loop while (zerop fini) do 
	 (setq fini 1)
	 (dotimes (i taille t)
	   (dotimes (j taille t)
	     (setq c (aref grille i j))
	     (when (and (zerop (val-init-of c)) (= 1 (nb-pos-of c)))
	       ;;Une seule possibilité et on ne prend pas en compte les modifs du joueur
	       (setf (val-actu-of c) (first (possibilite-of c)))
	       (clear-possibilite c)
	       (if (= 1 once) (setq fini 0))
	       (clean-possibilite grille (val-actu-of c) i j)
	       )
	     )
	   )
	 )
    )
  )

(defparameter n 3)


;;;**********************************************;;;
;;;***************FONCTIONS FINALE***************;;;
;;;**********************************************;;;
;;**Affiche la grille**;;
(defun print-grille (n) ;(grille)
  ;;acces à une case : c = (aref grille ligne colonne)
  ;;acces à la valeur actuelle valeur = (val-actu-of c)
  ;;En une suele ligne (val-actu-of (aref grille lig col))
        (line-star n)
	(format t "~C" #\linefeed)
  (dotimes (i (* n n) (1+ i))
    (line-number n)   
    (if (= i 8)
	(line-star n)
	(if (= (1- (* 2 n)) i)
	    (line-star n)
	    (if (= (1- n) i)
		(line-star n)
		(line-normal n))))
    (format t "~C" #\linefeed))
  t)
(defun line-number (n)
  (dotimes (i (* n n) (1+ i))
    (if (zerop i)
	(format t "***  ~D  |"0)
	(if (= (1- n) i)
	    (format t "  ~D  ***"0 )
	    (if (= i (1- (* 2 n)))
		(format t "  ~D  ***"0 )
		(if (= i (1- (* n n)))
		    (format t "  ~D  ***"0 )
		    (format t "  ~D  |"0 ))))))
  (format t "~C" #\linefeed))
(defun line-star (n)
       (dotimes (i (* n n) (1+ i))
	 (format t "*******")))
(defun line-normal(n)
  (dotimes (i (* n n) (1+ i))
    (if (zerop i)
	 (format t "***-----")
	(if (= n i)
	    (format t "***-----")
	    (if (= i (* 2 n))
		(format t "***-----")
		(if (= i (1- (* n n)))
		    (format t "*-----***")
		    (format t "*-----") ))))))

;;**Modifie une valeur actuelle à voir avec l'api**;;
(defun modif-valeur (grille nombre lig col)
  (setf (val-actu-of (aref grille lig col)) nombre)
  )
;;**Sudoku créé la grille, charge la grille donnée et initialise les possibilités**;;
(defun sudoku (grille)
  (let* ((taille (array-dimension grille 0))
	 (notre-grille (make-grille :taille taille)))
    (load-grille notre-grille)
    )
  )

;;**Solve-grille lance la fonction solver sur la fonction sudoku à utiliser pour tester les grilles prédéfinis sans possibilité de jouer**;;
(defun solve-grille (grille)
  (solver (sudoku grille))
  )
