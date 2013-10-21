Pour lancer des simulations, faire:


./lp.sh $1 $2 $3 $4 $5 $6 $7

où vous remplacez
$1 par rmax (la taille maximale d'une requète)
$2 par tree_type (pour l'instant 0 pour un arbre quelconque)
$3 par size_of_tree (le nombre de noed de l'arbre)
$4 par number_of_speeds (le nombre de vitesses, compter la vitesse 0)
$5 par max_speed (la vitesse maximale)
$6 par regularity_speeds (1 pour des vitesses régulièrement espacées, 0 pour des vitesses aléatoires)
$7 par number_of_tests (le nombre de tests à lancer)

exemple de confs: 
./lp.sh 100 0 20 4 150 1 100
./lp.sh 1000 0 21 4 1500 1 50
