<h2><details open>

<summary><b>Introduction</b></summary>
 
  <h4>
    <div STYLE="text-align:justify;"> Bienvenue sur l’outil ModEM-ACV qui vous permettra de réaliser l’évaluation environnementale de vos parcs de véhicules personnels en combinant le modèle d’émissions et de consommation Copert5 et l’Analyse de Cycle de Vie avec les données d’EcoInvent. L’application ModEm-ACV est structurée en trois étapes successives : le <b>Configurateur</b>, le <b>lanceur de calcul</b> et les <b>analyses</b>. Chacune des trois phases de ModEm-ACV sont représentées par un onglet. A l’ouverture de l’application, aucuns résultats sont disponibles et aucunes analyses ne peut être réalisées.<br> <br>
    Au sein de chaque onglet peut apparaitre une liste de sous items sur la partie gauche de l’écran, correspondant à des familles, soit de paramètres ou d’analyses. La partie centrale de l’écran content les éléments permettant le paramétrage, le lancement des calculs ou les analyses. Afin de facilité la visualisation, certains éléments sont contenus dans des panneaux d’affichage rétractables. Les signes + ou – dans le coin supérieur droit permet de les déplier ou de les replier.<br><br>
    Pour l’ensemble des onglets, les tableaux et figures produits par l’applications peuvent être sauvés ou copiés. Les figures sont éditées par la librairie graphique Plotly et peuvent être enregistrées sur votre disque dur en utilisant le pictogramme <img src="save_image.png" alt="drawing" style="height:60px;"/>, disposé sur la partie supérieure droite des graphiques. <br>
    Pour les tableaux, soit une option d’enregistrement des valeurs est disponible sous le tableau … sinon la sélection puis le copiage et le collage permettent de récupérer facilement les valeurs contenues dans les tableaux pour les reporter dans un tableur.<br> <br> </div>

  </h4>

</details></h2>
  
<h2><details close>

<summary><b><i class="fas fa-sliders-h"></i> Configurateur</b></summary>
 
 <h3 style="margin-left:2.5em"><details close>

<summary><b>Générale : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Cet onglet offre la possibilité de modifier un nombre conséquent de paramètres influant sur le résultats d’évaluation environnementale. Ces paramètres sont modifiables à travers les différents tableaux éditables qui permettent d’entrer des valeurs numériques souhaitées directement dans les cellules appropriées. Attention, dans certains tableaux les sommes des colonnes doivent être égales à 100%, une message d’erreur apparait pour vous avertir de cette situation qui bloque la validation de votre paramétrage.<br> <br>
    Les paramètres sont répartis en 5 catégories qui constitue 5 des 7 menus de gauches : <b>Composition du parc</b>, <b>Paramètres carburants</b>, <b>Poids et équipements</b>, <b>Usage des infrastructures</b> et <b>Conditions météorologiques</b>. Les deux derniers menus <b>Exporter ma configuration</b> et <b>Charger ma configuration</b> permettent de sauvegarder ou de charger des fichiers de configuration vers et depuis votre disque dur.<br><br>
    Afin que l’application prenne en considération vos nouvelles valeurs, il est nécessaire de valider votre entrée en utilisation les différents boutons … Si vous souhaitez annuler les modifications réalisées et revenir aux valeurs par défaut, le bouton … est disponible. L’application ModEm-ACV présente pour chaque paramètre une valeur par défaut correspondant au contexte français. Il n’est donc pas nécessaire d’entrer toutes les valeurs. <br> <br> </div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-car"></i> Composition du parc : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Trois options sont proposées pour paramétrer le parc de véhicules à évaluer : <em>utiliser un parc existant</em>, <em>personnaliser un parc</em> ou <em>importer un parc</em> depuis votre ordinateur. En choisissant d’utiliser un parc existant vous pouvez ensuite choisir dans le menu déroulant un des parcs déjà implémentés dans l’outil ModEm-ACV.<br> <br>
    En sélectionnant <em>personnaliser un parc</em>, vous avez accès au panneau de personnalisation du parc, qui vous permet de faire varié le type de motorisation, les normes Euro et les segments de puissance du parc. Chacune de ces trois variables dispose d’un tableau éditable pour entrer vos valeurs. Afin de d’affiner la composition de votre parc les variables peuvent être croisées entre elles, par exemple en exprimant les part de chaque énergie de motorisation en fonction des normes Euro ou des segments de puissance. Si certaines combinaisons sont redondantes ou impossible une message d’erreur sera affiché.<br><br>
    Un panneau de synthèse permet de visualiser l’état du parc de véhicules que vous allez évaluer. Il présente tout d’abord la répartition du parc par énergie de motorisation, norme Euro et par segments de puissance. L’année moyenne d’immatriculation et la puissance moyenne en chevaux fiscaux sont aussi estimées et affichées dans ce panneau. Pour compléter la visualisation du parc un graphique à barre interactif permet d’afficher une des variables en fonction d’une autre, par exemple la répartition des énergies en fonction des normes Euro. <br> <br> 
    Si vous avez paramétré votre propre parc, vous avez la possibilité d’exporter la description de ce parc en fichier Excel à travers l’option d’exportation situé à la fin du panneau de synthèse. Ce fichier Excel pourra être importer dans l’outil ModEm-ACV lors d’une future visite. <br> <br> 
    La troisième option pour définir le parc à évaluer est d’importer un fichier Excel contenant la composition de votre parc de véhicule. Ce fichier peut provenir d’une exportation précédente ou être construit de façon ad-hoc. Pour cela le format du fichier Excel à utiliser est disponible au téléchargement dans cette onglet via le bouton Télécharger la structure Excel. Si le format du fichier Excel est incorrect une erreur vous sera renvoyée.<br><br></div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-gas-pump"></i> Paramètres carburants : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Dans le panneau de synthèse, les deux graphiques camemberts permettent de représenter le mix électrique utilisé pour la propulsion des véhicules électriques et hybrides. Un zoom est présent pour les énergies renouvelables. Le graphique de droite présente les taux d’incorporation d’agro-carburant dans les carburants, il s’agit de taux exprimés en volume.<br> <br>
    L’incorporation peut être modifié à travers le panneau <em>Incorporation d’agro-carburants</em> dans lequel figure le tableau des agro-carburants associé aux valeurs de surconsommation, de taux d’oxygène dans l’essence et du taux total d’éthanol dans l’essence.<br><br>
    Le mix électrique est modifiable dans le panneau <em>Production électrique</em>, où figure le tableau des principales sources de production d’électricité. Un second tableau permet d’entrée les valeurs de perte énergétiques entre les différents réseaux de tension électrique.<br> <br> 
    Les facteurs de consommations d’électricité dans les véhicules électriques sont paramétrables dans le panneau <em>Consommation des véhicules</em> électriques. La valeur de consommation en kWh/100km est modifiable ainsi que le taux d’usage des véhicules hybrides en mode électrique.<br><br></div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-weight"></i> Poids et équipements : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">La masse des véhicules et des principaux blocs des véhicules sont décrits et paramétrables dans le panneau <em>Masses des composants</em>. Le véhicule se réparti alors en trois blocs, le (ou les) moteur(s) thermique et électrique, qui représente(nt) l’ensemble de la chaine de traction du véhicule, la batterie, pour les véhicules électriques et hybrides et un bloc nommé carrosserie qui regroupe le reste des éléments de la voiture (roues, vitres, sièges …). Les masse sont paramétrable pour chaque énergie de motorisation en sélectionnant l’énergie en question et par segment de puissance pour chacune des lignes du tableau. Pour changer la masse totale du véhicule, il est nécessaire de changer la masse des différents blocs qui le composent.<br> <br>
    La durée de vie du véhicule et de la batterie sont des paramètres importants dans une ACV. Le panneau <em>Durée de vie</em> des véhicules permet de paramétrer ces durées de vie, par type d’énergie de voiture pour la durée de vie totale du véhicule. Les durées de vie des véhicules et des batteries sont exprimées en kilomètre.<br><br>
    La présence de la climatisation et l’usage moyen de cette dernière sont des coefficients modifiables dans le panneau <em>Climatisation</em>. Le taux d’installation d’un système de climatisation et son taux d’usage sont constants en fonction de l’énergie et de la puissance du véhicule, en revanche il est possible de faire évoluer ces taux en fonction de l’âge, et donc des normes euro, des véhicules. <br><br></div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-road"></i> Usage des infrastructures : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Afin de prendre en compte les conditions de circulation de la flotte évaluer et d’estimer au mieux les impacts associés aux infrastructures routière, il est préférable de spécifier le type d’infrastructures utilisées. Quatre archétypes de route sont représentés : Autoroute, Route primaire (routes nationales et grande départementales), Route secondaire (routes départementales) et Route tertiaire communales). <br> <br>
    En complément de la répartition par grands archétypes, chaque archétype peut être spécifié afin de préciser certains éléments (nb de voies, proportion de tunnel et de pont) et de choisir comment se réparti l’impact total en fonction des flux de véhicules. Un flux important de véhicule induit moins d’impacts par véhicule et réciproquement. <br><br></div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-sun"></i> Conditions météorologiques : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Les conditions météorologiques induisent des variations en termes d’émissions et de consommation d’énergie. L’utilisation de la climatisation varie aussi en fonction de la température et de l’humidité. Le graphique de cette page résume les conditions météorologiques mensuelles auxquelles est soumis le parc évalué. <br> <br>
    A droite du graphique figure le pourcentage des trajets réalisé en voiture thermique avec un moteur considéré comme froid qui induit des surémissions et des surconsommations. Ce pourcentage dépend de la température mais aussi de la taille des déplacements, c’est pour cela que la taille moyenne d’un déplacement peut être modifiée. Plus le déplacement sera court, plus la part du déplacement avec un moteur froid sera important. <br><br>
    L’ensemble des températures et des taux d’humidité par mois sont modifiables dans le tableau du panneau <em>Températures et humidités</em> relatives.<br><br></div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-file-export"></i> Exporter ma configuration : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Afin de garder une trace des paramètres utilisés pour l’évaluation environnementale, il est possible, avec la page <em>Exporter ma configuration</em>, de sauvegarder l’intégralité du jeu de paramètres. <br> <br>
    Le premier panneau de cette page permet donc de télécharger un fichier Excel contenant dans plusieurs feuilles l’ensemble des paramètres. <br><br>
    Le deuxième panneau permet de parcourir les tableaux brutes contenant les paramètres et la composition du parc évalué.<br><br></div>

  </h4>

</details></h3>

<h3 style="margin-left:2.5em"><details close>

<summary><b><i class="fas fa-file-upload"></i> Charger ma configuration : </b></summary>
 
  <h4>
    <div STYLE="text-align:justify;">Dans le cas où vous souhaitez directement importer une configuration précédemment exportée ou modifié de façon ad-hoc, la page <em>Charger ma configuration</em> permet d’importer l’ensemble des éléments nécessaire aux calculs. <br> <br>
    Pour importer correctement une configuration, vous devez respecter le format de données pour qu'il corresponde au format du fichier Excel d'exportation (voir  menu <em>Exporter ma configuration</em>). Il vous reste ensuite à selectionner votre fichier, grâce à l'explorateur de documents, d'entrer le nom associé à votre configuration et de cliquer sur <b>Importer</b>. Tous vos paramètres de votre fichier remplaceront ceux du configurateur, dans tous les menus. De plus, un nouveau parc de véhicule sera disponible dans la liste des parcs du menu <em>Composition du parc</em><br><br></div>

  </h4>

</details></h3>

</details></h2>

<h2><details close>

<summary><b><i class="fas fa-calculator"></i> Lancer un calcul</b></summary>

  <h4>
    <div STYLE="text-align:justify;">Cette page vous permet de lancer les calculs d'impacts environnementaux du parc de véhicules que vous avez configuré. Le bouton <em>Calcul des impacts environnementaux</em> exécute le programme de calcul qui prend entre 10 et 30 secondes pour s'exécuter. La durée de calcul peut être dans le cas où le serveur soit particulièrement utilisé. <br> <br>
    La partie gauche de cette page liste l'ensemble des indicateurs disponibles dans le calculateur, tous ces indicateurs peuvent être sélectionnés simultanément. Il est aussi possible de réduire cette liste en utilisant le menu déroulant situé au-dessus du tableau d'indicateurs. <br><br>
    Avant de lancer un calcul il est possible d'inclure ou d'exclure l'analyse des émissions de polluants directs et des consommation d'énergies. Pour cela un menu déroulant à gauche offre la possibilité de paramétrer cette option<br><br>
    En fonction du but recherché, il est possible de définir le niveau de détail de l'ACV, afin de réaliser une analyse de contribution un peu plus fine en considerant notamment le role des composants (moteurs, batterie, type d'infrastructures). Un bouton dédié est présent pour cela, ne analyser l'ACV en détail rend le calcul plus rapide <br><br>
    Avec ce calculateur, vous avez la possibilité d'évaluer plusieurs parcs de véhicules et plusieurs configurations. Pour cela il vous suffit de spécifié pour chaque calcul le nom de votre configuraion. Une liste de l'ensemble de vos calculs apparait ensuite en dessous du bouton d'exécution. Dans le cas où vous souhaitez supprimer un calcul précédent, vous pouvez inscrire le nom du calcul à supprimer dans la cellule dédié et cliquer sur <em>Supprimer ce calcul</em>. Si vous avez réalisé plusieurs calculs avec le calculateur, vous aurez la possbilité lors de l'analyse de comparer vos différentes configuration. <br><br></div>

  </h4>
</details></h2>

<h2><details close>

<summary><b><i class="fas fa-chart-bar"></i> Analyses</b></summary>

  <h4>
    <div STYLE="text-align:justify;">Dans cet onglet, vous avez accès à un large panel d'analyse qui sont classées en quatre catégories figurant sur le menu à gauche : la <b>Synthèse</b>, les analyses <b>Monocritères</b>, les analyses <b>Multicrtères</b> et l'analyse des <b>Emissions directes et consommations</b>. Certaines catégories regroupent plusieurs types d'analyse disponible à partir du menu.<br> <br>
    L'ensemble des résultats d'analyse sont exportable sous forme de tableau grâce aux boutons <em>Télécharger les données</em> disposés en bas des panneaux d'analyse. Vous téléchargerez ainsi les données affichées sous format .csv, directement exploitable avec un tableur. Les figures peuvent aussi être sauvegardées en .png diretement grâce à l'option <em> download plot as png</em> dans l'angle supérieur droit des figures.<br><br>
    l'ensemble des pages d'analyse disposent d'un menu déroulant orange vous permettant de sélectionner le calcu que vous souhaitez analyser. Dans le cas où vous comparer plusieurs parcs et calculs, ce menu déroulant vous donne la possibilité de sélectionner les différentes options à comparer. <br><br>
    Les menus déroulants bleus permettent dans certaines pages de sélectionner soit l'indicateur à analyser, dans le cas d'une analyse monocritère, soit de sélectionner la liste d'indicateurs à afficher, dans le cas d'une analyse multicritère.<br><br>
    Pour certaines analyses, il est possible de modifier la vitesse de circulation grâce à des boutons bleus. La vitesse <em>Réseau</em> correspond à la vitesse moyenne de circulation de votre parc en fonction de l'usage des infrastructures configurer. Les vitesses <em>Lente, Moyenne, Rapide</em> et <em> Très Rapide</em> correspondent respectivement à chaque archétype d'infrastructure du calculateur, c'est-à-dire : route tertiaire, route secondaire, route primaire et autoroute. Les vitesses sur ces infrastructures sont liées aux phases du cycle WLTC3b. Enfin, vous avez la possibilité de rentrer manuellement la vitesse que vous souhaitez analyser en selectionnant <em>Personnaliser</em> parmi la liste des boutons et d'entrer la valeur en km/h voulue. <br><br>
    Enfin, les menus déroulants verts servent à modifier les paramètres de discritisation des données, afin d'analyser par exemple les impacts par norme euro et par type d'énergie plutôt que simplement par énergie. De plus, dans des analyses de contribution ces menus servent à spécifier la clé de répartision des impacts, par exemple par phase de cycle de vie ou par segments de puissance. <br><br>
    Les graphiques générés pour les analyses sont issue de la librairie graphique <a href="https://plotly.com/r/">Plotly</a> qui permet de réaliser des zooms sur des parties des graphiques et de visualiser les valeurs directement sur les graphiques en survolant les graphiques de la souris. Les options dans le coin supérieure droit des figures permettent de modifier certains aspects et de réinitiliser la figure. <br><br></div>

  </h4>


</details></h2>

<h2><details close>

<summary><b><i class="fas fa-info-circle"></i> Remonter un problème</b></summary>

  <h4>
    <div STYLE="text-align:justify;">Dans le cas où vous auriez repéré une erreur dans l'application que vous souhaitez soumettre afin de l'intégrer dans les versions futures, nous vous invitons soit à nous écrire directement par <a href="mailto:cyrille.francois@enpc.fr">Email</a>, en précisant dans votre objet qu'il s'agit d'un retour lié à ModEm-ACV. Soit en créant un nouveau sujet de problème dans le <a href="https://github.com/Ifsttar/ModEm-ACV/issues">Github</a> de l'application.  <br><br></div>

  </h4>
</details></h2>