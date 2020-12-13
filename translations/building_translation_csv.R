translation_fr <- 
  data.frame(en = "Biodiversity", fr = "Biodiversité") %>% 
  # Menu
  # add_row(en = "Biodiversity", fr = "Biodiversité") %>% 
  add_row(en = "Add title as titletextSus_UI(title = 'my title')", 
          fr = "Ajouter le titre en tant que titletextSus_UI (title = 'mon titre')") %>%
  add_row(en = "Add text as titletextSus_UI(textAboveSplit = 'short description')", 
          fr = "Ajoutez du texte comme titletextSus_UI (textAboveSplit = 'short description'") %>%
  add_row(en = "Add text as titletextSus_UI(textBelowSplit = 'long description')", 
          fr = "Ajouter du texte comme titletextSus_UI (textBelowSplit = 'longue description')") %>%
  add_row(en = "Learn more", fr = "En savoir plus") %>%
  add_row(en = "Compare", fr = "Comparez") %>%
  add_row(en = "SUS Preview", fr = "Aperçu du SUS") %>%
  # title_text.csv
  add_row(en = paste0("The CanALE dataset (developed by Prof. Nancy Ross and her team) captures ", 
                      "four key elements related to active living environments: population density, ",
                      "points of interest, street grid, and proximity of transit service."), 
          fr = paste0("La base de données AVA-Can (développée par la professeure Nancy Ross et son équipe) ",
                      "saisit quatre éléments clés liés aux milieux de vie actifs: la densité de population, ",
                      "les points d'intérêt, la grille de rues et la proximité du service de transport en commun.")) %>%
  add_row(en = paste0("<p>A safe and inviting pedestrian realm is not distributed equally across socio-demographic ",
                      "factors. The risks of pedestrian injuries and fatalities are higher in low-income and ",
                      "racialized communities where residents often rely on walking as a daily mode of transport but ",
                      "where the local environment is not necessarily inviting and safe.<p>In addition to evidence ",
                      "pointing towards large discrepancies in the provision of walkable urban space across income ",
                      "and racial lines, concern has been raised with regard to the possible gentrification and displacement ",
                      "impacts of improved pedestrian infrastructure. In other words, who can afford to live in walkable ",
                      "neighbourhoods?<br><p>Further resources:<ul><li><a href= ''>Thomas Herrmann, William Gleckner, ",
                      "Rania A. Wasfi, Benoît Thierry, Yan Kestens and Nancy A. Ross. 2019. 'A pan-Canadian measure of ",
                      "active living environments using open data. Statistics Canada Health Reports, ",
                      "82-003-X.</a><li><a href = ''>Kevin Manaugh, Linnea Soli, Samuel Kohn, Robin Basalaev-Binder, ",
                      "Ty Tuff, David Wachsmuth. 2020. 'Montreal’s response to COVID-19: An equity analysis of new active ",
                      "transport infrastructure.' Transportation Research Board working paper.</a> <b>(MSSI research)</b></ul><br><p><i>",
                      "Module lead authors: Robin Basalaev-Binder, Ty Tuff, David Wachsmuth</i>"), 
          fr = paste0("<p>Une voie piétonne sécuritaire et accueillante n'est pas répartie de manière égale entre les ",
                      "facteurs sociodémographiques. Les risques de blessures et de décès de piétons sont plus élevés ",
                      "dans les communautés racialisées et à faible revenu, où les résidents comptent souvent sur la ",
                      "marche comme mode de transport quotidien, mais où l'environnement local n'est pas nécessairement ",
                      "accueillant et sécuritaire.<p> Outre les éléments indiquant de grandes disparités dans la provision ",
                      "d'un espace piétonnier urbain selon le revenu et le pourcentage de minorités visibles, des préoccupations ont été soulevées ",
                      "concernant les éventuels impacts de l'amélioration des infrastructures piétonnes sur la gentrification ",
                      "et le déplacement. En d'autres termes, qui peut se permettre de vivre dans des quartiers piétonniers? ",
                      "<br><p>Ressources supplémentaires:<ul><li><a href= ''>Thomas Herrmann, William Gleckner, Rania A. Wasfi, ",
                      "Benoît Thierry, Yan Kestens et Nancy A. Ross. 2019. Une mesure pancanadienne fondées sur les données ouvertes de ",
                      "l'accessibilité à la vie active dans les milieux de vie. Rapports sur la santé de Statistique Canada, 82-003-X.</a><li><a ",
                      "href = ''>Kevin Manaugh, Linnea Soli, Samuel Kohn, Robin Basalaev-Binder, Ty Tuff, David Wachsmuth. ",
                      "2020. «Réponse de Montréal à COVID-19 : Une analyse de l'équité des nouvelles infrastructures de ",
                      "transport actif». Document de travail pour le Transportation Research Board.</a> <b>(MSSI research)</b></ul><br><<p><<i>Auteur(e)s ",
                      "principaux du module: Robin Basalaev-Binder, Ty Tuff, David Wachsmuth</i>")) %>%
  add_row(en = paste0("Based on 2016 commuting data, we quantify possible reductions in VMT and GHG by identifying car ",
                      "trips that could be shifted to cycling based on distance, elevation change and other factors."), 
          fr = paste0("Basé sur les données relatives aux déplacements domicile-travail en 2016, nous quantifions ",
                      "les réductions possibles de KPV et de GES en identifiant les déplacements en voiture qui pourraient ",
                      "être remplacés par des déplacement en vélo en fonction de la distance, du changement ",
                      "d'altitude et d'autres facteurs.")) %>%
  add_row(en = paste0("<p>The transportation sector is a major contributor to Canada's greenhouse gas (GHG) emissions which ",
                      "are linked to global climate change. Reducing vehicle miles traveled (VMT) over the long term is ",
                      "increasingly recognized as the key to reduce GHG emissions from the transportation sector but has not ",
                      "received as much attention as needed.<p>The primary objective of this study is to investigate the ",
                      "potential for reducing VMT and GHG emissions by shifting short car trips to cycling in Montreal. ",
                      "Based on commuting data from the 2016 Canadian Census, commuting patterns were explored. Two scenarios ",
                      "were introduced to model environmental effects of a modal shift towards cycling based on characteristics ",
                      "of current bicycle trips.<p>The results showed that enhanced cycling commuting can reduce VMT and GHG ",
                      "emissions from car travel. Other mitigation measures are necessary for achieving GHG emissions reduction ",
                      "targets.<br><p><i>Module lead authors: Qiao Zhao, Kevin Manaugh</i>"), 
          fr = paste0("<p>Le secteur des transports est un contributeur majeur aux émissions de gaz à effet de serre (GES) ",
                      "du Canada, qui sont liées au changement climatique mondial. La réduction des kilomètres parcourus ",
                      "par les véhicules (KPV) sur le long terme est de plus en plus reconnue comme la clé de la réduction ",
                      "des émissions de GES du secteur des transports, mais n'a pas reçu autant d'attention que nécessaire.<p>Le ",
                      "principal objectif de cette étude est d'étudier le potentiel de réduction des KPV et des émissions de ",
                      "GES en déplaçant les courts trajets en voiture par le vélo à Montréal. Deux scénarios ont été introduits pour modéliser ",
                      "les effets environnementaux d'un transfert modal vers le vélo en fonction des caractéristiques des ",
                      "déplacements actuels à vélo.<p>Les résultats ont montré que l'augmentation des déplacements à vélo peut ",
                      "réduire les KPV et les émissions de GES des déplacements en voiture. D'autres mesures de réduction ",
                      "sont nécessaires pour atteindre les objectifs de diminution des émissions de GES.<br><p><i>Auteurs ",
                      "principaux du module: Qiao Zhao, Kevin Manaugh</i>")) %>%
  add_row(en = paste0("The capacity for pedestrian social distancing is a capacity measurement that determines the percentage ",
                      "of a neighbourhood’s population that can make local trips on foot at the same time while respecting ",
                      "‘social distancing’ regulations."), # I don't believe we need to put social distancing in quotes
          fr = paste0("La capacité de distanciation sociale des piétons est une mesure de capacité qui détermine le pourcentage ",
                      "de la population d'un quartier qui peut faire des trajets locaux à pied en même temps, tout en respectant ",
                      "les règles de «distanciation sociale».")) %>%
  add_row(en = paste0("<p>Using open data from Montreal's open data portal as well as OpenStreetMap, it was possible to calculate ",
                      "the total surface area of sidewalks, neighbourhood parks, and pre-Covid pedestrian streets. Summing these ",
                      "surface areas gets us the neighbourhood's total walkable surface area. It is then possible to calculate how ",
                      "many residents can ‘fit’ into the pedestrian realm while respecting ‘social distancing’ regulations of 2 meters ",
                      "(total walkable surface area divided by the surface area of a circle with a 2-meter radius, that is 12.54 ",
                      "square meters). Finally, we normalize the value by representing it as a percentage of the residential population. ",
                      "Neighbourhoods where less than 100% of the local population can make trips on foot at the same time are above ",
                      "capacity and more at risk of overcrowding from local pedestrian trips. <p>While it is still important to ",
                      "take into account pedestrian flows coming from external neighbourhoods (some data on pedestrian flows obtained ",
                      "from 2016 TrajetMtl data is presented in this research), the measurement’s focus on local pedestrian capacity ",
                      "is especially relevant during a pandemic situation where shelter in place and travel restrictions have ",
                      "generally led to a rise in local trips and a decline in trips from other neighbourhoods.<br><p><i>Module ",
                      "lead author: Samuel Kohn</i>"), 
          fr = paste0("<p>En utilisant les données du portail de données ouvertes de Montréal ainsi que OpenStreetMap, il a été ",
                      "possible de calculer la surface totale des trottoirs, des parcs de quartier et des rues piétonnes pré-Covid-19. ",
                      "En additionnant ces surfaces, on obtient la surface totale du quartier où l'on peut marcher. Il est alors ",
                      "possible de calculer combien d'habitants peuvent «s'intégrer» dans la zone piétonne tout en respectant ",
                      "les règles de «distanciation sociale» de 2 mètres (surface totale piétonne divisée par la surface d'un ",
                      "cercle de 2 mètres de rayon, soit 12,54 mètres carrés). Enfin, nous normalisons la valeur en la représentant ",
                      "en pourcentage de la population résidentielle. Les quartiers où moins de 100 % de la population locale peut ",
                      "effectuer des trajets à pied en même temps sont au-dessus de leur capacité et sont plus exposés au risque de ",
                      "surpeuplement des trajets locaux à pied. <p>Même s'il est toujours important de prendre en compte les flux ",
                      "de piétons provenant des quartiers extérieurs (certaines données sur les flux de piétons obtenues à partir ",
                      "des données de 2016 de TrajetMtl sont présentées dans cette recherche), l'accent mis par la mesure sur la ",
                      "capacité piétonne locale est particulièrement pertinent en situation de pandémie, où les protections en place ",
                      "et les restrictions de déplacement ont généralement conduit à une augmentation des déplacements locaux et à ",
                      "une diminution des déplacements provenant d'autres quartiers.<br><p><i>Auteur principal du module: ",
                      "Samuel Kohn</i>")) %>%
  add_row(en = paste0("Compare the pedestrian capacity for social distancing metric across a variety of other variables, such ",
                      "as walkable access to amenities, income level, immigration, visible minorities, population density, etc."),
          fr = paste0("Comparez la mesure de la capacité des piétons à pratiquer la distance sociale en fonction de diverses ",
                      "autres variables, telles que l'accès à pid aux services de base, le revenu, l'immigration, les ",
                      "minorités visibles, la densité de population, etc.")) %>%
  add_row(en = paste0("The data shows us that the ability to safely navigate pedestrian space tends to be much lower in ",
                      "DAs with lower incomes as well as DAs with high proportions of visible minorities and immigrants ",
                      "compared to majority white regions of Montreal. While the City’s plans to increase walkable urban ",
                      "space made some improvements to these discrepancies, there is room for improvement. The interactive ",
                      "data within this platform has the potential to support policy-makers towards making strategic decisions ",
                      "with more equitable outcomes. Below is a policy analysis exemplar using two variables: capacity for pedestrian ",
                      "social distancing and walkable access to key amenities."),
          fr = paste0("Les données nous montrent que la capacité à se déplacer en toute sécurité dans l'espace piétonnier ",
                      "tend à être beaucoup plus faible dans les AD à faible revenu ainsi que dans les AD ayant une forte ",
                      "proportion de minorités visibles et d'immigrants par rapport aux régions de Montréal où la majorité ",
                      "des habitants sont blancs. Bien que les plans de la Ville visant à augmenter l'espace urbain piétonnier ",
                      "aient permis d'améliorer ces écarts, il y a place à amélioration. Les données interactives de cette ",
                      "plateforme peuvent aider les décideurs politiques à prendre des décisions stratégiques avec des résultats ",
                      "plus équitables. Vous trouverez ci-dessous un exemple d'analyse de politique utilisant deux variables: ",
                      "la capacité de distanciation sociale des piétons et l'accès à pied aux services de base.")) %>%
  add_row(en = paste0("In order to calculate the width of sidewalks in Montreal, we used a spatial dataset published by ",
                      "Montréal Open Data Portal that includes polygons of all sidewalks within the Montreal agglomeration. ",
                      "Using the the object-oriented programming language R, we developed a function which first creates ",
                      "negative buffers inside each sidewalk segment, and then iteratively adjusts the distance of that buffer ",
                      "until the maximum distance is achieved which still produces valid buffer geometry (if the buffer ",
                      "boundaries overlap, the geometry becomes invalid). The outcome is the equivalent of a centreline ",
                      "inside each sidewalk polygon. The last step to determine sidewalk width is to sum the distances ",
                      "between the centreline and both edges of a given sidewalk polygon segment. This process is ",
                      "illustrated below."),
          fr = paste0("Afin de calculer la largeur des trottoirs à Montréal, nous avons utilisé un ensemble de données ",
                      "spatiales publié par le Portail de données ouvertes de Montréal, qui comprend les polygones de ",
                      "tous les trottoirs de l'agglomération de Montréal. En utilisant le langage de programmation orienté ",
                      "objet R, nous avons développé une fonction qui crée d'abord des tampons négatifs à l'intérieur de ",
                      "chaque segment de trottoir, puis ajuste itérativement la distance de ce tampon jusqu'à ce que la ",
                      "distance maximale soit atteinte, ce qui produit encore une géométrie de tampon valide (si les ",
                      "limites du tampon se chevauchent, la géométrie devient invalide). Le résultat est l'équivalent ",
                      "d'une ligne centrale à l'intérieur de chaque polygone de trottoir. La dernière étape pour déterminer ",
                      "la largeur du trottoir consiste à additionner les distances entre la ligne centrale et les deux bords ",
                      "d'un segment donné de polygone de trottoir. Ce processus est illustré ci-dessous.")) %>%
  # Description
  add_row(en = paste0("The whole of an ecosystem is more than the sum of its parts. ",
                      "The health and resilience of our urban green spaces are determined ",
                      "by the quantity, quality, and composition of the species with cohabitat with."), 
          fr = paste0("L'ensemble d'un écosystème est plus que la somme de ses parties. ", 
                      "La santé et la résilience de nos espaces verts urbains sont déterminées ", 
                      "par la quantité, la qualité et la composition des espèces avec lesquelles nous cohabitons.")) %>%
  add_row(en = paste0("Montreal's biodiversity is the result of many competing factors..."), 
          fr = paste0("La biodiversité de Montréal est le résultat de nombreux facteurs concurrents...")) %>%
  add_row(en = "Hello Shiny!", fr = "French") %>%
  # Website descriptions
  add_row(en = paste0("Dashboards offer a tool for communicating sustainability data in a visually ", 
                      "based digital platform. We see a gap in current dashboards going beyond the ", 
                      "visualization of pre-existing data at static scales, leaving room for a more ", 
                      "future-oriented, scalable, and interactive model."), 
          fr = paste0("Les tableaux de bord offrent un outil pour communiquer des données sur ", 
                      "le développment durable dans une plate-forme numérique visuelle. Nous constatons ", 
                      "une lacune dans les tableaux de bord actuels allant au-delà de la visualisation ", 
                      "de données préexistantes à des échelles statiques, laissant la place à un modèle ", 
                      "plus orienté vers l'avenir, modulable et interactif.")) %>%
  add_row(en = paste0("Existing data-driven approaches to urban sustainability are characterized by static ", 
                      "data, limited user interaction, and the oversimplification of complex urban issues. ", 
                      "They provide little opportunity for user engagement and exploration of questions ", 
                      "connecting different data and issues."), 
          fr = paste0("Les approches actuelles fondées sur les données étudiant la durabilité urbaine ", 
                      "se caractérisent par des données statiques, une interaction limitée pour les utilisateurs ",  
                      "et une simplification excessive de problèmes urbains complexes. Elles offrent peu ", 
                      "d’opportunités pour l’engagement des utilisateurs et l’exploration de questions reliant ", 
                      "différentes données et problèmes.")) %>%
  add_row(en = paste0("Some of the limitations of existing dashboards include a bias towards quantifiable, ", 
                      "measurable components of sustainability, and a reliance on data with potential bias. ", 
                      "Furthermore, they often attempt to play the role of a neutral force to communicate ", 
                      "“objective” information on cities."), 
          fr = paste0("Parmi les limites des tableaux de bord existants, on peut citer la préférence ", 
                      "accordée aux éléments quantifiables et mesurables de la durabilité, et la dépendance ", 
                      "à l'égard de données potentiellement biaisées. En outre, ils tentent souvent de jouer ", 
                      "le rôle d'une force neutre pour communiquer des informations “objectives” sur les villes.")) %>%
  add_row(en = paste0("Sustainability dashboards should build upon best practices to provide useful tools for ", 
                      "individuals and cities alike to examine the many facets of urban sustainability and ", 
                      "question existing assumptions."), 
          fr = paste0("Les tableaux de bord sur la durabilité devraient s'appuyer sur les meilleures pratiques ", 
                      "afin de fournir des outils utiles aux individus comme aux villes pour examiner les ",
                      "nombreuses facettes de la durabilité urbaine et remettre en question les hypothèses ", 
                      "existantes.")) %>%
  add_row(en = paste0("Maintaining transparency with data and methodologies, ensuring public participation ",
                      "and accurate representation of underprivileged communities, and using engaging and ", 
                      "accessible tools contribute to the success of a dashboard."), 
          fr = paste0("Maintenir la transparence des données et des méthodologies, s'assurer de la participation ", 
                      "du public et de la représentation exacte des communautés défavorisées, ainsi qu'utiliser ", 
                      "des outils attrayants et accessibles contribuent au succès d'un tableau de bord.")) %>%
  add_row(en = paste0("Sus aims to more accurately represent and better engage urban residents in order to ", 
                      "harness the momentum surrounding technologically-based approaches to sustainability ", 
                      "for public good."), 
          fr = paste0("Sus vise à représenter plus justement et à mieux impliquer les résidents urbains ", 
                      "afin d'exploiter le momentum entourant les approches technologiques de la durabilité ", 
                      "pour le bien public.")) %>%
  add_row(en = "Further resources:", fr = "Ressources additionnelles") %>%
  # Sidebar menu
  add_row(en = "Active living potential", fr = "Potentiel de vie active") %>%
  add_row(en = "Commuter mode switching", fr = "Changement de mode de transport") %>%
  add_row(en = "Pedestrian realm", fr = "La voie piétonne") %>%
  # General info
  add_row(en = "Why a dashboard?", fr = "Pourquoi un tableau de bord?") %>%
  add_row(en = "Meet the team", fr = "Rencontrez l'équipe") %>%
  add_row(en = "Nature-based solutions", fr = "Solutions basées sur la nature") %>%
  add_row(en = "Simulation", fr = "Simulation") %>%
  # Built environment
  add_row(en = "Built environment", fr = "Le cadre bâti") %>%
  add_row(en = "Pedestrian capacity for social distancing (census tracts)", 
          fr = "Capacité de distanciation sociale dans la voie piétonne (secteur de recensement)") %>%
  add_row(en = "Pedestrian capacity for social distancing (dissemination areas)", 
          fr = "Capacité de distanciation sociale dans la voie piétonne (aire de diffusion)") %>%
  add_row(en = "Explore sidewalks and parks", fr = "Explorez les trottoirs et les parcs") %>%
  add_row(en = paste0("<ul><li>  The top 3 boroughs that have the highest proportion of people living ", 
                      "in census tracts with a low capacity for pedestrian physical distancing were 1) ", 
                      "Le Plateau-Mont-Royal (74%), 2) Villeray-Saint-Michel-Parc-Extension (65%) and 3) ", 
                      "Montréal Nord (60%).</ul>"), 
          fr = paste0("<ul><li>  Les trois arrondissements ayant la plus grande proportion de personnes vivant ", 
                      "dans des secteurs de recensement avec une faible capacité pour une distanciation physique ", 
                      "piétonne sont 1) Le Plateau-Mont-Royal (74%), 2) Villeray-Saint-Michel-Parc-Extension (65%) ", 
                      "and 3) Montréal Nord (60%).</ul>")) %>%
  # Bivariate analysis
  add_row(en = "Select your second variable", fr = "Choisissez votre deuxième variable") %>%
  add_row(en = "Net Median Income", fr = "Revenu médian net") %>%
  add_row(en = "Visible Minority Population Proportion", fr = "Minorité visible (proportion de la population)") %>%
  add_row(en = "Immigrant Population Proportion", fr = "Immigrant (proportion de la population)") %>%
  add_row(en = "Montreal Covid-19 Expanded Active Transit Corridors", 
          fr = "Voies actives et sécuritaires de la Ville de Montréal (Covid-19)") %>%
  add_row(en = "Play with the slider to filter the map", 
          fr = "Jouez avec le curseur afin de filtrer et modifier la carte") %>%
  add_row(en = "Explore", fr = "Explorez") %>%
  add_row(en = "Did you know?", fr = "Saviez-vous?") %>%
  # names of list
  add_row(en = "Housing", fr = "Logement") %>% 
  add_row(en = "Income", fr = "Revenu") %>% 
  add_row(en = "Immigration", fr = "Immigration") %>% 
  add_row(en = "Transportation", fr = "Déplacement domicile-travail") %>% 
  # geographic boundaries
  add_row(en = "borough/city", fr = "de l'arrondissement/de la ville") %>% 
  add_row(en = "boroughs or cities", fr = "des arrondissements ou des villes") %>% 
  add_row(en = "census tract", fr = "du secteur de recensement") %>%
  add_row(en = "census tracts", fr = "des secteurs de recensement") %>% 
  add_row(en = "dissemination area", fr = "de l'aire de diffusion") %>%
  add_row(en = "dissemination areas", fr = "des aires de diffusion") %>%
  add_row(en = paste0("Census tract {dat$name}"), 
          fr = paste0("Secteur de recensement {dat$name}")) %>% 
  add_row(en = paste0("Dissemination area {dat$name}"), 
          fr = paste0("Aire de diffusion {dat$name}")) %>% 
  add_row(en = paste0("{dat$name_2} of {place_name}"), 
          fr = paste0("{dat$name_2} de {place_name}")) %>% 
  # Housing
  add_row(en = "Tenant-occupied (%)", fr = "Occupé par locataire (%)") %>% 
  add_row(en = "Average rent", fr = "Loyer moyen") %>% 
  add_row(en = "Average property value", fr = "Value foncière moyenne") %>% 
  add_row(en = "Unaffordable housing (%)", fr = "Logement inabordable (%)") %>% 
  add_row(en = "Unsuitable housing (%)", fr = "Logement inadéquat (%)") %>% 
  # Income
  add_row(en = "Median household income", fr = "Revenu médian des ménages") %>% 
  add_row(en = "Income under $50k (%)", fr = "Revenu inférieur à 50k (%)") %>% 
  add_row(en = "Income between $50k-$100k (%)", fr = "Revenu entre 50k-100k (%)") %>% 
  add_row(en = "Income above $100k (%)", fr = "Revenu supérieur à 100k (%)") %>% 
  # Immigration
  add_row(en = "Immigrants (%)", fr = "Immigrants (%)") %>% 
  add_row(en = "New immigrants (%)", fr = "Nouveaux immigrants (%)") %>% 
  # Transportation / Déplacement domicile-travail
  add_row(en = "Drive to work (%)", fr = "Conducteur ou passager (%)") %>% 
  add_row(en = "Walk or cycle to work (%)", fr = "À pied ou à vélo (%)") %>% 
  add_row(en = "Public transit to work (%)", fr = "Transport en commun (%)") %>% 
  add_row(en = "15 minutes to work (%)", fr = "Trajet de 15 minutes (%)") %>% 
  add_row(en = "15-30 minutes to work (%)", fr = "Trajet de 15-30 minutes (%)") %>% 
  add_row(en = "30-45 minutes to work (%)", fr = "Trajet de 30-45 minutes (%)") %>% 
  add_row(en = "45-60 minutes to work (%)" , fr = "Trajet de 45-60 minutes (%)") %>% 
  # For my life to be easier
  add_row(en = "----" , fr = "----") %>% 
  # Quintiles and quantitative terms
  add_row(en = "much larger than" , fr = "beaucoup plus grand que") %>%
  add_row(en = "larger than" , fr = "plus grand que") %>%
  add_row(en = "almost the same as" , fr = "presque le même que") %>%
  add_row(en = "smaller than" , fr = "plus petit que") %>%
  add_row(en = "much smaller than" , fr = "beaucoup plus petit que") %>%
  add_row(en = "larger" , fr = "plus grand") %>%
  add_row(en = "smaller" , fr = "plus petit") %>%
  add_row(en = "strong" , fr = "fort") %>%
  add_row(en = "poor" , fr = "faible") %>%
  add_row(en = "moderate" , fr = "modéré") %>%
  # Correlation
  add_row(en = "positive" , fr = "positive") %>%
  add_row(en = "negative" , fr = "négative") %>%
  add_row(en = "weak" , fr = "faible") %>%
  add_row(en = "higher" , fr = "plus grandes") %>%
  add_row(en = "lower" , fr = "plus petites") %>%
  add_row(en = "with only a few exceptions" , fr = "à quelques exceptions près") %>%
  add_row(en = "although with some exceptions" , fr = "bien qu'avec des exceptions") %>%
  add_row(en = "although with many exceptions" , fr = "bien qu'avec beaucoup d'exceptions") %>%
  # Bivariate comparison 
  add_row(en = "dramatically different" , fr = "radicalement différents") %>%
  add_row(en = "substantially different" , fr = "sensiblement différents") %>%
  add_row(en = "considerably different" , fr = "modérément différents") %>%
  # CanAle module
  # Chosen value and comparison
  add_row(en = paste0("At the {scale_singular} scale, the CanALE index varies from ",
                      "{min_val} to {max_val}, with an average value of {mean_val} ",
                      "and a median value of {median_val}. ",
                      "Two thirds of {scale_plural} have a score between {quant_low} ",
                      "and {quant_high}."),
          fr = paste0("À l'échelle {scale_singular}, l'index AVA-Can varie de ",
                      "{min_val} à {max_val}, avec une valeur moyenne de {mean_val} ",
                      "et une valeur médianne de {median_val}. ",
                      "Deux tiers {scale_plural} ont un score se situant entre {quant_low} ",
                      "et {quant_high}.")) %>% 
  # Mohawk Territory
  add_row(en = paste0("<strong>Kahnawake Mohawk Territory</strong>",
                      "<p>Statistics Canada does not gather the same ",
                      "data for indigenous reserves in the Census as it does ",
                      "for other jurisdictions, so we cannot display findings ",
                      "here."),
          fr = paste0("<strong>Kahnawake (Réserve indienne)</strong>",
                      "<p>Dans le cadre du recensement, Statistique Canada ne ",
                      "recueille pas les mêmes données pour les réserves",
                      "autochtones que dans les autres juridictions, nous ne ",
                      "pouvons donc pas afficher de résultats ici.")) %>%
  # CanALE active living potential
  add_row(en = "Active living potential: the CanALE index",
          fr = "Potentiel de vie active: l'index AVA-Can") %>% 
  add_row(en = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} has a population of ",
                      "{prettyNum(dat$population, ',')} and a CanALE index ",
                      "score of {round(poly_value, 2)}, which is {larger_smaller} ",
                      "the region-wide median of {median_val}.", 
                      
                      "<p>{place_name} has {poor_strong} potential for active ", 
                      "living, with a CanALE index score higher than {percentile}% ",
                      "of {scale_plural} in the Montreal region."),
          fr = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} a une population de ",
                      "{prettyNum(dat$population, ',')} et un score d'index ",
                      "AVA-Can de {round(poly_value, 2)}, ce qui est {larger_smaller} ",
                      "la médiane régionale de {median_val}.", 
                      
                      "<p>{place_name} a un potentiel {poor_strong} de vie ", 
                      "active, avec un score d'index AVA-Can plus grand que {percentile}% ",
                      "des {scale_plural} dans la région de Montréal.")) %>% 
  # Correlation explanation
  add_row(en = paste0("<p>{var_explanation}", 
                      "<p>The CanALE index has effectively no correlation ",
                      "({correlation}) with {var_name} at the ",
                      "{scale_singular} scale.",
                      "<p>This means that, at the {scale_singular} scale, ", 
                      "there is no relationship between the two variables."),
          fr = paste0("<p>{var_explanation}", 
                      "<p>L'index AVA-Can n'a en fait aucune corrélation ",
                      "({correlation}) avec {var_name} à l'échelle ",
                      "'{scale_singular}'.",
                      "<p>Cela signifie que, à l'échelle '{scale_singular}', ", 
                      "il n'y a pas de relation entre ces deux variables.")) %>% 
  add_row(en = paste0("<p>{var_explanation}", 
                      "<p>The CanALE index has a {strong_weak} {pos_neg} ",
                      "correlation ({correlation}) with '{tolower(var_name)}' at the ",
                      "{scale_singular} scale.",
                      "<p>This means that, in general, {scale_plural} with higher ",
                      "potential for active living tend to have {higher_lower} ",
                      "values for '{tolower(var_name)}', {high_low_disclaimer}."),
          fr = paste0("<p>{var_explanation}", 
                      "<p>L'index AVA-Can a un index de corrélation {strong_weak} et {pos_neg} ",
                      "({correlation}) avec '{tolower(var_name)}' à l'échelle ",
                      "'{scale_singular}'.",
                      "<p>Cela signifie qu'en général, les {scale_plural} avec un haut ",
                      "potentiel de vie active tendent à avoir des {higher_lower} ",
                      "valeurs pour '{tolower(var_name)}', {high_low_disclaimer}.")) %>% 
  # Bivariate comparison
  add_row(en = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} has a population of ",
                      "{prettyNum(dat$population, ',')}, a CanALE index score ",
                      "of {round(poly_value_1, 2)}, and a '{tolower(var_name)}' ",
                      "value of {round(poly_value_2, 2)}. ",
                      
                      "<p>These two scores are {relative_position}, in relative ",
                      "terms. {place_name} has a CanALE index score higher ",
                      "than {percentile_left}% of {scale_plural} and ",
                      "a '{tolower(var_name)}' score higher than ", 
                      "{percentile_right}% of {scale_plural} in the ",
                      "Montreal region."),
          fr = paste0("<strong>{place_heading}</strong>", 
                      
                      "<p>{place_name} a une population de ",
                      "{prettyNum(dat$population, ',')}, un score d'index AVA-Can ",
                      "de {round(poly_value_1, 2)}, et une '{tolower(var_name)}' ",
                      "valeur de {round(poly_value_2, 2)}. ",
                      
                      "<p>Ces deux scores sont {relative_position}, en termes ",
                      "relatifs. {place_name} a un score d'index AVA-Can plus grand ",
                      "que {percentile_left}% des {scale_plural} et ",
                      "un score '{tolower(var_name)}' plus grand que ", 
                      "{percentile_right}% des {scale_plural} dans la ",
                      "région de Montréal.")) %>% 
  # Did you know
  add_row(en = "Hide" , fr = "En voir moins") %>%
  add_row(en = "Learn more" , fr = "En savoir plus") %>%
  add_row(en = "Show" , fr = "Afficher") %>%
  # Pedestrian realm 
  add_row(en = "Perform a Bivariate Analysis" , fr = "Effectuez une analyse bivariée") %>%
  add_row(en = "Walkable Access to Key Amenities" , fr = "Accès à pied aux services de base") %>%
  add_row(en = "Net Median Income" , fr = "Revenu médian net") %>%
  add_row(en = "Original Plan (May 15, 2020)" , fr = "Plan initial (15 mai 2020") %>%
  add_row(en = "Revised Plan (July 25, 2020)" , fr = "Plan révisé (25 juillet 2020") %>%
  add_row(en = "Choose more variables and explore further" , fr = "Choisissez d'autres variables et explorez davantage") %>%
  add_row(en = "Population density per square km" , fr = "Densité de population par kilomètre carré") %>%
  add_row(en = "Pedestrian social distancing capacity" , fr = "Capacité de distanciation sociale des piétons") %>%
  add_row(en = "Work commutes by car (%)" , fr = "Trajets domicile-travail en voiture (%)") %>%
  add_row(en = "Trajet MTL 2016 data on pedestrian flows" , fr = "Données Trajet MTL 2016 sur les déplacements des piétons") %>%
  add_row(en = "Explore" , fr = "Explorez") %>%  
  add_row(en = "Capacity for pedestrian social distancing" , fr = "Capacité de distanciation sociale des piétons") %>% 
  add_row(en = "Capacity for pedestrian social distancing (%)" , fr = "Capacité de distanciation sociale des piétons (%)") %>% 
  add_row(en = paste0("Capacity of local population to make ",
                      "trips on foot while maintaining 2 meters distance (%)"),
          fr = paste0("Capacité de la population locale à effectuer des déplacements à ",
                      "pied tout en conservant une distance de 2 mètres (%)")) %>%  
  add_row(en = "Log of Population density / km2" , fr = "Log de la densité de population / km2") %>% 
  add_row(en = "Pedestrian trips per sqm of walkable space index (0 = average)" , fr = "Indice des déplacements à pied par m² d'espace piétonnier (0 = moyenne)") %>%
  add_row(en = "Clear selection" , fr = "Effacez sélection") %>% 
  # Pedestrian realm, social distancing capacity
  add_row(en = paste0("At the census tract scale, after removing outliers with a ",
                      "population below 500, the capacity for pedestrian social distancing varies from ",
                      "{min_ped_ct}% to {max_ped_ct}%, with an average value of {mean_ped_ct}% ",
                      "and a median value of {median_ped_ct}%. ",
                      "Two thirds of census tracts have a score between {quant_low_ped_ct}% ",
                      "and {quant_high_ped_ct}%. Out of the 532 census tracts, ",
                      "227 of them have a capacity score below 100%, ",
                      "while 85 of them have a capacity score below 50%."),
          fr = paste0("À l'échelle du secteur de recensement, après avoir éliminé les données aberrantes avec une ", 
                      "population en dessous de 500 personnes, la capacité pour la distanciation sociale des piétons varie de ",
                      "{min_ped_ct}% à {max_ped_ct}%, avec une valeur moyenne de {mean_ped_ct} ",
                      "et une valeur médiane de {median_ped_ct}%. ",
                      "Deux tiers des secteurs de recensement ont un score entre {quant_low_ped_ct}% ",
                      "et {quant_high_ped_ct}%. Sur les 532 secteurs de recensement, ",
                      "227 d'entre eux ont un score de capacité en dessous de 100%, ",
                      "alors que 85 d'entre eux ont un score de capacité en dessous de 50%.")) %>% 
  add_row(en = paste0("At the dissemination area scale, after removing outliers with a population below 100, the capacity for pedestrian social distancing varies from ",
                      "{min_da_uni}% to {max_da_uni}%, with an average value of {mean_da_uni}% ",
                      "and a median value of {median_da_uni}%. ",
                      "Two thirds of dissemination areas have a score between {quant_low_da_uni}% ",
                      "and {quant_high_da_uni}%."),
          fr = paste0("À l'échelle de l'aire de diffusion, après avoir éliminé les valeurs aberrantes avec une ", 
                      "population en dessous de 100 personnes, la capacité pour la distanciation sociale des piétons varie de ",
                      "{min_da_uni}% à {max_da_uni}%, avec une valeur moyenne de {mean_da_uni}% ",
                      "et une valeur médiane de {median_da_uni}%. ",
                      "Deux tiers des aires de diffusion ont un score entre {quant_low_da_uni}% ",
                      "et {quant_high_da_uni}%.")) %>% 
  add_row(en = paste0("The dissemination area {dat_ped_uni$ID} has a population of ",
                      "{prettyNum(dat_ped_uni$population, ',')} and a pedestrian social distancing capacity ",
                      "of {round(poly_value_ped_uni, 2)}%, which is {larger_smaller_ped_uni} ",
                      "the region-wide median of {median_da_uni}%.", 
                      
                      "<p>Dissemination area {dat_ped_uni$ID} offers a {poor_strong_ped_uni} capacity for its residents to practice social distancing in the local pedestrian realm."),
          fr = paste0("L'aire de diffusion {dat_ped_uni$ID} a une population de ",
                      "{prettyNum(dat_ped_uni$population, ',')} et une capacité de distanciation sociale des piétons ",
                      "de {round(poly_value_ped_uni, 2)}%, ce qui est {larger_smaller_ped_uni} ",
                      "que la médiane régionale de {median_da_uni}%.", 
                      
                      "<p>L'aire de diffusion {dat_ped_uni$ID} offre un potentiel {poor_strong_ped_uni} pour ses résidents de pratiquer la distanciation sociale dans les voies piétonnes locales.")) %>%
  # Pedestrian realm, correlation with other variables
  add_row(en = paste0("The capacity for pedestrian social distancing metric has effectively no correlation ",
                      "({correlation_ped}) with {var_name_ped} at the dissemination area scale. ",
                      "<p>This means that, at the dissemination area scale, ",
                      "there is no relationship between the two variables."),
          fr = paste0("La mesure de la capacité pour la distanciation sociale des piétons n'a en fait aucune corrélation ",
                      "({correlation_ped}) avec '{var_name_ped}' à l'échelle de l'aire de diffusion. ",
                      "<p>Cela signifie que, à l'échelle de l'aire de diffusion, ",
                      "il n'y a pas de relation entre ces deux variables.")) %>% 
  add_row(en = paste0("The capacity for pedestrian social distancing metric has a {strong_weak_ped} {pos_neg_ped} ",
                      "correlation ({correlation_ped}) with '{tolower(var_name_ped)}' at the dissemination area scale. ",
                      "<p>This means that, in general, dissemination areas with higher ",
                      "capacities to allow for pedestrian social distancing tend to have {higher_lower_ped} ",
                      "'{tolower(var_name_ped)}' values, {high_low_disclaimer_ped}."),
          fr = paste0("La mesure de la capacité de distanciation sociale des pétions a un coefficient de corrélation {strong_weak_ped} {pos_neg_ped} ",
                      "({correlation_ped}) avec '{tolower(var_name_ped)}', à l'échelle de l'aire de diffusion. ",
                      "<p>Cela signifie que, en général, les aires de diffusion avec de plus grandes ",
                      "capacités de distanciation sociale des piétons tendent à avoir de {higher_lower_ped} ",
                      "valeurs de '{tolower(var_name_ped)}', {high_low_disclaimer_ped}.")) %>% 
  add_row(en = paste0("Dissemination area {dat_ped_biv$ID} has a population of ",
                      "{prettyNum(dat_ped_biv$population, ',')}, a capacity for pedestrian social distancing ",
                      "of {round(poly_value_1, 2)}%, and a '{tolower(var_name_ped)}' ",
                      "value of {round(poly_value_2, 2)}. ",
                      
                      "<p>These two scores are {relative_position}, in relative ",
                      "terms. Dissemination area {dat_ped_biv$ID} has a capacity for pedestrian social distancing higher ",
                      "than {percentile_left}% of dissemination areas and ",
                      "a '{tolower(var_name_ped)}' score higher than ", 
                      "{percentile_right}% of dissemination areas in the ",
                      "Montreal region."),
          fr = paste0("L'aire de diffusion {dat_ped_biv$ID} a une population de ",
                      "{prettyNum(dat_ped_biv$population, ',')}, une capacité de distanciation sociale des piétons ",
                      "de {round(poly_value_1, 2)}%, et une valeur '{tolower(var_name_ped)}' ",
                      "de{round(poly_value_2, 2)}. ",
                      
                      "<p>Ces deux scores sont {relative_position}, en termes ",
                      "relatifs. L'aire de diffusion {dat_ped_biv$ID} a une capacité de distanciation sociale des piétons plus grande ",
                      "que {percentile_left}% des aires de diffusion et ",
                      "une valeur '{tolower(var_name_ped)}' plus grand que ", 
                      "{percentile_right}% des aires de diffusion de la région ",
                      "de Montréal.")) %>% 
  # Pedestrian realm, sidewalk width
  add_row(en = paste0("Sidewalk width in Montreal varies from ",
                      "{min_sidewalk} meters to {max_sidewalk} meters, ",
                      "with an average value of {mean_sidewalk} meters ",
                      "and a median value of {median_sidewalk} meters. ",
                      "Two thirds of Montreal's sidewalks have widths ",
                      "between {quant_low_sidewalk} meters and {quant_high_sidewalk} meters."),
          fr = paste0("La largeur des trottoirs de Montréal varie de ",
                      "{min_sidewalk} mètres à {max_sidewalk} mètres, ",
                      "avec une valeur moyenne de {mean_sidewalk} mètres ",
                      "et une valeur médiane de {median_sidewalk} mètres. ",
                      "Deux tiers des trottoirs montréalais ont une largeur ",
                      "entre {quant_low_sidewalk} mètres et {quant_high_sidewalk} mètres.")) %>% 
  # Mode shift module
  add_row(en = "Shifting car trips to cycling" , fr = "Transférez les trajets de voiture en vélo") %>% 
  add_row(en = "Share of trips taken by car" , fr = "Part des trajets effectués en voiture") %>% 
  add_row(en = "Average commuting distance" , fr = "Distance moyenne pour du trajet domicile-travail") %>% 
  add_row(en = "Access to cycling infrastructure" , fr = "Accès aux infrastructures cyclables") %>% 
  add_row(en = "% of trips taken by car, by census tract" , fr = "% des trajets effectués en voiture, par secteur de recensement") %>% 
  add_row(en = "Modal shift scenarios" , fr = "Scénarios de transfert modal") %>% 
  add_row(en = "Baseline" , fr = "Base de référence") %>% 
  add_row(en = "Distance" , fr = "Distance") %>% 
  add_row(en = "Elevation/time" , fr = "Élévation/temps") %>% 
  add_row(en = "Show baseline" , fr = "Afficher la base de référence") %>% 
  add_row(en = "Cycling distance (km):" , fr = "Distance parcourue à vélo (km):") %>% 
  add_row(en = "Elevation gain (m):" , fr = "Gain d'élévation (m):") %>% 
  add_row(en = "Time ratio:" , fr = "Ratio de temps:") %>% 
  add_row(en = "VKT Reduction" , fr = "Réduction de KPV") %>% 
  add_row(en = "Cycling network" , fr = "Réseau cycliste") %>% 
  # Mode shift, scenarios
  add_row(en = "Criteria: Cycling Distance (km)" , fr = "Critère: Distance parcourue à vélo (km)") %>% 
  add_row(en = "Potential Cyclable Trips (per day)" , fr = "Trajets cyclistes potentiels (par jour)") %>% 
  add_row(en = "VKT Savings (per day)" , fr = "Économies de KPV (par jour)") %>% 
  add_row(en = "Criteria: Elevation Gain (m)" , fr = "Critère: Gain d'élévation (m)") %>% 
  add_row(en = "Criteria: Time Ratio" , fr = "Critère: Ratio de temps") %>%
  # Mode shift, legend
  add_row(en = "Access to Cycling Infrastructure (km/sq.km)" , fr = "Accès aux infrastructures cyclables (km/km2)") %>%
  add_row(en = "Share of Car Trips by Origin (%)" , fr = "Part des trajets en voiture par origine (%)") %>%
  add_row(en = "Average Commuting Distance (km)" , fr = "Distance moyenne du trajet domicile-travail (km)") %>%
  # Mode shift, map
  add_row(en = "Access to cycling inf. (km/sq.km)" , fr = "Accès aux infr. cyclables (km/km2)") %>%
  add_row(en = "Share of trips taken by car (%)" , fr = "Part des trajets effectués en voiture (%)") %>%
  add_row(en = "Average commuting distance (km)" , fr = "Distance moyenne du trajet domicile-travail (km)") %>%
  add_row(en = "Cycling infrastructure (km/sq.km) by census tract:" , fr = "Infrastructure cyclable (km/km2) par secteur de recensement:") %>%
  add_row(en = "Length of the average commute (km), by census tract:" , fr = "Distance moyenne du trajet domicile-travail (km), par secteur de recensement :") %>%
  add_row(en = "Access to cycling inf. (km/sq.km)" , fr = "Accès aux infr. cyclables (km/km2)") %>%
  distinct(en, .keep_all = T)

write_csv(translation_fr, "translations/translation_fr.csv")
