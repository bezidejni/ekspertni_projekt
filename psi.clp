;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question crlf)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question da ne d n))
   ?response)

;;;****************************
;;;* SKUPLJANJE INFORMACIJA *
;;;****************************

(defrule GetDresura ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Imate li prethodnog iskustva s dresurom pasa (da/ne)?"))
   (assert (prethodno-iskustvo ?response)))

(defrule GetLinjanje ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Da li vam je bitno da se pas ne linja (da/ne)?"))
   (assert (linjanje-bitno ?response)))

(defrule GetVrucina ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Jesu li ljeta  vruća tamo gdje živite (da/ne)?"))
   (assert (podnosi-vrucinu ?response)))

(defrule GetHladnoca ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Jesu li zime hladne tamo gdje živite (da/ne)?"))
   (assert (podnosi-hladnocu ?response)))

(defrule GetDrugiPsi ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Imate li druge pse (da/ne)?"))
   (assert (drugi-psi ?response)))

(defrule GetDrugiLjubimci ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Imate li druge kućne ljubimce (da/ne)?"))
   (assert (drugi-ljubimci ?response)))

(defrule GetStranci ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Je li vam bitno se pas dobro ponaša prema strancima (da/ne)?"))
   (assert (stranci ?response)))

(defrule GetZastitnicki ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Je li vam bitno da je pas zaštitnički nastrojen (da/ne)?"))
   (assert (zastitnicki ?response)))

(defrule GetBrigaZaDlaku ""
  (declare (salience 5))
   =>
   (bind ?response (yes-or-no-p "Imate li puno vremena za brigu oko dlake (češljanje i sl.) (da/ne)?"))
   (assert (zahtjevna-dlaka ?response)))
;;;****************************
;;;* PSI *
;;;****************************

(defrule AustralskiCattleDog ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-ljubimci ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Australian Cattle Dog" crlf)
   (printout t "Težina: 15-21kg" crlf)
   (assert (breed-found australian-cattle-dog)))

(defrule AustralianShephard ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Australian Shephard" crlf)
   (printout t "Težina: 18-30kg" crlf)
   (assert (breed-found australian-shephard)))

(defrule BorderCollie ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-ljubimci ne)
   (stranci ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Border Collie" crlf)
   (printout t "Težina: 13-21kg" crlf)
   (assert (breed-found border-collie)))

(defrule CardiganWelshCorgi ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-psi ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Cardigan Welsh Corgi" crlf)
   (printout t "Težina: 11-17kg" crlf)
   (assert (breed-found carigan-welsh-corgi)))


(defrule AiredaleTerrier ""
   (prethodno-iskustvo da)
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Airedale Terrier" crlf)
   (printout t "Težina: 25-27kg" crlf)
   (assert (breed-found airedale-terrier)))


(defrule Dalmatian ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Dalmatian" crlf)
   (printout t "Težina: 18-29kg" crlf)
   (assert (breed-found dalmatian)))


(defrule EnglishSpringerSpaniel ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: English Springer Spaniel" crlf)
   (printout t "Težina: 18-22kg" crlf)
   (assert (breed-found english-springer-spaniel)))

(defrule MiniaturePinscher ""
   (linjanje-bitno ne)
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (stranci ne)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Miniature Pinscher" crlf)
   (printout t "Težina: 3-5kg" crlf)
   (assert (breed-found miniature-pinscher)))


(defrule Beagle ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Beagle" crlf)
   (printout t "Težina: 8-13kg" crlf)
   (assert (breed-found beagle)))


(defrule BerneseMountainDog ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Bernese Mountain Dog" crlf)
   (printout t "Težina: 32-55" crlf)
   (assert (breed-found bernese-mountain-dog)))


(defrule Boxer ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Boxer" crlf)
   (printout t "Težina: 23-36kg" crlf)
   (assert (breed-found boxer)))


(defrule Collie ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Collie" crlf)
   (printout t "Težina: 27-34kg" crlf)
   (assert (breed-found collie)))


(defrule GoldenRetriever ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Golden Retriever" crlf)
   (printout t "Težina: 25-34kg" crlf)
   (assert (breed-found golden-retriever)))


(defrule PembrokeWelshCorgi ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Pembroke Welsh Corgi" crlf)
   (printout t "Težina: 11-13kg" crlf)
   (assert (breed-found pembroke-welsh-corgi)))


(defrule GermanShepherd ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-psi ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: German Shepherd" crlf)
   (printout t "Težina: 34-43kg" crlf)
   (assert (breed-found german-shepherd)))


(defrule ShetlandSheepdog ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (stranci ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Shetland Sheepdog" crlf)
   (printout t "Težina: 9-10kg" crlf)
   (assert (breed-found shetland-sheepdog)))


(defrule BelgianSheepdog ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Belgian Sheepdog" crlf)
   (printout t "Težina: 18-34kg" crlf)
   (assert (breed-found belgian-sheepdog)))


(defrule Whippet ""
   (prethodno-iskustvo da)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Whippet" crlf)
   (printout t "Težina: 9-18kg" crlf)
   (assert (breed-found whippet)))


(defrule Greyhound ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-psi ne)
   (podnosi-hladnocu ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Greyhound" crlf)
   (printout t "Težina: 27-32kg" crlf)
   (assert (breed-found greyhound)))


(defrule RhodesianRidgeback ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Rhodesian Ridgeback" crlf)
   (printout t "Težina: 32-39kg" crlf)
   (assert (breed-found rhodesian-ridgeback)))


(defrule Dachshund ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-ljubimci ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Dachsund" crlf)
   (printout t "Težina: 5-15kg" crlf)
   (assert (breed-found dachshund)))


(defrule BichonFrise ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Bichon Frise" crlf)
   (printout t "Težina: 4-7kg" crlf)
   (assert (breed-found bichon-frise)))


(defrule YorkshireTerrier ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Yorkshire Terrier" crlf)
   (printout t "Težina: <4kg" crlf)
   (assert (breed-found yorkshire-terrier)))


(defrule Poodle ""
   (prethodno-iskustvo da)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Poodle" crlf)
   (printout t "Težina: 20-29kg" crlf)
   (assert (breed-found poodle)))


(defrule Bulldog ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-psi ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Bulldog" crlf)
   (printout t "Težina: 18-23kg" crlf)
   (assert (breed-found bulldog)))


(defrule Bullmastiff ""
   (linjanje-bitno ne)
   (drugi-psi ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Bullmastiff" crlf)
   (printout t "Težina: 45-59kg" crlf)
   (assert (breed-found bullmastiff)))


(defrule Pug ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Pug" crlf)
   (printout t "Težina: 6-8kg" crlf)
   (assert (breed-found pug)))


(defrule Mastiff ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Mastiff" crlf)
   (printout t "Težina: 79-86kg" crlf)
   (assert (breed-found mastiff)))


(defrule CavalierKingCharlesSpaniel ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-psi ne)
   (zastitnicki ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Cavalier King Charles Spaniel" crlf)
   (printout t "Težina: 6-8kg" crlf)
   (assert (breed-found cavalier-king-charles-spaniel)))


(defrule Chihuahua ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-psi ne)
   (stranci ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Chihuahua" crlf)
   (printout t "Težina: <3kg" crlf)
   (assert (breed-found chihuahua)))


(defrule Maltese ""
   (prethodno-iskustvo da)
   (zastitnicki ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Maltese" crlf)
   (printout t "Težina: 2-4kg" crlf)
   (assert (breed-found maltese)))


(defrule Pekinese ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Pekinese" crlf)
   (printout t "Težina: <4kg" crlf)
   (assert (breed-found pekinese)))


(defrule AlaskanMalamute ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Alaskan Malamute" crlf)
   (printout t "Težina: 34-39kg" crlf)
   (assert (breed-found alaskan-malamute)))


(defrule DobermanPinscher ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (drugi-psi ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Doberman Pinscher" crlf)
   (printout t "Težina: 29-41kg" crlf)
   (assert (breed-found doberman-pinscher)))


(defrule SaintBernard ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Saint Bernard" crlf)
   (printout t "Težina: 54-91kg" crlf)
   (assert (breed-found saint-bernard)))


(defrule TibetanTerrier ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (stranci ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Tibetian Terrier" crlf)
   (printout t "Težina: 9-11kg" crlf)
   (assert (breed-found tibetian-terrier)))


(defrule AmericanWaterSpaniel ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-psi ne)
   (stranci ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: American Water Spaniel" crlf)
   (printout t "Težina: 11-20kg" crlf)
   (assert (breed-found american-water-spaniel)))


(defrule IrishTerrier ""
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (stranci ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Irish Terrier" crlf)
   (printout t "Težina: 11-12kg" crlf)
   (assert (breed-found irish-terrier)))


(defrule BostonTerrier ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-vrucinu ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Boston Terrier" crlf)
   (printout t "Težina: 5-11kg" crlf)
   (assert (breed-found boston-terrier)))


(defrule ChowChow ""
   (linjanje-bitno ne)
   (drugi-psi ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Chow Chow" crlf)
   (printout t "Težina: 20-32kg" crlf)
   (assert (breed-found chow-chow)))


(defrule ChinesheSharPei ""
   (linjanje-bitno ne)
   (drugi-psi ne)
   (stranci ne)

   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Chinese Shar-Pei" crlf)
   (printout t "Težina: 20-27kg" crlf)
   (assert (breed-found chinese-sharpei)))


(defrule IrishSetter ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Irish Setter" crlf)
   (printout t "Težina: 27-32kg" crlf)
   (assert (breed-found irish-setter)))


(defrule LabradorRetriever ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Labrador Retriever" crlf)
   (printout t "Težina: 25-36kg" crlf)
   (assert (breed-found labrador-retriever)))


(defrule Papillon ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Papillon" crlf)
   (printout t "Težina: 2-4kg" crlf)
   (assert (breed-found papillon)))


(defrule ShihTzu ""
   (zastitnicki ne)
   (zahtjevna-dlaka da)
   (podnosi-vrucinu ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Shih Tzu" crlf)
   (printout t "Težina: 5-8kg" crlf)
   (assert (breed-found shih-tzu)))


(defrule ShibaInu ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (drugi-psi ne)
   (drugi-ljubimci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Shiba Inu" crlf)
   (printout t "Težina: 7-10kg" crlf)
   (assert (breed-found shiba-inu)))


(defrule EnglishSetter ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: English Setter" crlf)
   (printout t "Težina: 23-29kg" crlf)
   (assert (breed-found english-setter)))


(defrule Brittany ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Brittany" crlf)
   (printout t "Težina: 14-18kg" crlf)
   (assert (breed-found brittany)))


(defrule Viszla ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-hladnocu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Viszla" crlf)
   (printout t "Težina: 20-29kg" crlf)
   (assert (breed-found viszla)))


(defrule BullTerrier ""
   (linjanje-bitno ne)
   (drugi-psi ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Bull Terrier" crlf)
   (printout t "Težina: 23-32kg" crlf)
   (assert (breed-found bull-terrier)))


(defrule SoftCoatedWheatenTerrier ""
   (prethodno-iskustvo da)
   (zastitnicki ne)
   (zahtjevna-dlaka da)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Soft Coated Wheaten Terrier" crlf)
   (printout t "Težina: 14-18kg" crlf)
   (assert (breed-found soft-coated-wheaten-terrier)))


(defrule WestHighlandWhiteTerrier ""
   (prethodno-iskustvo da)
   (zastitnicki ne)
   (zahtjevna-dlaka da)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: West Highland White Terrier" crlf)
   (printout t "Težina: 6-9kg" crlf)
   (assert (breed-found west-highland-white-terrier)))


(defrule Pomeranian ""
   (linjanje-bitno ne)
   (drugi-psi ne)
   (stranci ne)
   (zastitnicki ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Pomeranian" crlf)
   (printout t "Težina: 1-3kg" crlf)
   (assert (breed-found pomeranian)))


(defrule Akita ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Akita" crlf)
   (printout t "Težina: 29-59kg" crlf)
   (assert (breed-found akita)))


(defrule BlackRussianTerrier ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Black Russian Terrier" crlf)
   (printout t "Težina: 36-66kg" crlf)
   (assert (breed-found black-russian-terrier)))


(defrule Rottweiler ""
   (prethodno-iskustvo da)
   (linjanje-bitno ne)
   (stranci ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Rottweiler" crlf)
   (printout t "Težina: 36-60kg" crlf)
   (assert (breed-found rottweiler)))


(defrule SiberianHusky ""
   (linjanje-bitno ne)
   (zastitnicki ne)
   (podnosi-vrucinu ne)
   =>
   (printout t crlf)
   (printout t "Naziv pasmine: Siberian Husky" crlf)
   (printout t "Težina: 16-21kg" crlf)
   (assert (breed-found siberian-husky)))

;;;****************************
;;;* PROGRAM *
;;;****************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Preporuka pasmine")
  (printout t crlf crlf))

(defrule PreporucenePasmine ""
  (declare (salience 3))
  (not (header-printed))
  =>
  (printout t crlf crlf)
  (printout t "Preporučene pasmine:" crlf)
  (assert (header-printed)))

(defrule None
   (declare (salience -100))
   (not (breed-found ?))
   =>
   (printout t "Ne postoji pas koji odgovara vašim kriterijima, probajte nabaviti mačku." crlf))