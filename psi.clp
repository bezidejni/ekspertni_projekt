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
  (not prethodno-iskustvo ?)
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
   (linjanje-bitno ne)
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
  (declare (salience 10))
  (breed-found ?)
  =>
  (printout t crlf crlf)
  (printout t "Preporučene pasmine:" crlf))

(defrule None
   (declare (salience -100))
   (not (breed-found ?))
   =>
   (printout t "Ne postoji pas koji odgovara vašim kriterijima, probajte nabaviti mačku." crlf))