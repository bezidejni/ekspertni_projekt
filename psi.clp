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

(defrule GetDvoriste ""
   =>
   (bind ?response (yes-or-no-p "Imate li dvoriste (da/ne)?"))
   (assert (dvoriste ?response)))

;;;****************************
;;;* PSI *
;;;****************************

(defrule Boxer ""
   (dvoriste da)
   =>
   (assert (breed-found boxer))
   (printout t crlf))
   (printout t "Naziv pasmine: Bokser" crlf)
   (printout t "Težina: 10-15kg" crlf)
   (printout t "Briga za dlaku: lagano" crlf))

;;;****************************
;;;* PROGRAM *
;;;****************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The Engine Diagnosis Expert System")
  (printout t crlf crlf))

(defrule None
   (declare (salience -100))
   (not (breed-selected ?))
   =>
   (printout t "Ne postoji pas koji odgovara vašim kriterijima, probajte nabaviti mačku." crlf))