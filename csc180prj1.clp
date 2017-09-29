;;Facts
(deffacts timeAssumptions
	(rules no))

;;Fuzzy Set Definition

;;AgeGroup	I=Infant
;;			T=Toddler
;;			Y=Youngster
;;We are not using the "traditional" definition 
;;of these age groups
(deftemplate AgeGroup
	0 48 months
	((I (6 1) (16 0))
	 (T (6 0) (16 1) (24 1) (36 0))
	 (Y (24 0) (36 1))))

;;Elapsed Time 	S=Small amount of time
;;				M=Medium amount of time
;;				L=Long amount of time
;;				XL=Xtra Long amount of time
(deftemplate ElapsedFoodTime
	0 300 minutes
	((S (30 1) (60 0))
	 (M (30 0) (60 1) (90 1) (120 0))
	 (L (90 0) (120 1) (150 1) (180 0))
	 (XL (150 0) (180 1))))

(deftemplate ElapsedNapTime
	0 300 minutes
	((S (30 1) (60 0))
	 (M (30 0) (60 1) (90 1) (120 0))
	 (L (90 0) (120 1) (150 1) (180 0))
	 (XL (150 0) (180 1))))

(deftemplate ElapsedDiaperTime
	0 300 minutes
	((S (30 1) (60 0))
	 (M (30 0) (60 1) (90 1) (120 0))
	 (L (90 0) (120 1) (150 1) (180 0))
	 (XL (150 0) (180 1))))

;;Hunger	VH=Very Hungry
;;			H=Hungry
;;			KH=Kinda Hungry
;;			NH=Not Hungry
(deftemplate Hunger
	0 150 minutes
	((VH (0 1) (10 0))
	 (H (0 0) (30 1) (60 0))
	 (KH (30 0) (60 1) (90 0))
	 (NH (60 0) (90 1) (120 1) (150 0))))

;;Nap		VT=Very Tired
;;			T=Tired
;;			KT=Kinda Tired
;;			NT=Not Tired
(deftemplate Nap
	0 300 minutes
	((VT (0 1) (20 0))
	 (T (0 0) (40 1) (60 0))
	 (KT (60 0) (90 1) (120 0))
	 (NT (200 0) (240 1) (300 1))))

;;Diaper	U=Urgent
;;			SO=Soon
;;		    OK=Okay
(deftemplate Diaper
	0 150 minutes
	((U (0 1) (10 0))
	 (SO (0 0) (30 1) (60 1) (90 0))
	 (OK (60 0) (90 1) (120 1) (150 0))))


;;Get inputs
;;Begin with an introduction to the program
(defrule introduction
	?i <- (initial-fact)
	=>
	(printout t "This program is designed to aid caretakers of young children (aged 0-4 years)." crlf)
	(printout t "You will be prompted for the child's name, age, and time of day." crlf crlf)
	(assert (get name))
	(retract ?i))

;;Move into getting inputs
;;Specifically child's name and age
(defrule getNameAge
	?g <- (get name)
	=>
	(printout t "Before we go any farther, what is the child's name? ")
	(bind ?response (read))
	(assert (name ?response))
	(printout t "How old is " ?response " in months (0-48)? ")
	(bind ?response (read))
	(assert (crispAge ?response))
	(assert (get feed time))
	(retract ?g))

;;Next, we will ask the user if they know the last time the baby was napped and fed
(defrule getFeedTimeYN
	(declare (salience 2))
	?f <- (get feed time)
	(name ?name)
	=>
	(printout t crlf "You will now be asked about the last time " ?name " was fed and napped." crlf)
	(printout t "If you don't know when that time was, it will be assumed to be 7:00 AM." crlf crlf)
	(printout t "Do you know the last time the child was fed (yes/no)? ")
	(bind ?response (read))
	(printout t crlf)
	(retract ?f)
	(assert (time fed ?response)))
	
(defrule getActualFeedTime	
	(declare (salience 1))
	(name ?name)
	(rules yes)
	?f <- (time fed yes|Yes|YES|y|Y)
	=>
	(printout t "So, when was the last time " ?name " was fed? ")
	(bind ?response (read))
	(printout t crlf)
	(assert (last fed ?response))
	(assert (get nap time))
	(retract ?f))

(defrule getFeedTimeNo
	(declare (salience 1))
	(name ?name)
	?f <- (time fed no|No|NO|n|N)
	=>
	(printout t "No worries.  We'll just assume " ?name " was last fed at 7 this morning." crlf crlf)
	(assert (last fed 700))
	(assert (get nap time))
	(retract ?f))
	
(defrule getNapTimeYN
	(declare (salience 2))
	?n <- (get nap time)
	(name ?name)
	=>
	(printout t "Do you know what time " ?name " last woke up from a nap  (yes/no)? ")
	(bind ?response (read))
	(assert (time nap ?response))
	(assert (get current time))
	(retract ?n)
	(printout t crlf))
	
(defrule getNapTimeYes
	(declare (salience 1))
	(name ?name)
	(rules yes)
	?n <- (time nap yes|Yes|YES|y|Y)
	=>	
	(printout t "So, keeping the input instructions in mind," crlf)
	(printout t "when was the last time " ?name " woke up? ")
	(bind ?response (read))
	(assert (last nap ?response))
	(assert (get PT status))
	(retract ?n))

	
(defrule getNapTimeNo
	(declare (salience 1))
	(name ?name)
	?get <- (get nap time)
	?n <- (time nap no|No|NO|n|N)
	=>
	(printout t "No worries.  We'll just assume " ?name " last woke up around 7 this morning." crlf crlf)
  (assert (last nap 700))
	(assert (get PT status))
	(retract ?get ?n))

;; <<Diaper Query here>>
;; <= 24 months not potty trained
;; older than 24 must be checked for PT status
(defrule getPtStatus1
	(declare (salience 1))
	?r <- (get PT status)
	(crispAge ?a)
	(> ?a 24)
	=>
	(assert (get PT inquiry))
	(retract ?r))

(defrule getPtStatus2
	(declare (salience 1))
	?r <- (get PT status)
	(crispAge ?a)
	(<= ?a 24)
	=>
	(assert (time diaper no))
	(retract ?r))

;; inquire potty training
(defrule getPtYN
	(declare (salience 1))
	?n <- (get PT inquiry)
	(name ?name)
	=>
	(printout t "Do you know if " ?name " is potty trained  (yes/no)? ")
	(bind ?response (read))
	(assert (time diaper ?response))
	(assert (get current time))
	(retract ?n)
	(printout t crlf))

(defrule getPtYes
	(declare (salience 1))
	(name ?name)
	(rules yes)
	?n <- (time diaper yes|Yes|YES|y|Y)
	=>	
	(assert	(?name is PT))
	(retract ?n))

(defrule getPtNo
	declare (salience 1))
	(name ?name)
	?get <- (get diaper time)
	?n <- (time diaper no|No|NO|n|N)
	=>
	(printout t "So, keeping the input instructions in mind," crlf)
	(printout t "when was the last time " ?name " had a diaper change? ")
	(bind ?response (read))
	(assert (last diaper ?response))
	(assert (get current time))
	(retract ?get ?n))

(defrule explainInput
	(declare (salience 3))
	?i <- (explain input)
	?r <- (rules no)
	=>
	(printout t "Please ensure your input is in a 24 hour format," crlf)
	(printout t "and without a colon (i.e. 1:30 PM should be input as 1330)." crlf crlf)
	(assert (rules yes))
	(retract ?i ?r))

(defrule timePrompt
	(declare (salience 1))
	(time fed|nap yes|Yes|YES|y|Y)
	=>
	(printout t "Great!  You'll now be prompted for that time." crlf)
	(assert (explain input)))
	
(defrule inputRuleCheck
	(declare (salience 1))
	(get current time)
	(rules no)
	=>
	(printout t "We now need to know the current time." crlf)
	(assert (explain input)))
	
(defrule getCurrTime
	?get <- (get current time)
	=>
	(printout t "Keeping the input instructions in mind, what is the current time? ")
	(bind ?response (read))
	(assert (currTime ?response))
	(printout t crlf)
	(retract ?get))

;; <<UNFINISHED>>
(defrule time-elapsed1
	?i <- (currTime ?curr)
	?f <- (last fed ?food)
	?n <- (last nap ?nap)
	=>
	(assert (crispFoodTime (+ (* 60 (- (div ?curr 100) (div ?food 100))) (- (mod ?curr 100) (mod ?food 100)))))
	(assert (crispNapTime (+ (* 60 (- (div ?curr 100) (div ?nap 100))) (- (mod ?curr 100) (mod ?nap 100)))))
	(retract ?i ?f ?n))

;; diaper special case
(defrule time-elapsed2
	?d <- (last diaper ?diap)
	=>
	(assert (crispDiaperTime (+ (* 60 (- (div ?curr 100) (div ?food 100))) (- (mod ?curr 100) (mod ?food 100)))))
	(retract ?d))
	
;;Fuzzify
(defrule fuzzify1
	(crispAge ?a)
	(crispFoodTime ?f)
	(crispNapTime ?n)
	=>
	(assert (AgeGroup (?a 0) (?a 1) (?a 0)))
	(assert (ElapsedFoodTime (?f 0) (?f 1) (?f 0)))
	(assert (ElapsedNapTime (?n 0) (?n 1) (?n 0))))

;; diaper special case
(defrule fuzzify2
	(cripsDiaperTime ?d)
	=>
	(assert (ElapsedDiaperTime (?d 0) (?d 1) (?d 0))))

;;defuzzify the outputs
(defrule deffuzzify1
	(declare (salience -1))
	?h <- (Hunger ?)
	?n <- (Nap ?)
	?d <- (Diaper ?)
	=>
	(bind ?ht (moment-defuzzify ?h))
	(bind ?nt (moment-defuzzify ?n))
	(bind ?dt (moment-defuzzify ?d))
	(assert (feed in ?ht))
	(assert (nap in ?nt))
	(assert (diaper in ?dt)))

;; diaper special case
(defrule deffuzzify2
	(declare (salience -1))
	?d <- (Diaper ?)
	=>
	(bind ?dt (moment-defuzzify ?d))
	(assert (diaper in ?dt)))

(defrule output1
	?f<-(feed in ?food)
	?n<-(nap in ?nap)
	=>
	(printout t "Feed the child in " (div ?food 60) " hours and " (integer(mod ?food 60)) " minutes." crlf)
	(printout t "Then make sure the child naps in " (div ?nap 60) " hours and " (integer(mod ?nap 60)) " minutes." crlf)
	(retract ?f)
	(retract ?n))

;; diaper special case
(defrule output2
	(declare (salience -1))
	?d<-(diaper in ?diap)
	=>
	(printout t "Also, check for a diaper change in " (div ?diap 60) " hours and " (integer(mod ?diap 60)) " minutes." crlf)
	(retract ?d))



;; FAM rule definition for Hunger

(defrule SIh
	(ElapsedFoodTime S)
	(AgeGroup I)
	=>
	(assert (Hunger KH)))

(defrule STh
	(ElapsedFoodTime S)
	(AgeGroup T)
	=>
	(assert (Hunger NH)))

(defrule SYh
	(ElapsedFoodTime S)
	(AgeGroup Y)
	=>
	(assert (Hunger NH)))

(defrule MIh
	(ElapsedFoodTime M)
	(AgeGroup I)
	=>
	(assert (Hunger H)))

(defrule MTh
	(ElapsedFoodTime M)
	(AgeGroup T)
	=>
	(assert (Hunger KH)))

(defrule MYh
	(ElapsedFoodTime M)
	(AgeGroup Y)
	=>
	(assert (Hunger KH)))

(defrule LIh
	(ElapsedFoodTime L)
	(AgeGroup I)
	=>
	(assert (Hunger VH)))

(defrule LTh
	(ElapsedFoodTime L)
	(AgeGroup T)
	=>
	(assert (Hunger H)))

(defrule LYh
	(ElapsedFoodTime L)
	(AgeGroup Y)
	=>
	(assert (Hunger H)))

(defrule XLIh
	(ElapsedFoodTime XL)
	(AgeGroup I)
	=>
	(assert (Hunger VH)))

(defrule XLTh
	(ElapsedFoodTime XL)
	(AgeGroup T)
	=>
	(assert (Hunger VH)))

(defrule XLYh
	(ElapsedFoodTime XL)
	(AgeGroup Y)
	=>
	(assert (Hunger VH)))


;;FAM rule definition for Nap Time

(defrule SIn
	(ElapsedNapTime S)
	(AgeGroup I)
	=>
	(assert (Nap T)))

(defrule STn
	(ElapsedNapTime S)
	(AgeGroup T)
	=>
	(assert (Nap NT)))

(defrule SYn
	(ElapsedNapTime S)
	(AgeGroup Y)
	=>
	(assert (Nap NT)))

(defrule MIn
	(ElapsedNapTime M)
	(AgeGroup I)
	=>
	(assert (Nap VT)))

(defrule MTn
	(ElapsedNapTime M)
	(AgeGroup T)
	=>
	(assert (Nap KT)))

(defrule MYn
	(ElapsedNapTime M)
	(AgeGroup Y)
	=>
	(assert (Nap NT)))

(defrule LIn
	(ElapsedNapTime L)
	(AgeGroup I)
	=>
	(assert (Nap VT)))

(defrule LTn
	(ElapsedNapTime L)
	(AgeGroup T)
	=>
	(assert (Nap T)))

(defrule LYn
	(ElapsedNapTime L)
	(AgeGroup Y)
	=>
	(assert (Nap KT)))

(defrule XLIn
	(ElapsedNapTime XL)
	(AgeGroup I)
	=>
	(assert (Nap VT)))

(defrule XLTn
	(ElapsedNapTime XL)
	(AgeGroup T)
	=>
	(assert (Nap VT)))

(defrule XLYn
	(ElapsedNapTime XL)
	(AgeGroup Y)
	=>
	(assert (Nap KT)))


;; FAM rule definition for Diaper Change Time

(defrule SId
	(ElapsedDiaperTime S)
	(AgeGroup I)
	=>
	(assert (Diaper SO)))

(defrule MId
	(ElapsedDiaperTime M)
	(AgeGroup I)
	=>
	(assert (Diaper SO)))

(defrule LId
	(ElapsedDiaperTime L)
	(AgeGroup I)
	=>
	(assert (Diaper U)))

(defrule XLId
	(ElapsedDiaperTime XL)
	(AgeGroup I)
	=>
	(assert (Diaper U)))

(defrule STd
	(ElapsedDiaperTime S)
	(AgeGroup T)
	=>
	(assert (Diaper OK)))

(defrule MTd
	(ElapsedDiaperTime M)
	(AgeGroup T)
	=>
	(assert (Diaper SO)))

(defrule LTd
	(ElapsedDiaperTime L)
	(AgeGroup T)
	=>
	(assert (Diaper SO)))

(defrule XLTd
	(ElapsedDiaperTime XL)
	(AgeGroup T)
	=>
	(assert (Diaper U)))

(defrule SYd
	(ElapsedDiaperTime S)
	(AgeGroup Y)
	=>
	(assert (Diaper OK)))

(defrule MYd
	(ElapsedDiaperTime M)
	(AgeGroup Y)
	=>
	(assert (Diaper OK)))

(defrule LYd
	(ElapsedDiaperTime L)
	(AgeGroup Y)
	=>
	(assert (Diaper SO)))

(defrule XLYd
	(ElapsedDiaperTime XL)
	(AgeGroup Y)
	=>
	(assert (Diaper SO)))