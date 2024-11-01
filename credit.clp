;; Define templates for each attribute with a value and an Order slot
;;The order slot is used to keep track of the order in which the applied rules are fired, justifying the classification

(deftemplate credit-history
    (slot value)
    (slot Order))

(deftemplate debt
    (slot value)
    (slot Order))

(deftemplate income
    (slot value)
    (slot Order))

(deftemplate collateral
    (slot value)
    (slot Order))

;; Define a global variable to track the order of inputs
(defglobal ?*order* = 1) ;; starting from 1

;; Validation functions
;;---------------------------------------------------------------------------
;;the validation functions are used to ensure that the user's input is valid

(deffunction validate-History (?cred)
    (while (not (or (eq ?cred Unknown) (eq ?cred Bad) (eq ?cred Good))) 
        do
            (printout t "Invalid input. Please answer with Unknown, Bad, or Good." crlf)
            (bind ?cred (read)))
    ?cred
)

(deffunction validate-Debt (?d)
    (while (not (or (eq ?d Low) (eq ?d High))) ;;if its not either Low or High
        do
            (printout t "Invalid input. Please answer with Low or High." crlf)
            (bind ?d (read)))
    ?d
)

(deffunction validate-Income (?inc)
    (while (not (or (eq ?inc Low) (eq ?inc Mid) (eq ?inc High))) ;;if its not either Low, Mid, or High
        do
            (printout t "Invalid input. Please answer with Low, Mid, or High." crlf)
            (bind ?inc (read)))
    ?inc
)

(deffunction validate-Collateral (?coll)
    (while (not (or (eq ?coll None) (eq ?coll Adequate))) ;;if its not either None or Adequate
        do
            (printout t "Invalid input. Please answer with None or Adequate." crlf)
            (bind ?coll (read)))
    ?coll
)

;; Rules
;;---------------------------------------------------------------------------

;;Procedure is set the rule's constraints
;;Print the question to the user
;;Bind the user's input to a variable which is then validated by the validation function which does not return until the user's input is valid
;; Assert the leaf's value and order slots to the validated variable and the global order variable accordingly
;; Increment the global order variable

(defrule Ask-Credit-History
    =>
    (printout t "How good is your credit history? Please answer with Unknown, Bad, or Good." crlf) 
    (bind ?cred (validate-History (read))) 
    (assert (credit-history (value ?cred) (Order ?*order*))) 
    (bind ?*order* (+ ?*order* 1)) ;; Increment the order 
)

(defrule Ask-Debt
    (or (credit-history (value Unknown)) (credit-history (value Good))) ;; if credit-history is Unknown or Good
    =>
    (printout t "High or Low Debt? (Low, High)" crlf)
    (bind ?d (validate-Debt (read)))
    (assert (debt (value ?d) (Order ?*order*)))
    (bind ?*order* (+ ?*order* 1)) ;; Increment the order
)

(defrule Ask-Income
    (and (collateral (value None)) (not (credit-history (value Bad))))
    =>
    (printout t "What is your income? (Low [<15k], Mid [15k-35k], High [>35k] )" crlf)
    (bind ?inc (validate-Income (read)))
    (assert (income (value ?inc) (Order ?*order*)))
    (bind ?*order* (+ ?*order* 1)) ;; Increment the order
)

(defrule Ask-Collateral
    (or
        (credit-history (value Bad))
        (and (credit-history (value Unknown)) (debt (value Low)))
        (and (credit-history (value Good)) (debt (value High)))
    )
    =>
    (printout t "Do you have collateral? (None, Adequate)" crlf)
    (bind ?coll (validate-Collateral (read)))
    (assert (collateral (value ?coll) (Order ?*order*)))
    (bind ?*order* (+ ?*order* 1)) ;; Increment the order
)

(deffunction explain ()
    (printout t crlf "This is because your: " crlf)
    (foreach ?f (find-all-facts ((?f credit-history debt income collateral)) TRUE)
        (printout t  
                  (fact-relation ?f)
                  "(" (fact-slot-value ?f Order) ")"
                  " was " (fact-slot-value ?f value) ", " crlf))

    (printout t "which match your classification's rule. " crlf)
)



;; Restart function which also resets the order counter
(deffunction restart ()
    (printout t "Would you like to perform another Credit Assessment? (Yes/No)" crlf)
    (bind ?response (read))
    (if (eq ?response Yes) 
        then
            (bind ?*order* 1) ;; Reset the order counter
            (reset)
            (run)
        else ;;any other response
            (printout t "Goodbye" crlf)
    )
)

;; Risk assessment rules
;;Only one rule will be triggered based on the user's input
;;Since the rules are mutually exclusive, the user's input will only match one rule
;;And the inference engine will only fire one rule

(defrule High-Risk
    (or
        (and (credit-history (value Unknown)) (debt (value High)))
        (and (collateral (value None)) (credit-history (value Bad)))
        (income (value Low))
    )
    =>
    (printout t "-> Your Credit Assessment is High Risk." crlf)
    (explain)
    (restart)
)

(defrule Moderate-Risk
    (or
        (and (credit-history (value Bad)) (collateral (value Adequate)))
        (income (value Mid))
    )
    =>
    (printout t "-> Your Credit Assessment is Moderate Risk." crlf)
    (explain)    
    (restart)
)

(defrule Low-Risk
    (or
        (and (credit-history (value Good)) (debt (value Low)))
        (and (collateral (value Adequate)) (not (credit-history (value Bad))))
        (income (value High))
    )
    =>
    (printout t "-> Your Credit Assessment is Low Risk." crlf)
    (explain)
    (restart)
)