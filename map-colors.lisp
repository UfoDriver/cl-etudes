;;; https://en.wikipedia.org/wiki/United_States_states#/media/File:Map_of_USA_with_state_names_2.svg


(defpackage :cl-etudes.map-colors
  (:use :common-lisp))

(in-package :cl-etudes.map-colors)

(defparameter *states*
  '((AL "Alabama" "Montgomery")
    (AK "Alaska" "Juneau")
    (AZ "Arizona" "Phoenix")
    (AR "Arkansas" "Little Rock")
    (CA "California" "Sacramento")
    (CO "Colorado" "Denver")
    (CT "Connecticut" "Hartford")
    (DE "Delaware" "Dover")
    (FL "Florida" "Tallahassee")
    (GA "Georgia" "Atlanta")
    (HI "Hawaii" "Honolulu")
    (ID "Idaho" "Boise")
    (IL "Illinois" "Springfield")
    (IN "Indiana" "Indianapolis")
    (IA "Iowa" "Des Moines")
    (KS "Kansas" "Topeka")
    (KY "Kentucky" "Frankfort")
    (LA "Louisiana" "Baton Rouge")
    (ME "Maine" "Augusta")
    (MD "Maryland" "Annapolis")
    (MA "Massachusetts" "Boston")
    (MI "Michigan" "Lansing")
    (MN "Minnesota" "Saint Paul")
    (MS "Mississippi" "Jackson")
    (MO "Missouri" "Jefferson City")
    (MT "Montana" "Helena")
    (NE "Nebraska" "Lincoln")
    (NV "Nevada" "Carson City")
    (NH "New Hampshire" "Concord")
    (NJ "New Jersey" "Trenton")
    (NM "New Mexico" "Santa Fe")
    (NY "New York" "Albany")
    (NC "North Carolina" "Raleigh")
    (ND "North Dakota" "Bismarck")
    (OH "Ohio" "Columbus")
    (OK "Oklahoma" "Oklahoma City")
    (OR "Oregon" "Salem")
    (PA "Pennsylvania" "Harrisburg")
    (RI "Rhode Island" "Providence")
    (SC "South Carolina" "Columbia")
    (SD "South Dakota" "Pierre")
    (TN "Tennessee" "Nashville")
    (TX "Texas" "Austin")
    (UT "Utah" "Salt Lake City")
    (VT "Vermont" "Montpelier")
    (VA "Virginia" "Richmond")
    (WA "Washington" "Olympia")
    (WV "West Virginia" "Charleston")
    (WI "Wisconsin" "Madison")
    (WY "Wyoming" "Cheyenne")))

(defparameter *state-borders*
  '(("Alabama" ("Florida" "Georgia" "Mississippi" "Tennessee"))
    ("Alaska" nil)
    ("Arizona" ("California" "Colorado" "Nevada" "New Mexico" "Utah"))
    ("Arkansas" ("Louisiana" "Mississippi" "Missouri" "Oklahoma" "Tennessee" "Texas"))
    ("California" ("Arizona" "Nevada" "Oregon"))
    ("Colorado" ("Arizona" "Kansas" "Nebraska" "New Mexico" "Oklahoma" "Utah" "Wyoming"))
    ("Connecticut" ("Massachusetts" "New York" "Rhode Island"))
    ("Delaware" ("Maryland" "New Jersey" "Pennsylvania"))
    ("Florida" ("Alabama" "Georgia"))
    ("Georgia" ("Alabama" "Florida" "North Carolina" "South Carolina" "Tennessee"))
    ("Hawaii" nil)
    ("Idaho" ("Montana" "Nevada" "Oregon" "Utah" "Washington" "Wyoming"))
    ("Illinois" ("Indiana" "Iowa" "Michigan" "Kentucky" "Missouri" "Wisconsin"))
    ("Indiana" ("Illinois" "Kentucky" "Michigan" "Ohio"))
    ("Iowa" ("Illinois" "Minnesota" "Missouri" "Nebraska" "South Dakota" "Wisconsin"))
    ("Kansas" ("Colorado" "Missouri" "Nebraska" "Oklahoma"))
    ("Kentucky" ("Illinois" "Indiana" "Missouri" "Ohio" "Tennessee" "Virginia" "West Virginia"))
    ("Louisiana" ("Arkansas" "Mississippi" "Texas"))
    ("Maine" ("New Hampshire"))
    ("Maryland" ("Delaware" "Pennsylvania" "Virginia" "West Virginia"))
    ("Massachusetts" ("Connecticut" "New Hampshire" "New York" "Rhode Island" "Vermont"))
    ("Michigan" ("Illinois" "Indiana" "Minnesota" "Ohio" "Wisconsin"))
    ("Minnesota" ("Iowa" "Michigan" "North Dakota" "South Dakota" "Wisconsin"))
    ("Mississippi" ("Alabama" "Arkanssas" "Louisiana" "Tennessee"))
    ("Missouri" ("Arkansas" "Illinois" "Iowa" "Kansas" "Kentucky" "Nebraska" "Oklahoma" "Tennessee"))
    ("Montana" ("Idaho" "North Dakota" "South Dakota" "Wyoming"))
    ("Nebraska" ("Colorado" "Iowa" "Kansas" "Missouri" "South Dakota" "Wyoming"))
    ("Nevada" ("Arizona" "California" "Idaho" "Oregon" "Utah"))
    ("New Hampshire" ("Maine" "Massachusetts" "Vermont"))
    ("New Jersey" ("Delaware" "New York" "Pennsylvania"))
    ("New Mexico" ("Arizona" "Colorado" "Oklahoma" "Texas" "Utah"))
    ("New York" ("Connecticut" "Massachusetts" "New Jersey" "Pennsylvania" "Rhode Island" "Vermont"))
    ("North Carolina" ("Georgia" "South Carolina" "Tennessee" "Virginia"))
    ("North Dakota" ("Minnesota" "Montana" "South Dakota"))
    ("Ohio" ("Indiana" "Kentucky" "Michigan" "Pennsylvania" "West Virginia"))
    ("Oklahoma" ("Arkansas" "Colorado" "Kansas" "Missouri" "New Mexico" "Texas"))
    ("Oregon" ("California" "Idaho" "Nevada" "Washington"))
    ("Pennsylvania" ("Delaware" "Maryland" "New Jersey" "New York" "Ohio" "West Virginia"))
    ("Rhode Island" ("Connecticut" "Massachusetts" "New York"))
    ("South Carolina" ("Georgia" "North Carolina"))
    ("South Dakota" ("Iowa" "Minnesota" "Montana" "Nebraska" "North Dakota" "Wyoming"))
    ("Tennessee" ("Alabama" "Arkansas" "Georgia" "Kentucky" "Mississippi" "Missouri" "North Carolina" "Virginia"))
    ("Texas" ("Arkansas" "Louisiana" "New Mexico" "Oklahoma"))
    ("Utah" ("Arizona" "Colorado" "Idaho" "Nevada" "New Mexico" "Wyoming"))
    ("Vermont" ("Massachusetts" "New Hampshire" "New York"))
    ("Virginia" ("Kentucky" "Maryland" "North Carolina" "Tennessee" "West Virginia"))
    ("Washington" ("Idaho" "Oregon"))
    ("West Virginia" ("Kentucky" "Maryland" "Ohio" "Pennsylvania" "Virginia"))
    ("Wisconsin" ("Illinois" "Iowa" "Michigan" "Minnesota"))
    ("Wyoming" ("Colorado" "Idaho" "Montana" "Nebraska" "South Dakota" "Utah"))))

(defparameter *state-borders-count*
  '(
    (("Missouri" "Tennessee") 8)
    (("Colorado" "Kentucky") 7)
    (("Arkansas" "Idaho" "Illinois" "Iowa" "Nebraska" "New York" "Oklahoma" "Pennsylvania" "South Dakota" "Utah" "Wyoming") 6)
    (("Arizona" "Georgia" "Massachusetts" "Michigan" "Minnesota" "Nevada" "New Mexico" "Ohio" "Virginia" "West Virginia") 5)
    (("Alabama" "Indiana" "Kansas" "Maryland" "Mississippi" "Montana" "North Carolina" "Oregon" "Texas" "Wisconsin") 4)
    (("California" "Connecticut" "Delaware" "Louisiana" "New Hampshire" "New Jersey" "North Dakota" "Rhode Island" "Vermont") 3)
    (("Florida" "South Carolina" "Washington") 2)
    (("Maine") 1)
    (("Alaska" "Hawaii") 0)))

(defun state-name (state-code)
  (second (assoc state-code *states*)))

(defun state-code (state-name)
  (first (find state-name *states* :test #'string= :key #'second)))

(defun state-capital (state-code)
  (third (assoc state-code *states*)))

(defun state-bordered-with (state-code)
  (let ((state-name (state-name state-code)))
    (second (assoc state-name *state-borders* :test #'string=))))

(defun state-bordered-with-count (state-code)
  (length (state-bordered-with state-code)))

(defun state-bordered-with-count-answer (state-code)
  (let ((state-name (state-name state-code)))
    (dolist (data-row *state-borders-count*)
      (destructuring-bind (states count) data-row
          (when (member state-name states :test #'string=)
              (return count))))))
