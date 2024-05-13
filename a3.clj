
; Written by: Afra Azreen

(ns hello-world.core

 ; Indicates that this namespace will generate a Java class

  (:gen-class)

  ; Importing the clojure.string namespace and aliasing it as 'str'
  ; Importing the clojure.java.io namespace and aliasing it as 'io'
  (:require [clojure.string :as str]
            [clojure.java.io :as io])

   ; Importing the java.io.File class
   ; Importing the java.util.regex.Pattern class
  (:import [java.io File])
  (:import [java.util.regex Pattern]))

; Here, we define a function called read-events that takes a file-path parameter
(defn read-events [file-path]

  ; Open the file specified by file-path for reading
  (with-open [rdr (io/reader file-path)]

    ; Read all lines from the file and store them in 'lines'
    (let [lines (doall (line-seq rdr))
          events (partition 5 lines)] ; Partition 'lines' into groups of 5 lines each, forming events
     
      ; Map over each event block
      (vec (map (fn [block]

      ; Construct a map from the event block, with keys :date, :location, :type, and :significance
                  (zipmap [:date :location :type :significance] block))
                events)))))

                ; Return a vector containing the maps representing each event


; Here, we define a function called display-events that takes a parameter 'events', which is a sequence of event maps
(defn display-events [events]

  ; Print the total number of events found
  (println "\nTotal events found:" (count events))

  ; Print an empty line for formatting
  (println "")

  ; Iterate over each event in the 'events' sequence
  (doseq [event events]

  ; Print the date of the current event, the location, the type and the significance of the event. 
    (println (:date event))
    (println (:location event))
    (println (:type event))
    (println (:significance event))
    (println "----------------------------------------------------------------------------------------")))


; Here, define a function called add-event that takes a file-path parameter
(defn add-event [file-path]

; Prompt the user to enter the date
  (print "Enter date:") (flush)

  ; Read the date input from the user and format it
  (let [date (str "Date: " (read-line) "\n")

        ; Prompt the user to enter the location
        location (str "Location: " (do (print "Enter location:") (flush) (read-line)) "\n")

        ; Prompt the user to enter the type
        type (str "Type: " (do (print "Enter type:") (flush) (read-line)) "\n")

        ; Prompt the user to enter the significance
        significance (str "Significance: " (do (print "Enter significance:") (flush) (read-line)) "\n\n")]

    ; Append the formatted event information to the file specified by file-path
 ; Print a success message
    (spit file-path (str date location type significance) :append true)
    (println "Event added successfully.")))

; Here, we are just listing the events.
(defn list-eclipse-events [events]
  (println "")

  (doseq [[index event] (map-indexed vector events)]
    ; Iterate over each indexed event in the 'events' sequence

    ; Print the index of the event, location, type, significance. 
    (println (str "Event Index: " (inc index)))
    (println (:date event))
    (println (:location event))
    (println (:type event))
    (println (:significance event))
    (println "----------------------------------------------------------------------------------------")))
   
    ; Print a line of dashes as a separator after each event



; Define a function called update-eclipse-event that takes 'events' (a vector of events) and an 'index' as parameters
(defn update-eclipse-event [events index]

  ; Print the index of the event being modified
 ; Adjust index to be zero-based
  (println (str "\nModifying Event at Index: " index))
  (let [index (dec index)

        ; Retrieve the event to be updated from 'events' based on the provided index
        event-to-update (nth events index)

        ; Define a helper function to extract value from event attributes
        get-value (fn [s] (second (str/split s #":\s?")))

        ; Extract current date, location, type, significance from the event to be updated
        current-date (get-value (:date event-to-update))
        current-location (get-value (:location event-to-update))
        current-type (get-value (:type event-to-update))
        current-significance (get-value (:significance event-to-update))

        ; Prompt user for updated date, location, type, significance.
        new-date (do (print (str "Enter updated date [" current-date "]: ")) (flush) (read-line))
        new-location (do (print (str "Enter updated location [" current-location "]: ")) (flush) (read-line))
        new-type (do (print (str "Enter updated type [" current-type "]: ")) (flush) (read-line))
        new-significance (do (print (str "Enter updated significance [" current-significance "]: ")) (flush) (read-line))
        
        ; Create updated event by associating updated values
        updated-event (assoc event-to-update
                             :date (if (empty? new-date) (:date event-to-update) (str "Date: " new-date))
                             :location (if (empty? new-location) (:location event-to-update) (str "Location: " new-location))
                             :type (if (empty? new-type) (:type event-to-update) (str "Type: " new-type))
                             :significance (if (empty? new-significance) (:significance event-to-update) (str "Significance: " new-significance)))]
    
    (println "Event updated successfully.") ; Print success message
    (assoc events index updated-event))) ; Replace the event at the specified index in 'events' with the updated event


; Here, we define a function called modify-event that takes 'file-path' (path to file) and 'events' (a vector of events) as parameters.
(defn modify-event [file-path events]

   ; List all events
  (list-eclipse-events events)

  ; Print an empty line for formatting
  (println "")

  ; Prompt the user to enter the index of the event to be modified
  (print "Enter the index of the event you wish to modify: ") (flush)

  ; Read the index input from the user and convert it to an integer
  (let [index (Integer/parseInt (read-line))
        updated-events (update-eclipse-event events index)]  ; Update the event at the specified index
    (with-open [writer (io/writer file-path)] ; Open the file specified by 'file-path' for writing
      (doseq [event updated-events] ; Iterate over each updated event
           (.write writer (str (:date event) "\n" (:location event) "\n" (:type event) "\n" (:significance event) "\n\n"))))
        updated-events)) ; Return the vector of updated events

; Here, we define a function called search-eclipse-events that takes 'events' (a vector of events) as a parameter
(defn search-eclipse-events [events]

 ; Prompt the user to enter the search type
  (print "Enter search type (date/location): ") (flush)

  ; Read the search type input from the user and convert it to lowercase
  (let [search-type (str/lower-case (read-line))

        ; Prompt the user to enter the search query based on the search type
        ; Determine the search field based on the search type
        query-prompt (if (= "date" search-type) "Enter search query for date: " "Enter search query for location: ")
        search-field (if (= "date" search-type) :date :location)]

     ; Prompt the user to enter the search query 
    (print query-prompt) (flush)

    ; Read the search query input from the user and convert it to lowercase
    (let [search-query (str/lower-case (read-line))

        ; Create a regex pattern for case-insensitive search
          regex-pattern (re-pattern (str ".*" (Pattern/quote search-query) ".*"))

          ; Filter events based on the search criteria
          matching-events (filter (fn [event]
                                    (re-find regex-pattern
                                             (str/lower-case (event search-field))))
                                  events)]
       
       ; Check if any events match the search criteria                          
      (if (empty? matching-events)
        (println "No matching events found.") ; Display if none found.
        (display-events matching-events))))) ; Display the matching events if found

; Function to display the main menu of the program
(defn display-menu []
  (println "\n=== Eclipse History Encyclopedia ===")
  (println "1. View Eclipse Events")
  (println "2. Add New Eclipse Event")
  (println "3. Modify Eclipse Event")
  (println "4. Search for Eclipse Events")
  (println "5. Exit")
  (print "\nEnter your choice (1-5): ") (flush))

; Function to run the main loop of the program
(defn main-loop [file-path]

  ; Initialize events atom with events read from file
  (let [events (atom (read-events file-path))]

    ; Display the main menu
    (loop []
      (display-menu)

      ; Read user's choice
      (let [choice (Integer/parseInt (read-line))]
        (cond
          (= choice 1) (display-events @events)

          ; Update events atom after adding event
          (= choice 2) (do (add-event file-path)
                           (reset! events (read-events file-path)))

           ; Update events atom after modifying event                
          (= choice 3) (do (modify-event file-path @events)
                           (reset! events (read-events file-path)))
          (= choice 4) (search-eclipse-events @events)
          (= choice 5) (do (println "Exiting...") (System/exit 0))

          ; Handle invalid choices
          :else (println "Invalid choice, please enter a number from 1 to 5")))
      (recur))))

(defn -main [& args]

; Entry point of the program
; Set file path for events data file
; Start the main loop of the program with the specified file path
  (let [file-path (if (empty? args) "eclipse_events.txt" (first args))]
    (println "Starting Eclipse History Encyclopedia...")
    (main-loop file-path)))

; Call -main function to start the program
(-main)
