;; Wizard's Adventure Game
;; From Barski, C. _Land of Lisp_, Chapter 5
;; Enhanced and translated to Scheme by Brian C. Ladd.
;;
;; A small text adventure game using Scheme commands as
;; the text commands. As ported, uses global variables
;; in the source code for the world and state.
;;
;; @programmer Brian C. Ladd
;; @date Spring 2022
;; @course CIS 443 Programming Languages

;; ==================================================================
;; Utility Functions (not game specific)
;; ==================================================================
;; get a value out of an a-list
;;
;; uses assq to find a match in an a-list and
;; cadr (second) to get the value associated with the key
;;
;; @param key the symbolic key of the element in the a-list
;; @param a-list the association list to search
;; @return the value associated with the key if it exists; #f otherwise
(define (a-list-value key a-list)
  (let ((v (assq key a-list)))
    (and v (cdr v))
    )
  )


;; ==================================================================
;; game-object
;; ==================================================================
;; game-object (critter, item) location handling functions. Since both
;; of these objects have a location field, held in the same way, the
;; accessor and the mutator for that field was factored out to here.

;; get game-object's location
;; @param game-object to get location for; critter or item
;; @return location value
(define (game-object-get-location game-object)
  (car (a-list-value 'location game-object)))

;; set game-object's location to given location
;; @param game-object to set location for; critter or item
;; @param new-location name of the new location
;; @return location value
(define (game-object-set-location! game-object new-location)
  (let* ((game-object-entry (assq 'location game-object)))
    (and game-object-entry
         (set-car! (cdr game-object-entry) new-location))
    ))


;; ==================================================================
;; location
;; ==================================================================
;; What is the new definition of a /location/?
;;
;; ((description (<description symbols>))
;;  (paths (<path triples>))
;; )
;; The association list contains the fields description (value is a
;; list of symbols) and paths (value is a list of path triples). New
;; fields can be added to the list by adding a pair with a name and a
;; value.

;; -----------------------------------------------------------------
;; path-triple
;; -----------------------------------------------------------------
;; A /path triple/ is an edge in the graph of locations in the
;; game. Each location has a paths value that lists connections to
;; other locations. The triple is (new-location-name direction portal)

(define (path-triple-location triple)
  (car triple))
(define (path-triple-direction triple)
  (cadr triple))
(define (path-triple-portal triple)
  (caddr triple))


(define (location-get-description location)
  (car (a-list-value 'description location)))
(define (location-get-paths location)
  (car (a-list-value 'paths location)))

;; Get the named location on the location list
;; @param props a-list of locations to search for named item
;; @param location-name to find
;; @return location object matching given name if found; #f otherwise
(define (world-find-location world location-name)
  (car (a-list-value location-name world)))

(define *world* '((living-room ((description (you are in the living-room.
                                                  a wizard is snoring loudly on the couch.))
                                (paths ((garden west door)
                                        (attic upstairs ladder)
					(chalet east door)))
                                ))
                  (garden ((description (you are in a beautiful garden.
                                             there is a well in front of you.))
                           (paths ((living-room east door)))
                           ))
                  (chalet ((description (you are in the chalet.
                                            there is Macbook on the desk.))
                          (paths ((living-room west door)))
                          ))
		   (attic ((description (you are in the attic.
                                            there is a giant welding torch in the corner.))
                           (paths ((living-room downstairs ladder)))
                           ))
                  ))

;; ==================================================================
;; item
;; ==================================================================
;; An item has a name (how it is stored in the props list)
;; and a location. The location is actually the name of another
;; game object. It could be a location, a critter, or, I suppose,
;; another item (container items then become possible).

(define item-get-location game-object-get-location)


;; Get the named item from the prop list
;; @param props a-list of items to search for named item
;; @param item-name to find
;; @return item object matching given name if found; #f otherwise
(define (props-find-item props item-name)
  (car (a-list-value item-name props)))

(define *props* '((whiskey ((location living-room)
                           ))
                  (bucket ((location living-room)
                           ))
                  (chain ((location attic)
                          ))
                  (frog ((location garden)
                         ))
                  (wand ((location wizard)
                          ))
                  (AirPods ((location chalet)
                          ))
                  (iPhone ((location player)
                          ))
                  ))


;; ==================================================================
;; critter
;; ==================================================================
;; Interface functions for critters (player, monsters in the game)
;; A critter:
;;   - is in a location (name of location)
;;   - has an inventory (list of names of items)
;;
;; interface functions all begin critter-
;;   first parameter is always something that is a critter; no
;;   error checking currently in place (is-critter? should be
;;   provided).
;;
;; Naming follows Scheme convention that mutators end with !

;; get the location of the given critter
;; @param critter - a critter object
;; @return critter's location
(define critter-get-location game-object-get-location)

;; get the inventory of the given critter
;; @param critter - a critter object
;; @return critter's inventory list
(define (critter-get-inventory critter)
  (car (a-list-value 'inventory critter)))

(define critter-set-location! game-object-set-location!)

;; critter takes named item into their inventory
;; @param critter - a critter object
;; @param item name to add to inventory
;; @return inventory list AFTER take completes
(define (critter-take! critter item)
  (let* ((inventory-item (assq 'inventory critter)))
    (set-car! (cdr inventory-item) (cons item (cadr inventory-item)))
    (cadr inventory-item)
    )
  )

;; critter puts [down] named item from their inventory
;; @param critter - a critter object
;; @param item name to remove from inventory
;; @return inventory list AFTER put completes
(define (critter-put! critter item)
  (let* ((inventory-item (assq 'inventory critter)))
    (set-car! (cdr inventory-item)
              (filter (lambda (list-item) (not (eq? list-item item))) (cadr inventory-item)))
    (cadr inventory-item)
    )
  )
;; ==================================================================
;; The cast of characters, player and otherwise
;; This is an a-list. But interface functions are provided so that
;; the user does not have to remember that.

;; Get the named critter from the cast
;; @param cast a-list of critters to search for named critter
;; @param critter-name of critter to find
;; @return critter object matching given name if found; #f otherwise
(define (cast-find-critter cast critter-name)
  (car (a-list-value critter-name cast)))

(define *cast*
  '((player ((location living-room)
             (inventory (iPhone))
             ))
    (wizard ((location living-room)
             (inventory (wand))
             ))
))
;; ==================================================================
;; Inventory
;; ==================================================================
;; Inventory own how many items? Given an if statement to measure if
;; it is in inventory. Then count loop until the last one.
;; @param critter item where to check how many objects in the inventory
;; @return inventory item number
(define (inventory critter item)
 (lambda (item) 
  (if (!item-exist)
   0
   (+ 1 (inventory (critter item))))))

;; Does inventory own the item? Given an object that searched in inventory.
;; @param inventory-item the inventory-item to check the item of inventory
;; @param critter item where to check if object exist
;; @return true if item exists; false otherwise.
(define (item-exist critter item) 
 (critter-get-inventory critter)
  (cond ( (null? critter-get-inventory critter) #f)
   ((equal? (car critter-get-inventory critter) inventory-item) #t)
   (else (item-exist (cdr critter-get-inventory critter) inventory-item))))

;; ==================================================================
;; Game Logic
;; ==================================================================

;; in-location? Given an object and a location name, is the object in
;; the given location.
;; @param game-object the game-object to check the location of
;; @param location-name where to check if object resides
;; @return true if game-object.location == location-name; false
;; otherwise.
(define (in-location? game-object location-name)
  (let ((game-object-location (game-object-get-location game-object)))
    (and game-object-location (eq? location-name game-object-location))
    ))

;; add a period symbol to the last symbol in a list. Makes symbol list read
;; like a human sentence. Recursive function returns a copy of the
;; list with the period appended to the last symbol.
;; @example (add-a-period '(the end)) => (the end.)
;; @param list-of-atoms the list to make look like a sentence
;; @return copy of list-of-atoms with last symbol extended with period
;; unless list was null. If it was null, return empty list.
(define (add-a-period list-of-atoms)
  (if (null? list-of-atoms)
      '()
      (if (null? (cdr list-of-atoms))
          (list (string->symbol (string-append (symbol->string (car list-of-atoms)) ".")))
          (cons (car list-of-atoms) (add-a-period (cdr list-of-atoms))))))

;; describe the objects in the given location.
;; @param location-name where objects must be to be described
;; @param props (name (object)) list of objects
;; @param where list of symbols to describe where the object "lies"
;; @return list of descriptions of all objects in props that are in
;; location name.
(define (describe-objects location-name props where)
  (let* ((where-it-is (add-a-period where))
         (describe-one-object (lambda (obj)
                                (append `(you see a ,(car obj)) where-it-is)))
         (value-from-a-list-pair (lambda (pair) (cadr pair)))
         (local-objects
          (filter (lambda (item)
                    (in-location? (value-from-a-list-pair item) location-name)) props))
       )
    (apply append (map describe-one-object local-objects))
    )
)

;; Describe one path building a list of the direction and portal
;;
;; Uses the back-tick operator (quasiquoting) with commas to evaluate
;; code inline.
;;
;; @param path-triple triple representing path to display.
;; @return list describing the path
(define (describe-path path-triple)
  `(there is a ,(path-triple-portal path-triple) going ,(path-triple-direction path-triple) from here.)
)

;; given a list of path triples, describe all of them
;;
;; Map describe-path across the list of triples; use append to jam all the
;; elements together.
;; @param paths list of path triples to describe
;; @return list of the descriptions of each path concatenated
(define (describe-paths paths)
  (apply append (map describe-path paths)))

;; given a location name, describe paths from it
;;
;; @param location-name symbolic name of the location
;; @param world a-list of world locations
;; @return list describing all the paths out of location
(define (describe-paths-from-location location-name world)
  (let* ((location (world-find-location world location-name))
         (outbound-paths (and location (location-get-paths location))))
    (and outbound-paths (describe-paths outbound-paths))
    ))

;; move the named critter to the named location
;;
;; set-cdr! is DESTRUCIVE; it modifies the location of the critter
;; by resetting the cdr field of the ACTUAL entry in the critter-location-pair
;; (assuming it is found)
;; @param critter-name name of critter to be moved
;; @param new-location-name where the critter should be after move
;; @param cast a-list of characters
;; @return new-location-name if move worked; #f otherwise
;; @note: *MODIFIES* cast parameter
(define (move-to-location! critter-name new-location-name cast)
  (let* ((critter (cast-find-critter cast critter-name))
         )
    (and critter
         (critter-set-location! critter new-location-name))
    )
  )

;; =================================================================
;; Current player interface: move! and look
;; =================================================================

;; move player from current, in the given direction, if possible
;;
;; @param symbolic name of the direction player wants to move
;; @param world a-list of all locations
;; @param cast a-list of chararcters (including player)
;; @param props a-list of all the things
;; @return name of new location if move worked; #f otherwise
;; @note: *MODIFIES* cast parameter
(define (move! direction world cast props)
  (let* ((player (cast-find-critter cast 'player))
         (location-name (and player (critter-get-location player)))
         (location (and location-name (world-find-location world location-name)))
         (outbound-paths (and location (location-get-paths location)))
         (path-triple (and outbound-paths
                         (find
                          (lambda (curr-triple) (eq? (path-triple-direction curr-triple) direction))
                          outbound-paths))))
    (and path-triple
       (move-to-location! 'player (path-triple-location path-triple) cast)
       (look world cast props))
  )
)

;; look at the player's current location
;;
;; 1. find 'player in the cast of characters; get critter structure
;; 2. find the location structure that corresponds to the player's 'location
;; 3. get the description of the location
;; 4  get the description of items in the location
;; 5. get the description of paths leaving the location
;;
;; append all the descriptions together
;;
;; @param world a-list of locations
;; @param cast a-list of the characters
;; @param props a-list of the items
;; @return list describing player's location or #f if there was a problem
(define (look world cast props)
  (let* ((player (cast-find-critter *cast* 'player))
         (location-name (and player (critter-get-location player)))
         (location (and location-name (world-find-location world location-name)))
         (desc (and location (location-get-description location)))
         (item-desc (and location (describe-objects location-name props '(on the floor))))
         (path-desc (and location (describe-paths-from-location location-name world)))
         )
    (and desc (append desc item-desc path-desc))
    )
)



;; How to "play"
;; 1 (user) => (look *world* *cast* *props*)
;; Value: (you are in the living-room. a wizard is snoring loudly on the couch. you see a whiskey on the floor. you see a bucket on the floor. there is a door going west from here. there is a ladder going upstairs from here.)
;; 1 (user) => (move! 'upstairs *world* *cast* *props*)
;; Value: (you are in the attic. there is a giant welding torch in the corner. you see a chain on the floor. there is a ladder going downstairs from here.)