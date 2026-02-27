;; GRAFI

;; HASH TABLES
(defparameter *graphs* (make-hash-table :test #'equal)) ;variabile globale che contiene tutti i grafi creati
(defparameter *vertices* (make-hash-table :test #'equal)) ;variabile globale che contiene tutti i vertici di tutti grafi
(defparameter *arcs* (make-hash-table :test #'equal)) ;variabile globale che contiene gli archi

;; FUNZIONI PER I GRAFI
(defun is-graph (graph-id) ;; helper, controlla se un grafo esiste
  (gethash graph-id *graphs*)) ;; se esiste ritorna graph-id, altrimenti NIL ;gethash cerca la chiave nella hash table

(defun new-graph (graph-id) ;; se il grafo non esiste, lo crea e lo inserisce in graphs
  (or (gethash graph-id *graphs*) ;; altrimenti ritorna graph-id
      (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id) ;: elimina il grafo
  (let ((vertex-keys '())
        (arc-keys '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (and (equal (first key) 'vertex)
                          (equal (second key) graph-id))
                 (push key vertex-keys)))
             *vertices*)
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (and (equal (first key) 'arc)
                          (equal (second key) graph-id))
                 (push key arc-keys)))
             *arcs*)
    (mapc (lambda (k) (remhash k *vertices*)) vertex-keys)
    (mapc (lambda (k) (remhash k *arcs*)) arc-keys))
  (remhash graph-id *graphs*)
  nil)

(defun new-vertex (graph-id vertex-id)
  ;; questa versione crea il grafo se non esiste, ovvero non lancia nessun errore
  (new-graph graph-id)
  (or (gethash (list 'vertex graph-id vertex-id) *vertices*)
      (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
            (list 'vertex graph-id vertex-id))))

(defun graph-vertices (graph-id) ;ritorna tutti i vertici del grafo
  (let (result)
    (maphash
     (lambda (key value)
       (when (and (equal (first key) 'vertex)
                  (equal (second key) graph-id))
         (push value result)))
     *vertices*)
    result))

(defun new-arc (graph-id u v &optional (weight 1))
  ;; crea il grafo e i vertici se non esistono
  (new-vertex graph-id u)
  (new-vertex graph-id v)
  (setf (gethash (list 'arc graph-id u v) *arcs*)
        (list 'arc graph-id u v weight)))

(defun graph-arcs (graph-id) ;; ritorna tutti gli archi del grafo 
  (let ((result '()))
    (maphash
     (lambda (key value)
       (when (and (equal (first key) 'arc)
                  (equal (second key) graph-id))
         (push value result)))
     *arcs*)
    result))

(defun graph-vertex-neighbors (graph-id vertex-id) ;; restituisce tutti gli archi uscenti da un vertice
  (let ((result '()))
    (maphash
     (lambda (key value)
       (when (and (equal (first key) 'arc)
                  (equal (second key) graph-id)
                  (equal (third key) vertex-id))
         (push value result)))
     *arcs*)
    result))

;; STAMPA
(defun graph-print (graph-id) ;; stampa il grafo
  (format t "~%Graph: ~S~%" graph-id)
  (format t "Vertices: ~S~%" (graph-vertices graph-id))
  (format t "Arcs: ~S~%" (graph-arcs graph-id))
  t) ;; stampa di T per corretta esecuzione

;;===============================================================================================================
;; HEAP

(defparameter *heaps* (make-hash-table :test #'equal))

(defstruct (heap-rep (:constructor make-heap-rep (id size actual)))
  id
  size
  actual)

(defun new-heap (heap-id &optional (initial-capacity 42))
;; inserisce un nuovo heap nella hash-table *heaps*
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (make-heap-rep
             heap-id
             0
             (make-array initial-capacity
                         :initial-element nil
                         :adjustable t)))))

(defun heap-delete (heap-id)
;; elimina l'heap indicato dalla hash-table *heaps*
  (remhash heap-id *heaps*)
  t)

(defun heap-empty (heap-id)
;; verifica se uno heap è vuoto
  (let ((h (gethash heap-id *heaps*)))
    (or (null h)
        (= (heap-rep-size h) 0))))

(defun heap-not-empty (heap-id)
;; verifica se uno heap non è vuoto, ovvero che contenga almeno un elemento
  (not (heap-empty heap-id)))

(defun heap-head (heap-id)
;; ritorna la lista (K V) con la chiave minima
  (let ((h (gethash heap-id *heaps*)))
    (if (or (null h) (= (heap-rep-size h) 0))
        nil
      (aref (heap-rep-actual h) 0))))

(defun heap-parent-index (i)
  (floor (- i 1) 2))

(defun heap-left-index (i)
  (+ (* 2 i) 1))

(defun heap-right-index (i)
  (+ (* 2 i) 2))

(defun heap-bubble-up (arr i)
;; porta l'elemento in pos i verso l'alto finché la heap-property è soddisfatta.
  (if (<= i 0)
      nil
    (let* ((parent (heap-parent-index i))
           (ki (first (aref arr i)))
           (kp (first (aref arr parent))))
      (when (< ki kp)
        (let ((tmp (aref arr i)))
          (setf (aref arr i) (aref arr parent))
          (setf (aref arr parent) tmp))
        (heap-bubble-up arr parent)))))

(defun heap-bubble-down (arr size i)
;; porta l'elemento in pos i verso il basso finché la heap-property è soddisfatta
  (let* ((left  (heap-left-index i))
         (right (heap-right-index i))
         (smallest i))
    (let ((smallest
           (if (and (< left size)
                    (< (first (aref arr left))
                       (first (aref arr smallest))))
               left
               smallest)))
      (let ((smallest
             (if (and (< right size)
                      (< (first (aref arr right))
                         (first (aref arr smallest))))
                 right
                 smallest)))
        (when (not (= smallest i))
          (let ((tmp (aref arr i)))
            (setf (aref arr i) (aref arr smallest))
            (setf (aref arr smallest) tmp))
          (heap-bubble-down arr size smallest))))))

(defun heap-insert (heap-id k v)
;; inserisce (K V) nello heap ed allarga l'array se serve
  (let ((h (gethash heap-id *heaps*)))
    (if (null h)
        (error "heap-insert: heap ~A non esiste." heap-id)
        (let* ((size (heap-rep-size h))
               (arr  (heap-rep-actual h))
               (cap  (array-total-size arr)))
          ;; Resize se necessario
          (when (>= size cap)
            (let ((new-cap (* 2 cap)))
              (setf arr (adjust-array arr new-cap :initial-element nil))
              (setf (heap-rep-actual h) arr)))
          ;; Inserisci in fondo
          (setf (aref arr size) (list k v))
          (setf (heap-rep-size h) (+ size 1))
          ;; Bubble up
          (heap-bubble-up arr size)
          t))))

(defun heap-extract (heap-id)
;; estrae e ritorna la coppia (K V) con K minima.
  (let ((h (gethash heap-id *heaps*)))
    (cond
      ((null h)
       (error "heap-extract: heap ~A non esiste." heap-id))
      ((= (heap-rep-size h) 0)
       (error "heap-extract: heap ~A e' vuoto." heap-id))
      (t
       (let* ((arr  (heap-rep-actual h))
              (size (heap-rep-size h))
              (min  (aref arr 0))
              (last (aref arr (- size 1))))
         ;; Metti l'ultimo in cima
         (setf (aref arr 0) last)
         (setf (aref arr (- size 1)) nil)
         (setf (heap-rep-size h) (- size 1))
         ;; Bubble down
         (heap-bubble-down arr (- size 1) 0)
         min)))))

(defun heap-find-index (arr size k v index)
  (cond
    ((>= index size) nil)
    ((and (equal (first (aref arr index)) k)
          (equal (second (aref arr index)) v))
     index)
    (t (heap-find-index arr size k v (+ index 1)))))

(defun heap-modify-key (heap-id new-key old-key v)
;; 
  (let ((h (gethash heap-id *heaps*)))
    (if (null h)
        (error "heap-modify-key: heap ~A non esiste." heap-id)
        (let* ((arr  (heap-rep-actual h))
               (size (heap-rep-size h))
               (idx  (heap-find-index arr size old-key v 0)))
          (if (null idx)
              (error "heap-modify-key: old-key ~A con valore ~A non trovata nello heap ~A."
                     old-key v heap-id)
              (progn
                (setf (aref arr idx) (list new-key v))
                (if (< new-key old-key)
                    (heap-bubble-up arr idx)
                    (heap-bubble-down arr size idx))
                t))))))

(defun heap-print-elements (arr size index)
  (when (< index size)
    (format t "  [~A] -> ~A~%" index (aref arr index))
    (heap-print-elements arr size (+ index 1))))

(defun heap-print (heap-id)
  (let ((h (gethash heap-id *heaps*)))
    (if (null h)
        (error "heap-print: heap ~A non esiste." heap-id)
        (progn
          (format t "Heap ID: ~A~%" (heap-rep-id h))
          (format t "Size: ~A~%" (heap-rep-size h))
          (format t "Capacity: ~A~%" (array-total-size (heap-rep-actual h)))
          (format t "Elementi:~%")
          (heap-print-elements (heap-rep-actual h) (heap-rep-size h) 0)
          t))))

;;==================================================================================================================
;; DIJKSTRA

(defparameter *distances* (make-hash-table :test 'equal)
  "Associa (graph-id vertex-id) -> distanza minima dalla sorgente.")

(defparameter *visited* (make-hash-table :test 'equal)
  "Associa (graph-id vertex-id) -> T se il vertice e' stato visitato.")

(defparameter *previous* (make-hash-table :test 'equal)
  "Associa (graph-id vertex-id) -> vertice precedente nel cammino minimo.")

(defun sssp-dist (graph-id vertex-id)
  "Ritorna la distanza minima di vertex-id dalla sorgente nel grafo graph-id.
   Ritorna NIL se il vertice non e' raggiungibile o non e' stato inizializzato."
  (gethash (list graph-id vertex-id) *distances*))

(defun sssp-visited (graph-id vertex-id)
  "Ritorna T se vertex-id e' stato visitato durante Dijkstra in graph-id."
  (gethash (list graph-id vertex-id) *visited*))

(defun sssp-previous (graph-id vertex-id)
  "Ritorna il vertice precedente a vertex-id nel cammino minimo dalla sorgente."
  (gethash (list graph-id vertex-id) *previous*))

(defun sssp-change-dist (graph-id vertex-id new-dist)
  "Imposta la distanza di vertex-id a new-dist nella hash-table *distances*."
  (setf (gethash (list graph-id vertex-id) *distances*) new-dist)
  nil)

(defun sssp-change-previous (graph-id vertex-id predecessor)
  "Imposta il predecessore di vertex-id a predecessor nella hash-table *previous*."
  (setf (gethash (list graph-id vertex-id) *previous*) predecessor)
  nil)

(defun sssp-change-visited (graph-id vertex-id)
  "Marca vertex-id come visitato nella hash-table *visited*."
  (setf (gethash (list graph-id vertex-id) *visited*) t)
  nil)

(defun sssp-init (graph-id source)
  "Inizializza le hash-table per Dijkstra:
   - distanza infinita per tutti i vertici (tranne source = 0)
   - nessun predecessore
   - nessun vertice visitato"
  (let ((vertices (graph-vertices graph-id)))
    (sssp-init-vertices graph-id source vertices)))

(defun sssp-init-vertices (graph-id source vertices)
  "Helper ricorsivo per inizializzare tutti i vertici."
  (if (null vertices)
      nil
      (let ((v (third (car vertices)))) ; (vertex graph-id vertex-id) -> vertex-id
        (if (equal v source)
            (progn
              (sssp-change-dist graph-id v 0)
              (sssp-init-vertices graph-id source (cdr vertices)))
            (progn
              (sssp-change-dist graph-id v most-positive-fixnum)
              (sssp-init-vertices graph-id source (cdr vertices)))))))

(defun sssp-dijkstra (graph-id source)
  "Esegue l'algoritmo di Dijkstra su graph-id a partire da source.
   Popola *distances*, *visited* e *previous*.
   Ritorna NIL."
  ;; Assicurarsi che la sorgente esista nel grafo
  (new-vertex graph-id source)
  ;; Inizializzazione
  (sssp-init graph-id source)
  ;; Costruzione heap iniziale con tutti i vertici
  (let ((heap-id (gensym "DIJKSTRA-HEAP-")))
    (new-heap heap-id 10)
    (sssp-populate-heap graph-id heap-id (graph-vertices graph-id))
    (sssp-main-loop graph-id heap-id)
    (heap-delete heap-id))
  nil)

(defun sssp-populate-heap (graph-id heap-id vertices)
  "Inserisce tutti i vertici nello heap con la loro distanza iniziale."
  (if (null vertices)
      nil
      (let* ((v-rep (car vertices))
             (v (third v-rep))   ; vertex-id
             (d (sssp-dist graph-id v)))
        (heap-insert heap-id d v)
        (sssp-populate-heap graph-id heap-id (cdr vertices)))))

(defun sssp-main-loop (graph-id heap-id)
  "Ciclo principale di Dijkstra: estrae il minimo e rilassa i vicini."
  (if (heap-empty heap-id)
      nil
      (let* ((min-pair (heap-extract heap-id))
             (d (first min-pair))
             (u (second min-pair)))
        (if (sssp-visited graph-id u)
            ;; Vertice gia' visitato: ignora (elemento obsoleto nello heap)
            (sssp-main-loop graph-id heap-id)
            (progn
              (sssp-change-visited graph-id u)
              ;; Solo se la distanza corrente e' finita (raggiungibile)
              (when (< d most-positive-fixnum)
                (sssp-relax-neighbors graph-id heap-id u
                                      (graph-vertex-neighbors graph-id u)))
              (sssp-main-loop graph-id heap-id))))))

(defun sssp-relax-neighbors (graph-id heap-id u neighbors)
  "Rilassa tutti gli archi uscenti da u verso i suoi vicini."
  (if (null neighbors)
      nil
      (let* ((arc (car neighbors))
             ;; arc = (arc graph-id u v weight)
             (v (fourth arc))
             (w (fifth arc))
             (d-u (sssp-dist graph-id u))
             (d-v (sssp-dist graph-id v))
             (new-dist (+ d-u w)))
        (when (and (not (sssp-visited graph-id v))
                   (< new-dist d-v))
          ;; Aggiornamento distanza e predecessore
          (sssp-change-dist graph-id v new-dist)
          (sssp-change-previous graph-id v u)
          ;; Inserisce il vertice nello heap con la nuova distanza
          ;; (lazy deletion: elementi obsoleti vengono ignorati nel ciclo principale)
          (heap-insert heap-id new-dist v))
        (sssp-relax-neighbors graph-id heap-id u (cdr neighbors)))))

(defun sssp-shortest-path (graph-id source vertex-id)
  "Ritorna la lista di archi che rappresenta il cammino minimo
   da source a vertex-id in graph-id.
   La lista ha la forma:
     ((arc G source N1 W1) (arc G N1 N2 W2) ... (arc G Nk vertex-id Wk))
   Ritorna NIL se vertex-id non e' raggiungibile da source."
  (if (equal source vertex-id)
      nil
      (let ((path (sssp-build-path graph-id source vertex-id nil)))
        path)))

(defun sssp-build-path (graph-id source current acc)
  "Helper ricorsivo per ricostruire il cammino minimo seguendo i predecessori."
  (if (equal current source)
      acc
      (let ((prev (sssp-previous graph-id current)))
        (if (null prev)
            ;; Nodo non raggiungibile
            nil
            (let* ((w (sssp-arc-weight graph-id prev current))
                   (arc (list 'arc graph-id prev current w)))
              (sssp-build-path graph-id source prev (cons arc acc)))))))

(defun sssp-arc-weight (graph-id u v)
  "Ritorna il peso dell'arco da u a v nel grafo graph-id."
  (let ((neighbors (graph-vertex-neighbors graph-id u)))
    (sssp-find-weight v neighbors)))

(defun sssp-find-weight (v neighbors)
  "Trova il peso dell'arco verso v nella lista di archi neighbors."
  (if (null neighbors)
      1  ; default weight, non dovrebbe accadere
      (let ((arc (car neighbors)))
        ;; arc = (arc graph-id u v weight)
        (if (equal (fourth arc) v)
            (fifth arc)
            (sssp-find-weight v (cdr neighbors))))))