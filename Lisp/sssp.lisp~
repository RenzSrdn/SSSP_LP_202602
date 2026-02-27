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
             
;; VERTICI
;(defun new-vertex (graph-id vertex-id) ;aggiunge un vertice
;  (unless (is-graph graph-id)
;    (error "Graph ~S does not exist." graph-id)) ;controlla se il grafo non esiste, se non esiste ritorna errore
;  (setf (gethash (list 'vertex graph-id vertex-id)
;                 *vertices*)
;        (list 'vertex graph-id vertex-id))) ;altrimenti, inserisce il vertice

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

;ARCHI
;(defun new-arc (graph-id u v &optional (weight 1))
;; questa funzione aggiunge un arco senza creare vertici 
;  (unless (is-graph graph-id)
;    (error "Graph ~S does not exist." graph-id))
;  (setf (gethash (list 'arc graph-id u v)
;                 *arcs*)
;        (list 'arc graph-id u v weight)))

(defun new-arc (graph-id u v &optional (weight 1))
  ;; crea il grafo e i vertici se non esistono
  (new-vertex graph-id u)
  (new-vertex graph-id v)
  (setf (gethash (list 'arc graph-id u v) *arcs*)
        (list 'arc graph-id u v weight)))

(defun graph-arcs (graph-id) ;ritorna tutti gli archi del grafo 
  (let ((result '()))
    (maphash
     (lambda (key value)
       (when (and (equal (first key) 'arc)
                  (equal (second key) graph-id))
         (push value result)))
     *arcs*)
    result))

(defun graph-vertex-neighbors (graph-id vertex-id) ;restituisce tutti gli archi uscenti da un vertice
  (let ((result '()))
    (maphash
     (lambda (key value)
       (when (and (equal (first key) 'arc)
                  (equal (second key) graph-id)
                  (equal (third key) vertex-id))
         (push value result)))
     *arcs*)
    result))

;STAMPA
(defun graph-print (graph-id) ;stampa il grafo
  (format t "~%Graph: ~S~%" graph-id)
  (format t "Vertices: ~S~%" (graph-vertices graph-id))
  (format t "Arcs: ~S~%" (graph-arcs graph-id))
  t) ;stampa di T per corretta esecuzione

;ciao