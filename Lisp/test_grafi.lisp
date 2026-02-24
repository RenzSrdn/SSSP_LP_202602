;;; Carica il codice principale
(load "sssp.lisp")

;;; Reset
(clrhash *graphs*)
(clrhash *vertices*)
(clrhash *arcs*)

;;; Test con assert
(new-graph 'g1)

(assert (equal (new-graph 'g1) 'g1))
(assert (is-graph 'g1))
(assert (null (is-graph 'g-inesistente)))

(new-vertex 'g1 'a)
(new-vertex 'g1 'b)
(assert (= (length (graph-vertices 'g1)) 2))

(new-arc 'g1 'a 'b 3.5)
(assert (equal (new-arc 'g1 'a 'b 3.5) '(arc g1 a b 3.5)))

;;; sostituzione peso
(new-arc 'g1 'a 'b 99)
(assert (null (member '(arc g1 a b 3.5) (graph-arcs 'g1) :test #'equal)))
(assert (member '(arc g1 a b 99) (graph-arcs 'g1) :test #'equal))

;;; delete
(delete-graph 'g1)
(assert (null (is-graph 'g1)))
(assert (null (graph-vertices 'g1)))
(assert (null (graph-arcs 'g1)))

(format t "Tutti i test superati!~%")