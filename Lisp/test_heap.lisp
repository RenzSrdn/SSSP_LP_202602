(load "sssp.lisp")

(defun run-heap-tests ()
  (format t "~%=== TEST HEAP ===~%~%")

  ;; Test 1: creazione heap
  (format t "--- Test 1: new-heap ---~%")
  (new-heap 'h1)
  (format t "Heap H1 creato: ~A~%" (not (null (gethash 'h1 *heaps*))))

  ;; Test 2: heap-empty su heap vuoto
  (format t "~%--- Test 2: heap-empty ---~%")
  (format t "H1 vuoto? ~A (atteso T)~%" (heap-empty 'h1))

  ;; Test 3: heap-insert e heap-head
  (format t "~%--- Test 3: heap-insert e heap-head ---~%")
  (heap-insert 'h1 5 'a)
  (heap-insert 'h1 3 'b)
  (heap-insert 'h1 7 'c)
  (heap-insert 'h1 1 'd)
  (heap-insert 'h1 4 'e)
  (format t "Head di H1: ~A (atteso (1 D))~%" (heap-head 'h1))
  (format t "H1 vuoto? ~A (atteso NIL)~%" (heap-empty 'h1))
  (format t "H1 non vuoto? ~A (atteso T)~%" (heap-not-empty 'h1))

  ;; Test 4: heap-print
  (format t "~%--- Test 4: heap-print ---~%")
  (heap-print 'h1)

  ;; Test 5: heap-extract in ordine
  (format t "~%--- Test 5: heap-extract in ordine crescente ---~%")
  (format t "Extract 1: ~A (atteso (1 D))~%" (heap-extract 'h1))
  (format t "Extract 2: ~A (atteso (3 B))~%" (heap-extract 'h1))
  (format t "Extract 3: ~A (atteso (4 E))~%" (heap-extract 'h1))
  (format t "Extract 4: ~A (atteso (5 A))~%" (heap-extract 'h1))
  (format t "Extract 5: ~A (atteso (7 C))~%" (heap-extract 'h1))
  (format t "H1 vuoto? ~A (atteso T)~%" (heap-empty 'h1))

  ;; Test 6: heap-modify-key
  (format t "~%--- Test 6: heap-modify-key ---~%")
  (new-heap 'h2)
  (heap-insert 'h2 10 'x)
  (heap-insert 'h2 20 'y)
  (heap-insert 'h2 30 'z)
  (format t "Head prima: ~A (atteso (10 X))~%" (heap-head 'h2))
  (heap-modify-key 'h2 5 20 'y)
  (format t "Head dopo modify-key (20->5) su Y: ~A (atteso (5 Y))~%" (heap-head 'h2))
  (heap-modify-key 'h2 50 30 'z)
  (format t "Head dopo modify-key (30->50) su Z: ~A (atteso (5 Y))~%" (heap-head 'h2))

  ;; Test 7: resize automatico (inserimento oltre capacita' iniziale)
  (format t "~%--- Test 7: resize automatico ---~%")
  (new-heap 'h3 3) ;; capacita' iniziale piccola
  (heap-insert 'h3 9 'i1)
  (heap-insert 'h3 8 'i2)
  (heap-insert 'h3 7 'i3)
  (heap-insert 'h3 6 'i4)  ;; dovrebbe fare resize
  (heap-insert 'h3 5 'i5)
  (format t "Head di H3: ~A (atteso (5 I5))~%" (heap-head 'h3))
  (format t "Size di H3: ~A (atteso 5)~%" (heap-rep-size (gethash 'h3 *heaps*)))

  ;; Test 8: heap-delete
  (format t "~%--- Test 8: heap-delete ---~%")
  (heap-delete 'h1)
  (heap-delete 'h2)
  (heap-delete 'h3)
  (format t "H1 rimosso? ~A (atteso T)~%" (null (gethash 'h1 *heaps*)))

  ;; Test 9: errori attesi
  (format t "~%--- Test 9: gestione errori ---~%")
  (handler-case
      (heap-insert 'heap-inesistente 1 'a)
    (error (e) (format t "Errore atteso heap-insert: ~A~%" e)))
  (handler-case
      (heap-extract 'heap-inesistente)
    (error (e) (format t "Errore atteso heap-extract (non esiste): ~A~%" e)))
  (new-heap 'hempty)
  (handler-case
      (heap-extract 'hempty)
    (error (e) (format t "Errore atteso heap-extract (vuoto): ~A~%" e)))
  (heap-delete 'hempty)
  (handler-case
      (heap-print 'heap-inesistente)
    (error (e) (format t "Errore atteso heap-print: ~A~%" e)))
  (new-heap 'hmod)
  (heap-insert 'hmod 10 'a)
  (handler-case
      (heap-modify-key 'hmod 5 99 'a)
    (error (e) (format t "Errore atteso heap-modify-key (old-key non valida): ~A~%" e)))
  (heap-delete 'hmod)

  (format t "~%=== TUTTI I TEST COMPLETATI ===~%"))

(run-heap-tests)