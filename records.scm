;;;; srfi-9 reference implementation


; This implements a record abstraction that is identical to vectors,
; except that they are not vectors (VECTOR? returns false when given a
; record and RECORD? returns false when given a vector).  The following
; procedures are provided:
;   (record? <value>)                -> <boolean>
;   (make-record <size>)             -> <record>
;   (record-ref <record> <index>)    -> <value>
;   (record-set! <record> <index> <value>) -> <unspecific>
;
; These can implemented in R5RS Scheme as vectors with a distinguishing
; value at index zero, providing VECTOR? is redefined to be a procedure
; that returns false if its argument contains the distinguishing record
; value.  EVAL is also redefined to use the new value of VECTOR?.


; needs %record-type-id-counter from nonstd.scm


; Definitions of the record procedures.

(define-inline (make-record size name)
  (let* ((size (%fx+ size 2))
	 (r (%allocate-block #x0a (arithmetic-shift size 8) #f size #t #f)))
    (%slot-set! r 0 name)
    (%slot-set! r 1 %record-type-id-counter)
    (set! %record-type-id-counter (%fx+ %record-type-id-counter 1))
    r))

(define-inline (record-ref record index)
  (%slot-ref record (%fx+ index 2)))

(define-inline (record-set! record index value)
  (%slot-set! record (%fx+ index 2) value))


; We define the following procedures:
; 
; (make-record-type <type-name <field-names>)    -> <record-type>
; (record-constructor <record-type<field-names>) -> <constructor>
; (record-predicate <record-type>)               -> <predicate>
; (record-accessor <record-type <field-name>)    -> <accessor>
; (record-modifier <record-type <field-name>)    -> <modifier>
;   where
; (<constructor> <initial-value> ...)         -> <record>
; (<predicate> <value>)                       -> <boolean>
; (<accessor> <record>)                       -> <value>
; (<modifier> <record> <value>)         -> <unspecific>

; Record types are implemented using vector-like records.  The first
; slot of each record contains the record's type, which is itself a
; record.

(define-inline (record-type record) (record-ref record 0))

;----------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;  (define-record-type :record-type
;    (make-record-type name field-tags)
;    record-type?
;    (name record-type-name)
;    (field-tags record-type-field-tags))
; As it is, we need to define everything by hand.

(define :record-type (make-record 3 'record-type))
(record-set! :record-type 0 :record-type)	; Its type is itself.
(record-set! :record-type 1 ':record-type)
(record-set! :record-type 2 '(name field-tags))

; Now that :record-type exists we can define a procedure for making more
; record types.

(define (make-record-type name field-tags)
  (let ((new (make-record 3 'record-type)))
    (record-set! new 0 :record-type)
    (record-set! new 1 name)
    (record-set! new 2 field-tags)
    new))

; Accessors for record types.

(define-inline (record-type-name record-type)
  (record-ref record-type 1))

(define-inline (record-type-field-tags record-type)
  (record-ref record-type 2))

;----------------
; A utility for getting the offset of a field within a record.

(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

;----------------
; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag)
                        (field-index type tag))
                      tags)))
    (lambda args
      (if (= (length args)
             arg-count)
          (let ((new (make-record (+ size 1) (record-type-name type))))
            (record-set! new 0 type)
            (for-each (lambda (arg i)
			(record-set! new i arg))
                      args
                      indexes)
            new)
          (error "wrong number of arguments to constructor" type args)))))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing)
         (eq? (record-type thing)
              type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-ref thing index)
          (error "accessor applied to bad value" type tag thing)))))

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-set! thing index value)
          (error "modifier applied to bad value" type tag thing)))))


; Definition of DEFINE-RECORD-TYPE

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define type
         (make-record-type 'type '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

; An auxilliary macro for define field accessors and modifiers.
; This is needed only because modifiers are optional.

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))
