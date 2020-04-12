(in-package :whatsxmpp)

(defvar *db* nil
  "Connection to the database.")
(defvar *db-lock* (bt:make-recursive-lock "sqlite3 lock")
  "Lock for *DB*.")
(defparameter *default-database-path* "data.sqlite3"
  "Default path to the SQLite database file.")
(defvar *prepared-statements* nil
  "List of statements prepared by PREPARED-STATEMENT.")
(defparameter *sqlite-pragmas*
  '("PRAGMA journal_mode = WAL"
    "PRAGMA foreign_keys = ON"
    "PRAGMA synchronous = NORMAL")
  "List of SQLite pragmas to run on connection to make things bearable")

(defun run-pragmas ()
  "Runs all statements in *SQLITE-PRAGMAS*."
  (mapc (lambda (x) (sqlite:execute-non-query *db* x)) *sqlite-pragmas*))

(defun connect-database (&optional (path *default-database-path*))
  "Establish a connection to the database."
  (bt:with-recursive-lock-held (*db-lock*)
    (setf *db* (sqlite:connect path))
    (run-pragmas)
    (loop for sym in *prepared-statements*
          do (eval `(setf ,sym nil)))
    (setf *prepared-statements* nil)))

(defmacro with-transaction (&body forms)
  `(bt:with-recursive-lock-held (*db-lock*)
     (sqlite:with-transaction *db*
       ,@forms)))

(defmacro prepared-statement (statement)
  "Caches the creation of a prepared statement with SQL text STATEMENT.
In other words, prepares STATEMENT once, then returns the prepared statement after that instead of doing that work again."
  (let ((statement-sym (gensym "PREPARED-STATEMENT-")))
    (eval `(defvar ,statement-sym nil))
    `(progn
       (defvar ,statement-sym nil)
       (unless ,statement-sym
         (setf ,statement-sym (sqlite:prepare-statement *db* ,statement))
         (setf *prepared-statements* (cons ',statement-sym *prepared-statements*)))
       ,statement-sym)))

(defmacro with-prepared-statement ((name statement) &body forms)
  "Evaluates FORMS, binding a prepared statement with SQL text STATEMENT to NAME, and ensuring it is reset when control is transferred."
  `(bt:with-recursive-lock-held (*db-lock*)
     (let ((,name (prepared-statement ,statement)))
       (multiple-value-prog1
           (unwind-protect
                (progn ,@forms)
             (ignore-errors (sqlite:reset-statement ,name)))))))

(defmacro with-prepared-statements (statements &body forms)
  "Like WITH-PREPARED-STATEMENT, but takes multiple statements."
  (let ((let-forms (loop for (name statement) in statements
                         collect `(,name (prepared-statement ,statement))))
        (reset-forms (loop for (name statement) in statements
                           collect `(ignore-errors (sqlite:reset-statement ,name)))))
    `(bt:with-recursive-lock-held (*db-lock*)
       (let (,@let-forms)
         (multiple-value-prog1
             (unwind-protect
                  (progn ,@forms))
           (ignore-errors (progn ,@reset-forms)))))))

(defmacro column-values (statement)
  "Returns the values in the current row of the STATEMENT."
  (let ((i-sym (gensym))
        (stmt (gensym)))
    `(let ((,stmt ,statement))
       (loop
         for ,i-sym from 0 below (length (sqlite:statement-column-names ,stmt))
         collect (sqlite:statement-column-value ,stmt ,i-sym)))))

(defmacro with-bound-columns (parameters statement &body forms)
  "Binds each column value of STATEMENT to the symbols in PARAMETERS, and runs FORMS."
  (let ((let-forms (loop
                     for param in parameters
                     for idx from 0 upto (1- (length parameters))
                     collect `(,param (sqlite:statement-column-value ,statement ,idx)))))
    `(let (,@let-forms) ,@forms)))

(defmacro bind-parameters (statement &rest parameters)
  "Binds PARAMETERS to the prepared statement STATEMENT.

PARAMETERS are either simple values (in which case they're bound to parameters 1, 2, ...),
or cons cells, where the `car` is the index to bind to and the `cdr' is the value to use."
  `(progn
     ,@(loop for param in parameters
             for idx from 1 upto (length parameters)
             collect (if (listp param)
                         `(sqlite:bind-parameter ,statement ,(car param) ,(second param))
                         `(sqlite:bind-parameter ,statement ,idx ,param)))))
