(in-package :whatsxmpp)

(defvar *db* nil
  "Connection to the database.")
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
  (setf *db* (sqlite:connect path))
  (run-pragmas)
  (loop for sym in *prepared-statements*
        do (eval `(setf ,sym nil)))
  (setf *prepared-statements* nil))

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
  `(let ((,name (prepared-statement ,statement)))
     (unwind-protect
          (progn ,@forms)
       (ignore-errors (sqlite:reset-statement ,name)))))

(defmacro with-prepared-statements (statements &body forms)
  "Like WITH-PREPARED-STATEMENT, but takes multiple statements."
  (let ((let-forms (loop for (name statement) in statements
                         collect `(,name (prepared-statement ,statement))))
        (reset-forms (loop for (name statement) in statements
                           collect `(ignore-errors (sqlite:reset-statement ,name)))))
    `(let (,@let-forms)
       (unwind-protect
            (progn ,@forms))
       (ignore-errors (progn ,@reset-forms)))))

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
