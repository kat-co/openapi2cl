(in-package :cl-user)
(defpackage :openapi2cl/core
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:cl-yaml)
  (:import-from #:cl-strings)
  (:export
   #:with-directory-generate-files
   #:with-directory-generate
   #:with-yaml-generate
   #:with-json-generate))
(in-package :openapi2cl/core)

;; NOTE: The generation occurs by building up sexps. Any symbols which
;; are written into these sexps must be prepended by the cl-user
;; namespace; otherwise the symbols in the generated code will be
;; prepended by this package's namespace.

(defun generate (openapi client-name)
  "Generates a client and a list of its methods. These are returned in
a 2-tuple.

openapi: the sexp representation of a openapi document.
client-name: the name of the class that hosts all the client methods."
  (check-type client-name symbol)
  (let* ((host (gethash "host" openapi))
         (base-path (gethash "basePath" openapi))
         (consumes-media-types (gethash "consumes" openapi))
         (produces-media-types (gethash "produces" openapi))
         (schemes (or (gethash "schemes" openapi)
                      '("https")))
         (client (generate-client client-name schemes host base-path
                                  consumes-media-types produces-media-types)))
    (values
     client
     (loop for path-name being the hash-keys of (gethash "paths" openapi)
             using (hash-value path-operation)
           nconc (generate-path-methods client-name path-name path-operation
                                          consumes-media-types)))))

(defun with-yaml-generate (openapi-yaml client-name)
  "Generate a client and a list of its methods based on a YAML file.

openapi-yaml: a string containing the YAML representation of a openapi document, or a pathname to a YAML document.
client-name: the name of the class that hosts all the client methods."
  (generate (yaml:parse openapi-yaml) client-name))

(defun with-json-generate (openapi-json client-name)
  "Generate a client and a list of its methods based on a YAML file.

openapi-json: a string containing the JSON representation of a openapi document, or a pathname to a JSON document.
client-name: the name of the class that hosts all the client methods."
  (generate (yason:parse openapi-json) client-name))

(defun with-directory-generate (path process-results-fn)
  "Given a pathname, process the YAML and JSON files within, and
call `process-results-fn'. This function should take these
arguments: (1) The file-path (2) the client definition (3) the list of
methods for this client."
  (check-type process-results-fn function)
  (flet ((with-file-generate (file-path)
           (let* ((type (pathname-type file-path))
                  (name (kebab-symbol-from-string (pathname-name file-path)))
                  (output-path (make-pathname :type "lisp" :defaults file-path)))
             (multiple-value-bind (client-def methods-list)
                 (cond ((string= type "yaml")
                        (with-yaml-generate file-path name))
                       ((string= type "json")
                        (with-json-generate file-path name)))
               (funcall process-results-fn output-path client-def methods-list)))))
    (mapcar #'with-file-generate (directory path))))

(defun with-directory-generate-files (input-path output-path)
  "Given a pathname, generate lisp files for every specification
encountered and processed."
  (flet ((write-defs-out (file-path client-def methods-list)
           (let ((file-path (make-pathname :defaults file-path
                                           :directory output-path)))
             (with-open-file (stream file-path :direction :output)
               (format t "Output path: ~a~%" file-path)
               (format stream "~s~%~%" client-def)
               (dolist (m methods-list) (format stream "~s~%~%" m))))))
    (with-directory-generate input-path #'write-defs-out)))

;;; Unexported

(defun parameter-name (param)
  (check-type param hash-table)
  (gethash "name" param))

(defun kebab-symbol-from-string (s)
  (check-type s string)
  (intern (string-upcase (cl-strings:kebab-case s :delimiter #\_))))

(defun lisp-parameter-name (param)
  "Converts a string parameter name to a symbol for use in generated
code."
  (check-type param hash-table)
  (kebab-symbol-from-string (parameter-name param)))

(defun parameter-location (param)
  (check-type param hash-table)
  (gethash "in" param))

(defun parameter-type (param)
  (check-type param hash-table)
  (gethash "type" param))

(defun parameter-required-p (param)
  (check-type param hash-table)
  (or (gethash "required" param)
      (parameter-location-schema-p param)))

(defun parameter-location-schema-p (param)
  (check-type param hash-table)
  (gethash "schema" param))

(defun parameter-location-query-p (param)
  (check-type param hash-table)
  (string= (string-downcase (parameter-location param)) "query"))

(defun parameter-location-header-p (param)
  (check-type param hash-table)
  (string= (string-downcase (parameter-location param)) "header"))

(defun parameter-location-path-p (param)
  (check-type param hash-table)
  (string= (string-downcase (parameter-location param)) "path"))

(defun parameter-location-body-p (param)
  (check-type param hash-table)
  (string= (string-downcase (parameter-location param)) "body"))

(defun parameter-location-form-p (param)
  (check-type param hash-table)
  (string= (string-downcase (parameter-location param)) "formdata"))

(defun openapi-schemes-p (schemes)
  (and (listp schemes)
       (= (length schemes)
          ;; TODO(katco): ws and wss are also valid, but we don't yet handle these.
          (length (intersection schemes '("https" "http") :test #'string=)))))

(defun select-scheme (schemes)
  "Pick a scheme from a list of options in order of preference."
  (check-type schemes list)
  (or (find "https" schemes :test #'string=)
      (find "http" schemes :test #'string=)))

(defun media-type-form-p (media-type)
  (and media-type
       (find media-type '("application/x-www-form-urlencoded"
                          "multipart/form-data")
             :test #'string=)))

(defun media-type-subtype (media-type)
  (check-type media-type string)
  (subseq media-type (+ 1 (or (position #\+ media-type)
                              (position #\/ media-type)))))

(defun select-media-type (media-types)
  (check-type media-types list)
  (let ((media-subtypes (mapcar #'media-type-subtype media-types)))
    (or (find-if (lambda (e) (or (string= e "json") (string= e "yaml"))) media-subtypes)
        (nth (random (length media-subtypes)) media-subtypes))))

(defun generate-path-methods (client-name path-name path global-media-types)
  (check-type client-name symbol)
  (check-type path-name string)
  (check-type path hash-table)
  (check-type global-media-types list)
  (loop for operation-name being the hash-keys of path
          using (hash-value operation)
        for summary = (gethash "summary" operation)
        for description = (gethash "description" operation)
        for operation-id = (gethash "operationId" operation)
        for produces-media-types = (gethash "produces" operation)
        for consumes-media-types = (gethash "consumes" operation)
        for responses = (gethash "responses" operation)
        for schemes = (gethash "schemes" operation)
        for parameters = (gethash "parameters" operation)
        ;; Synthesized fields
        for body-param = (find-if #'parameter-location-schema-p parameters)
        for method-name = (if operation-id operation-id (format nil "~a-~a" operation-name path-name))
        for required-parameters = (remove body-param (remove-if-not #'parameter-required-p parameters))
        for optional-parameters = (remove-if #'parameter-required-p parameters)
        ;; Enumerate through the known valid operations that we know
        ;; how to handle. Ignore all else.
        unless (find operation-name '("get" "put" "post" "delete"))
          do (when body-param
               (multiple-value-bind (required-body-params optional-body-params)
                   (generate-schema-object-parameters (gethash "schema" body-param))
                 (setf required-parameters (append required-parameters required-body-params)
                       optional-parameters (append optional-parameters optional-body-params))))
        collect (let* ((param-descriptions (generate-parameter-comments (append required-parameters
                                                                                optional-parameters)))
                       (method-comment (concatenate 'string
                                                    (when summary (format nil "~a~%~%" summary))
                                                    (when description (format nil "~a~%~%" description))
                                                    (when param-descriptions param-descriptions))))
                  ;; Generate a method for each operation
                  (generate-operation-method client-name method-name method-comment schemes path-name
                                             operation-name global-media-types consumes-media-types
                                             produces-media-types required-parameters optional-parameters))))

(defun generate-client (client-name schemes host base-path consumes-media-types produces-media-types)
  "Generates a client that all the methods will hang off of."
  (check-type client-name symbol)
  (assert (openapi-schemes-p schemes))
  (check-type host string)
  (check-type base-path string)
  (check-type consumes-media-types list)
  (check-type produces-media-types list)
  `(defclass ,client-name ()
     ((cl-user::scheme
       :type string
       :documentation
       "The scheme to use for requests when the operation doesn't provide a preference of its own."
       :initarg :schemes
       :initform ,(select-scheme schemes)
       :accessor cl-user::scheme)
      (cl-user::host
       :type string
       :documentation
       "The host all requests for this client will be sent to."
       :initarg :host
       :initform ,host
       :accessor cl-user::host)
      (cl-user::base-path
       :type string
       :documentation
       "The base path that will be prepended to the path of all requests."
       :initarg :base-path
       :initform ,base-path
       :accessor cl-user::base-path)
      (cl-user::consumes-media-type
       :type string
       :documentation
       "The media-type to encode parameters to when operations do not declare a media-type."
       :initarg :consumes-media-type
       ,@(when consumes-media-types `(:initform ,(select-media-type consumes-media-types)))
       :accessor cl-user::consumes-media-type)
      (cl-user::produces-media-type
       :type string
       :documentation
       "The media-type to dencode results from when operations do not declare a media-type."
       :initarg :produces-media-type
       ,@(when produces-media-types `(:initform ,(select-media-type produces-media-types)))
       :accessor cl-user::produces-media-type)
      (cl-user::http-request
       :type function
       :documentation
       "A function for making HTTP requests. It needs to have the following signature: (lambda (uri &key method additional-headers content-type content parameters multipart-params))"
       :initarg :http-request
       :accessor cl-user::http-request)
      (cl-user::encoder-from-media-type
       :type hash-table
       :documentation
       "A hash table where the keys are media types the client can handle, and the values are functions which encode these media types to strings for inclusion into the http request."
       :initarg :encoder-from-media-type
       :initform (make-hash-table :test #'equalp)
       :accessor cl-user::encoder-from-media-type))))

(defun generate-operation-method (client-name method-name method-comment schemes path
                                  method global-media-types consumes-media-types produces-media-types
                                  required-parameters optional-parameters)
  "Generates a method for a generated client. Methods distinguish
between required parameters and optional parameters, and optional
parameters are defined as &key arguments. All arguments are run
through `check-type' to both ensure the correct type is used and that
the provided values meet any defined constraints."
  (check-type client-name symbol)
  (check-type method-name string)
  (check-type method-comment string)
  (when schemes (assert (openapi-schemes-p schemes)))
  (check-type path string)
  (check-type method string)
  (check-type global-media-types list)
  (check-type consumes-media-types list)
  (check-type produces-media-types list)
  (check-type required-parameters list)
  (check-type optional-parameters list)
  (let ((lisp-required-parameters (mapcar #'lisp-parameter-name required-parameters))
        (lisp-optional-parameters (mapcar #'lisp-parameter-name optional-parameters)))
    ;; Generated method begins here
    `(defmethod ,(kebab-symbol-from-string method-name) ((cl-user::client ,client-name)
                                                         ,@lisp-required-parameters
                                                         ,@(when lisp-optional-parameters
                                                             `(&key ,@lisp-optional-parameters)))
       ,method-comment
       ,@(generate-check-type (append required-parameters optional-parameters))
       ,(let ((path-params (append
                            (remove-if-not #'parameter-location-path-p required-parameters)
                            (remove-if-not #'parameter-location-path-p optional-parameters)))
              (query-params (append
                             (remove-if-not #'parameter-location-query-p required-parameters)
                             (remove-if-not #'parameter-location-query-p optional-parameters)))
              (headers (append
                        (remove-if-not #'parameter-location-header-p required-parameters)
                        (remove-if-not #'parameter-location-header-p optional-parameters)))
              (body-params (append
                            (remove-if-not #'parameter-location-body-p required-parameters)
                            (remove-if-not #'parameter-location-body-p optional-parameters)))
              (form-params (append
                            (remove-if-not #'parameter-location-form-p required-parameters)
                            (remove-if-not #'parameter-location-form-p optional-parameters)))
              (consumes-media-type (when consumes-media-types (select-media-type consumes-media-types))))
          ;; If the content-type requests form params, put all the
          ;; body parameters in the form params list. Otherwise, move
          ;; all the form parameters into the body list so that they
          ;; can be encoded properly.
          (if (media-type-form-p consumes-media-type)
              (setf form-params (append form-params body-params)
                    body-params nil)
              (setf body-params (append body-params form-params)
                    form-params nil))
          `(let (,@(when path-params '((cl-user::path-params (list))))
                 ,@(when query-params '((cl-user::query-params (list))))
                 ,@(when headers '((cl-user::headers (list))))
                 ,@(when body-params '((cl-user::body-params (list))))
                 ,@(when (and (media-type-form-p consumes-media-type)
                              form-params)
                     '((cl-user::form-params (list))))
                 ;; If we can't interpolate the scheme into the
                 ;; request URI's string, grab it from the client.
                 ,@(unless schemes `((cl-user::scheme (cl-user::scheme cl-user::client))))
                 (cl-user::consumes-media-type ,(if consumes-media-type
                                                    consumes-media-type
                                                    '(cl-user::consumes-media-type cl-user::client)))
                 ;; If the operation's declared content-type is form
                 ;; we don't need a hash-table to populate the body.
                 ;; If the operation doesn't have a declared
                 ;; content-type, we can't make the decision at
                 ;; gen-time and we must reflect on what's defined in
                 ;; the client.
                 ,@(when (and body-params
                              (or (not consumes-media-type)
                                  (media-type-form-p consumes-media-type)))
                     '((cl-user::req-body (make-hash-table)))))
             ;; Build up alist of query params
             ,@(generate-http-request-population path-params 'cl-user::path-params)
             ,@(generate-http-request-population query-params 'cl-user::query-params)
             ,@(generate-http-request-population headers 'cl-user::headers)
             ,@(if (media-type-form-p consumes-media-type)
                   (generate-http-request-population form-params 'cl-user::form-params)
                   (generate-http-request-body-population body-params 'cl-user::req-body))
             ;; Make the request
             (flet ((cl-user::replace-path-params (cl-user::uri cl-user::path-vars)
                      (funcall (cl-strings:make-template-parser "{" "}") cl-user::uri cl-user::path-vars)))
               (funcall (cl-user::http-request cl-user::client)
                        (format nil ,(concatenate 'string
                                                  (if schemes (select-scheme schemes) "~a:")
                                                  "//~a~a~a")
                                cl-user::scheme
                                (cl-user::host cl-user::client)
                                (cl-user::base-path cl-user::client)
                                ,(if path-params
                                     `(cl-user::replace-path-params ,path cl-user::path-params)
                                     path))
                        :method ,(intern (string-upcase method) "KEYWORD")
                        :content-type cl-user::consumes-media-type
                        ,@(when headers `(:additional-headers cl-user::headers))
                        ,@(when body-params
                            `(:content (funcall (gethash cl-user::consumes-media-type
                                                         (cl-user::encoder-from-media-type cl-user::client))
                                                cl-user::req-body)))
                        ,@(when query-params `(:parameters cl-user::query-params))
                        ,@(when form-params `(:multipart-params cl-user::form-params)))))))))

(defun generate-check-type (parameters)
  (check-type parameters list)
  (loop for param in parameters
        for param-name = (lisp-parameter-name param)
        for param-type = (parameter-type param)
        for lisp-param-type = (when param-type (kebab-symbol-from-string param-type))
        ;; Type must both be present and correspond to a check
        ;; function in order to perform type checking.
        when (find lisp-param-type '(string number integer array)
                   :test #'string=)
          collect (if (parameter-required-p param)
                      `(check-type ,param-name ,lisp-param-type)
                      `(when ,param-name (check-type ,param-name ,lisp-param-type)))))

(defun generate-http-request-body-population (params req-body-name)
  (check-type params list)
  (loop for param in params
        for param-name = (parameter-name param)
        for lisp-param-name = (lisp-parameter-name param)
        for set-value-sexp = `(setf (gethash ,param-name ,req-body-name) ,lisp-param-name)
        collect
        (if (parameter-required-p param) set-value-sexp `(when ,lisp-param-name ,set-value-sexp))))

(defun generate-http-request-population (params request-alist)
  "Generates code to populate an alist intended to be passed into an
http-request."
  (check-type params list)
  (check-type request-alist symbol)
  (loop for param in params
        for param-name = (parameter-name param)
        for lisp-param-name = (lisp-parameter-name param)
        collect
        (if (parameter-required-p param)
            `(setf ,request-alist (push (cons ,param-name ,lisp-param-name) ,request-alist))
            `(when ,lisp-param-name (setf ,request-alist (push (cons ,param-name  ,lisp-param-name) ,request-alist))))))

(defun generate-parameter-comments (parameters)
  "Generates a single string describing a list of parameters for
inclusion in a method's docstring."
  (check-type parameters list)
  (with-output-to-string (parameter-desc)
    (loop for param in parameters
          for lisp-param-name = (lisp-parameter-name param)
          for param-desc = (gethash "description" param)
          when param-desc
          do (format parameter-desc "~a: ~a~%" lisp-param-name param-desc))))

(defun generate-schema-object-parameters (schema-object)
  "Generates hash-tables which look like openapi parameters from
schema object properties. This is so that we can pass these into
generation code that synthesizes CL method parameters from openapi
parameters."
  ;; TODO(katco): Create types since properties and parameters have similarities
  (check-type schema-object hash-table)
  (let ((schema-properties (gethash "properties" schema-object))
        (required-parameters (list))
        (optional-parameters (list))
        (declared-as-required (gethash "required" schema-object)))
    (when schema-properties
      (loop for prop-name being the hash-keys of schema-properties
              using (hash-value prop)
            for prop-type = (parameter-type prop)
            for prop-desc = (gethash "description" prop)
            for faux-param = (make-hash-table :test #'equalp)
            do (setf (gethash "required" faux-param) (find prop-name declared-as-required :test #'string=)
                     (gethash "name" faux-param) prop-name
                     (gethash "type" faux-param) prop-type
                     (gethash "in" faux-param) "body"
                     (gethash "description" faux-param) prop-desc)
               (if (find prop-name declared-as-required :test #'string=)
                   (setf required-parameters (push faux-param required-parameters))
                   (setf optional-parameters (push faux-param optional-parameters)))))
    (values required-parameters optional-parameters)))
