(in-package :cl-user)
(defpackage :openapi2cl/core
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:cl-yaml)
  (:import-from #:cl-strings)
  (:import-from #:kebab)
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

(defun generate (root-path openapi client-name)
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
         (security-schemas (security-schemas-from-hash-table
                            (resolve-object root-path (gethash "securityDefinitions" openapi))))
         (client (generate-client client-name schemes host base-path
                                  consumes-media-types produces-media-types
                                  security-schemas)))
    (values
     client
     (loop for path-name being the hash-keys of (gethash "paths" openapi)
             using (hash-value path-operation)
           nconc (generate-path-methods client-name path-name path-operation
                                        consumes-media-types security-schemas)))))

(defun with-yaml-generate (root-path openapi-yaml client-name)
  "Generate a client and a list of its methods based on a YAML file.

root-path: The root path of the Yaml file.
openapi-yaml: a string containing the YAML representation of a openapi document, or a pathname to a YAML document.
client-name: the name of the class that hosts all the client methods."
  (check-type root-path (or string pathname))
  (check-type openapi-yaml (or string pathname))
  (check-type client-name symbol)
  (generate root-path (yaml:parse openapi-yaml) client-name))

(defun with-json-generate (root-path openapi-json client-name)
  "Generate a client and a list of its methods based on a YAML file.

root-path: The root path of the Json file.
openapi-json: a string containing the JSON representation of a openapi document, or a pathname to a JSON document.
client-name: the name of the class that hosts all the client methods."
  (check-type root-path (or string pathname))
  (check-type openapi-json (or string pathname))
  (check-type client-name symbol)
  (generate root-path (yason:parse openapi-json) client-name))

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
                        (with-yaml-generate (uiop:pathname-directory-pathname file-path) file-path name))
                       ((string= type "json")
                        (with-json-generate (uiop:pathname-directory-pathname file-path) file-path name)))
               (funcall process-results-fn output-path client-def methods-list)))))
    (mapcar #'with-file-generate (directory path))))

(defun with-directory-generate-files (input-path output-path package-root &optional preamble)
  "Given a pathname, generate lisp files for every specification
encountered and processed.

package-root will be used as the root of each file's package. The
file's name will be appended to this forming the fully qualified
package name."
  (check-type package-root symbol)
  (flet ((write-defs-out (file-path client-def methods-list)
           (let ((file-path (make-pathname :defaults file-path
                                           :directory output-path))
                 (*package* (find-package :cl-user)))
             (with-open-file (stream file-path :direction :output :if-exists :supersede)
               (format t "Output path: ~a~%" file-path)
               (when preamble (format stream "~a~%~%" preamble)
                     (finish-output))
               (when package-root
                 (dolist (pkg-clause (generate-package-clauses
                                      (intern (string-upcase (format nil "~a/~a" package-root (pathname-name file-path))))
                                      :packages-using '(#:cl)
                                      :packages-import '(#:cl-strings)))
                   (format stream "~s~%" pkg-clause)
                   (finish-output))
                 (format stream "~%~%"))
               (format stream "~s~%" client-def)
               (finish-output)
               (dolist (m methods-list) (format stream "~%~%~s~%" m) (finish-output stream))))))
    (with-directory-generate input-path #'write-defs-out)))

;;; Unexported

(defun resolve-ref (ref)
  "Returns the referenced object"
  (check-type ref pathname)
  (let ((type (pathname-type ref)))
    (when (equalp type "yaml")
      (return-from resolve-ref (yaml:parse ref)))
    (when (equalp type "json")
      (return-from resolve-ref (yason:parse ref)))
    (error "cannot resolve references of type ~a" type)))

(defun resolve-object (root-path object)
  "If OBJECT is a reference, then return the referenced object.
Otherwise, just return the object."
  (check-type root-path (or pathname string))
  (check-type object (or hash-table null))
  (unless object (return-from resolve-object nil))
  (alexandria:if-let (val (gethash "$ref" object))
    (resolve-ref (merge-pathnames val root-path))
    object))

(defun parameter-name (param)
  (check-type param hash-table)
  (gethash "name" param))

(defun kebab-symbol-from-string (s &optional package)
  (check-type s string)
  (if package
      (intern (string-upcase (kebab:to-lisp-case s)) package)
      (intern (string-upcase (kebab:to-lisp-case s)))))

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

(defun generate-package-clauses (package-name &key packages-using packages-import)
  (check-type package-name symbol)
  (check-type packages-using list)
  (check-type packages-import list)
  `((in-package :cl-user)
    (defpackage ,package-name
      ,@(when packages-using `((:use ,@packages-using)))
      ,@(when packages-import
         (loop for pkg in packages-import
               collect `(:import-from ,pkg))))
    (in-package ,package-name)))

(defun generate-path-methods (client-name path-name path global-media-types security-schemas)
  (check-type client-name symbol)
  (check-type path-name string)
  (check-type path hash-table)
  (check-type global-media-types list)
  (check-type security-schemas list)
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
        for security-requirement = (gethash "security" operation)
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
                                             produces-media-types required-parameters optional-parameters
                                             (select-security-requirement security-schemas
                                                                          security-requirement)))))

(defun select-security-requirement (security-schemas security-requirements)
  "Selects a security requirement from the list of options in an
opinionated way. This algorithm prefers API Keys"
  (check-type security-schemas list)
  (check-type security-requirements list)
  (when security-requirements
    (car security-schemas)))

(defun generate-client (client-name schemes host base-path consumes-media-types
                        produces-media-types security-schemas)
  "Generates a client that all the methods will hang off of."
  (check-type client-name symbol)
  (assert (openapi-schemes-p schemes))
  (check-type host string)
  (check-type base-path string)
  (check-type consumes-media-types list)
  (check-type produces-media-types list)
  (check-type security-schemas list)
  `(defclass ,client-name ()
     ((cl-user::scheme
       :type string
       :documentation
       "The scheme to use for requests when the operation doesn't
provide a preference of its own."
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
       :accessor cl-user::encoder-from-media-type)
      ,@(loop for (name . schema) in security-schemas
              for lisp-name = (kebab-symbol-from-string (name schema))
              when (eq (security-type schema) 'api-key)
                collect `(,lisp-name
                          :type string
                          :documentation
                          ,(description schema)
                          :accessor ,lisp-name)))))

(defun generate-operation-method (client-name method-name method-comment schemes path
                                  method global-media-types consumes-media-types produces-media-types
                                  required-parameters optional-parameters security-requirement)
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
  (check-type security-requirement list)
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
             ,(when (and security-requirement
                         (eq (security-type (cdr security-requirement)) 'api-key))
                (let ((accessor (kebab-symbol-from-string (car security-requirement)))
                      (requirement (cdr security-requirement)))
                  `(setf cl-user::query-params (push (cons ,(name requirement)
                                                  (,accessor cl-user::client))
                                                     cl-user::query-params))))
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

;;; Security Scheme Objects

(deftype variable-location () '(member query header path body form-data))

(defclass schema-variable ()
  ((name
    :type string
    :documentation
    "The name of the header or query parameter to be used."
    :initarg :name
    :reader name)
   (in
    :type variable-location
    :documentation
    "Where in the HTTP request the variable is placed. Valid values
are '(query header path body form-data)"
    :initarg :in
    :reader in)))

(deftype flow () '(member implicit password application access-code))
(deftype security-type () '(member basic api-key oauth2))

(defun security-schemas-from-hash-table (raw)
  "Collects security schemas into an alist of (name . instance)."
  (check-type raw (or hash-table null))
  (unless raw (return-from security-schemas-from-hash-table (list)))
  (loop for schema-name being the hash-keys of raw
        for schema being the hash-values of raw
        collect (cons
                 schema-name
                 (make-instance 'security-schema
                                :name (gethash "name" schema)
                                :in (kebab-symbol-from-string (gethash "in" schema) :openapi2cl/core)
                                :type (kebab-symbol-from-string (gethash "type" schema) :openapi2cl/core)
                                :description (gethash "description" schema)
                                :flow (alexandria:when-let (flow (gethash "flow" schema)) (intern flow))
                                :authorization-url (gethash "authorizationUrl" schema)
                                :token-url (gethash "tokenUrl" schema)
                                :scopes (gethash "scopes" schema)))))

(defclass security-schema (schema-variable)
  ((type
    :type security-type
    :documentation
    "The type of the security scheme."
    :initarg :type
    :initform (error "type not specified")
    :reader security-type)
   (description
    :type (or string null)
    :documentation
    "A short description for security scheme."
    :initarg :description
    :reader description)
   (flow
    :type (or flow null)
    :documentation
    "The flow used by the OAuth2 security scheme."
    :initarg :flow)
   (authorization-url
    :type (or string null)
    :documentation
    "The authorization URL to be used for this flow. This SHOULD be in
the form of a URL."
    :initarg :authorization-url)
   (token-url
    :type (or string null)
    :documentation
    "The token URL to be used for this flow. This SHOULD be in the
form of a URL."
    :initarg :token-url)
   (scopes
    :documentation
    "The available scopes for the OAuth2 security scheme."
    :initarg :scopes))
  (:documentation
   "A security schema that an OpenAPI endpoint accepts."))
