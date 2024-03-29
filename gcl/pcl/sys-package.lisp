;; Copyright (C) 2024 Camm Maguire
(make-package :slot-accessor-name :use '(:cl) :nicknames '(:s-a-n))
(make-package :walker :use '(:cl))
(make-package :iterate :use '(:cl :walker))
(make-package :pcl :use '(:cl :iterate :walker :s))
(import 'si::(clines defentry defcfun object void int double non-negative-fixnum macro memq seqind structurep structure-def structure-ref std-instance funcallable-std-instance) :pcl)
(import 'si::(macro) :walker)
