(ql:quickload "calc")
(ensure-directories-exist #p"build/calc")
(asdf:make "calc")
