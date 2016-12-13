(** Export the Plugins test API*)

(** Aggregate all the unittests of the Plugins module 
    and of its submodules(ie plugins)*)
val unittests : unit -> OUnit2.test


(** TODO
  *
  * integrationtests  : Testing of integrated modules to verify combined functionality after integration. Modules are typically code modules, individual applications, client and server applications on a network, etc. This type of testing is especially relevant to client/server and distributed systems.
  * Functional testing : This type of testing ignores the internal parts and focus on the output is as per requirement or not. Black-box type testing geared to functional requirements of an application.
  * Load testing : on heavy project for instance
  *
  *) 
