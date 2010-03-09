/***********************************************************************
 * 
 *          $Id: polebalance.pl,v 1.4 2009/01/29 16:42:40 stf Exp $
 *  description: external interface to pole balancer
 *       author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 * last modfied: $Date: 2009/01/29 16:42:40 $
 *           by: $Author: stf $
 * 
 ***********************************************************************/

:- external( init_cart/0, "init_cart" ).
:- external( exit_cart/0, "exit_cart" ).

:- external( cart_ok/0, "cart_ok" ).
:- external( cart_data/4, "cart_data" ).

:- external( push_left_low/0, "push_left_low" ).
:- external( push_left_medium/0, "push_left_medium" ).
:- external( push_left_strong/0, "push_left_strong" ).
:- external( push_right_low/0, "push_right_low" ).
:- external( push_right_medium/0, "push_right_medium" ).
:- external( push_right_strong/0, "push_right_strong" ).
:- external( push_nothing/0, "push_nothing" ).
