/***********************************************************************
 ******* 
 ******* Lab: Robotersteuerung mit Golog WS 04/05
 ******* 
 ******* $Id$
 ******* 
 ******* Description: external interface to read keyboard input
 ******* 
 ******* Author: A. Ferrein
 ******* 
 ******* Contact: <ferrein@cs.rwth-aachen.de>
 *******
 ******* last modfied: $Date$
 *******           by: $Author$
 ***********************************************************************/

:- external( chkkey/0, "p_chkkey" ).
:- external( getkey/1, "p_getkey" ).
:- external( getkey_blocking/1, "p_getkey_blocking" ).
