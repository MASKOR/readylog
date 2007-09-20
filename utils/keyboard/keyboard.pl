/***********************************************************************
 ******* 
 ******* Lab: Robotersteuerung mit Golog WS 04/05
 ******* 
 ******* $Id: hli.pl,v 1.1 2007/08/17 16:16:31 beck Exp $
 ******* 
 ******* Description: external interface to read keyboard input
 ******* 
 ******* Author: A. Ferrein
 ******* 
 ******* Contact: <ferrein@cs.rwth-aachen.de>
 *******
 ******* last modfied: $Date: 2007/08/17 16:16:31 $
 *******           by: $Author: beck $
 ***********************************************************************/

:- external( chkkey/0, "p_chkkey" ).
:- external( getkey/1, "p_getkey" ).
