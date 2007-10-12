/***********************************************************************
 ******* 
 ******* Lab: Robotersteuerung mit Golog WS 04/05
 ******* 
 ******* $Id: hli.pl,v 1.1 2004/10/26 09:53:09 ferrein Exp $
 ******* 
 ******* Description: external interface to read keyboard input
 ******* 
 ******* Author: A. Ferrein
 ******* 
 ******* Contact: <ferrein@cs.rwth-aachen.de>
 *******
 ******* last modfied: $Date: 2004/10/26 09:53:09 $
 *******           by: $Author: ferrein $
 ***********************************************************************/

:- external(start_display/3, "p_StartDisplay").

sd1 :- start_display(10,10, []).

sd2 :- start_display(5,5, []).

display :- get_domain(X, Y, W), start_display(X, Y, W).

:- external(draw_action/3, "p_DrawAction").

:- external(draw_policy/3, "p_DrawPolicy").

:- external(draw_goal/2, "p_DrawGoal").

:- external(draw_start/2, "p_DrawStart").

:- external(draw_values/1, "p_DrawValues").

:- external(refresh_window/0, "p_Refresh").

