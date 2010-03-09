/* ***************************************************************************
 *  ,=³ .%%% %%%%%%%. .%%%%%.  .%%%%%.   %%%%%.%%. %. %%%%%%%% %% Rheinisch-
 * [==].%%%   %%   %% %%   %%  %%   %%   %%  %%'%%.%%.%% %% %% %% Westfälische
 *  //l%%%    %%   %% %%%%. ' %%       @ %%%%%' '%%%%%%% %% %%%%% Technische
 * [==]%%     %%|=%%   %%=>%  %%         %%  %%  '%%'%%% %% %% %% Hochschule
 * [==]%%%    %%   %% . '%%%% %%  '%%%   %%   %%  '% '%% %% %% %% Aachen
 * [==]'%%%   %%   %% %%   %%  %%   %%   http://kbsg.rwth-aachen.de/
 * o^^o '%%% %%%%%%%' '%%%%%'O '%%%%%'   Knowledge Based Systems Group
 * ***************************************************************************
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; version 2 of the License.
 *
 * ***************************************************************************
 *
 *           $Id$
 *        author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 *   description: external interface to cart-pole-balancing sim+vis
 *
 * ************************************************************************ */

#include <eclipseclass.h>
#include <iostream>
#include "pole_balance.h"

//extern PoleCart pole_cart;
PoleCart pole_cart;
bool b_cart_initialized = false;

extern "C" int 
init_cart()
{
  //srandom(time(0));
  initializePoleCart(&pole_cart,1);
  initializePoleCartPosition(&pole_cart);
  b_cart_initialized = true;
  return EC_succeed;
}

extern "C" int 
exit_cart()
{
  destroyPoleCart(&pole_cart);
  b_cart_initialized = false;
  return EC_succeed;
}

extern "C" int 
push_right_strong()
{
  if( b_cart_initialized ) {
    //pushPoleCart(&pole_cart, 30);
    pushPoleCart(&pole_cart, 60);
    //pushPoleCart(&pole_cart, 75);
    return EC_succeed;
  } 
  return EC_fail;
}

extern "C" int 
push_right_medium()
{
  if( b_cart_initialized ) {
    //pushPoleCart(&pole_cart, 20);
    //pushPoleCart(&pole_cart, 25);
    pushPoleCart(&pole_cart, 40);
    //pushPoleCart(&pole_cart, 100);
    return EC_succeed;
  } 
  return EC_fail;
}

extern "C" int 
push_right_low()
{
  if( b_cart_initialized ) {
    //pushPoleCart(&pole_cart, 10);
    //pushPoleCart(&pole_cart, 15);
    pushPoleCart(&pole_cart, 20);
    //pushPoleCart(&pole_cart, 25);
    return EC_succeed;
  } 
  return EC_fail;
}

extern "C" int 
push_nothing()
{
  if( b_cart_initialized ) {
    pushPoleCart(&pole_cart,0);
    return EC_succeed;
  } 
  return EC_fail;
}

extern "C" int 
push_left_low()
{
  if( b_cart_initialized ) {
    //pushPoleCart(&pole_cart, -10);
    //pushPoleCart(&pole_cart, -15);
    pushPoleCart(&pole_cart, -20);
    //pushPoleCart(&pole_cart, -25);
    return EC_succeed;
  } 
  return EC_fail;
}

extern "C" int 
push_left_medium()
{
  if( b_cart_initialized ) {
    //pushPoleCart(&pole_cart, -20);
    //pushPoleCart(&pole_cart, -25);
    pushPoleCart(&pole_cart, -40);
    //pushPoleCart(&pole_cart, -100);
    return EC_succeed;
  } 
  return EC_fail;
}
 
extern "C" int 
push_left_strong()
{
  if( b_cart_initialized ) {
    //pushPoleCart(&pole_cart, -30);
    //pushPoleCart(&pole_cart, -50);
    pushPoleCart(&pole_cart, -60);
    //pushPoleCart(&pole_cart, -75);
    return EC_succeed;
  } 
  return EC_fail;
}
 
extern "C" int 
cart_ok()
{
  if( positionPoleCartOK(&pole_cart) ) {
    return EC_succeed;
  } 
  return EC_fail;
}

extern "C" int 
cart_data()
{
  if( b_cart_initialized ) {

//     std::cout << "Getting cart-pole data ..." << std::endl;
//     std::cout << "      cart-x-position:" << pole_cart.cart_x_position << std::endl;
//     std::cout << "      cart-x-velocity:" << pole_cart.cart_x_velocity << std::endl;
//     std::cout << "           pole-angle:" << pole_cart.pole_angle << std::endl;
//     std::cout << "      pole-angle-velo:" << pole_cart.pole_angular_velocity << std::endl;
    
    if( unify( EC_arg(1), EC_word( pole_cart.cart_x_position ) ) != EC_succeed ) {
      std::cout << " ***** cart-x-position couldn't be set" << std::endl;
      return EC_fail;
    }
    if( unify( EC_arg(2), EC_word( pole_cart.cart_x_velocity ) ) != EC_succeed ) {
      std::cout << " ***** cart-x-velocity couldn't be set" << std::endl;
      return EC_fail;
    }
    if( unify( EC_arg(3), EC_word( pole_cart.pole_angle ) ) != EC_succeed ) {
      std::cout << " ***** pole-angle couldn't be set" << std::endl;
      return EC_fail;
    }
    if( unify( EC_arg(4), EC_word( pole_cart.pole_angular_velocity ) ) != EC_succeed ) {
      std::cout << " ***** pole-angular velocity couldn't be set" << std::endl;
      return EC_fail;
    }

    return EC_succeed;
  } 
  return EC_fail;
}
