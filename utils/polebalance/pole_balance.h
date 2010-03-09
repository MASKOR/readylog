#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "EZX11.h"

#ifndef POLE_BALANCE_H
#define POLE_BALANCE_H

#define random_double_in_range(low,high)       ((((high) - (low)) * 0.4656612875e-9 * random()) + (low))

#define GRAVITY	9.81
//#define GRAVITY	5.0

#define MIN_X_POSITION	-15.0
#define MAX_X_POSITION	 15.0

#ifdef __cplusplus
extern "C" {
#endif

typedef struct  {
  float  sim_length;
  int    time_steps;
  float  pole_length;
  float  cart_mass;
  float  pole_mass;
  float  cart_x_position;
  float  cart_x_velocity;
  float  max_angle;
  float  min_angle;
  float  pole_angle;
  float  pole_angular_velocity;
  EZXW_p window;
} PoleCart;

//extern 
void initializePoleCart(PoleCart *pole_cart, int use_window);
//extern 
void initializePoleCartPosition(PoleCart *pole_cart);
//extern 
void destroyPoleCart(PoleCart *pole_cart);
//extern 
int  positionPoleCartOK(PoleCart *pole_cart);
void pushPoleCart(PoleCart *pole_cart, float force);

#ifdef __cplusplus
}
#endif

#endif
