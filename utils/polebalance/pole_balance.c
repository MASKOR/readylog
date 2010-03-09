#include "pole_balance.h"

#define WINDOW_HEIGHT		100
#define CART_SHOW_WIDTH		40
#define CART_SHOW_HEIGHT	10
#define POLE_SHOW_LENGTH	40
#define TIRE_SHOW_HEIGHT	4
#define WINDOW_WIDTH		((int) (((MAX_X_POSITION - MIN_X_POSITION) * (CART_SHOW_WIDTH / 2.0)) + CART_SHOW_WIDTH))

void showPoleCart(PoleCart *pole_cart) {
  if (pole_cart->window) {
    int cart_upper_left_x = (int) ((pole_cart->cart_x_position - MIN_X_POSITION) * (CART_SHOW_WIDTH / 2));
    int cart_upper_left_y = WINDOW_HEIGHT - CART_SHOW_HEIGHT - TIRE_SHOW_HEIGHT - 5;
    int pole_cart_x = cart_upper_left_x + CART_SHOW_WIDTH / 2;
    int pole_end_x = (int) (pole_cart_x + cos(pole_cart->pole_angle + M_PI_2) * (POLE_SHOW_LENGTH));
    int pole_end_y = (int) (cart_upper_left_y - sin(pole_cart->pole_angle + M_PI_2) * (POLE_SHOW_LENGTH));
    EZX_ClearWindow(pole_cart->window);
    EZX_SetColor(C_YELLOW);
    EZX_SetLineWidth(1);
    EZX_FillRectangle(pole_cart->window,cart_upper_left_x,cart_upper_left_y,CART_SHOW_WIDTH,CART_SHOW_HEIGHT);
    EZX_SetColor(C_RED);
    EZX_SetLineWidth(2);
    EZX_DrawLine(pole_cart->window,pole_cart_x,cart_upper_left_y,pole_end_x,pole_end_y);
    EZX_SetLineWidth(1);
    EZX_SetColor(C_BLUE);
    EZX_FillCircle(pole_cart->window,cart_upper_left_x + (CART_SHOW_WIDTH / 5),cart_upper_left_y + CART_SHOW_HEIGHT,TIRE_SHOW_HEIGHT,TIRE_SHOW_HEIGHT);
    EZX_FillCircle(pole_cart->window,cart_upper_left_x + CART_SHOW_WIDTH - (CART_SHOW_WIDTH / 5),cart_upper_left_y + CART_SHOW_HEIGHT,TIRE_SHOW_HEIGHT,TIRE_SHOW_HEIGHT);
    EZX_Flush();
  }
}

void initializePoleCart(PoleCart *pole_cart, int use_window) {
  //pole_cart->sim_length = 0.05;
  pole_cart->sim_length = 0.02;
  pole_cart->time_steps = 10;
  pole_cart->pole_length = 1.0;
  pole_cart->cart_mass = 1.0;
  pole_cart->pole_mass = 0.1;
  pole_cart->cart_x_position = 0.0;
  pole_cart->cart_x_velocity = 0.0;
  pole_cart->max_angle = M_PI_2; //0.209;//
  pole_cart->min_angle = - M_PI_2;//-0.209;//
  pole_cart->pole_angle = 0.0;
  pole_cart->pole_angular_velocity = 0.0;
  if (use_window) {
    pole_cart->window = EZX_MakeWindow("pole_balance", WINDOW_WIDTH, WINDOW_HEIGHT, NULL, C_BLACK);
    showPoleCart(pole_cart);
  }
  else
    pole_cart->window = NULL;
}

void initializePoleCartPosition(PoleCart *pole_cart) {
  pole_cart->cart_x_position = 0.0;
  pole_cart->cart_x_velocity = 0.0;
  //pole_cart->pole_angle = 0.01;
  //pole_cart->pole_angle = (random_double_in_range(-30.0,30.0) / 180.0) * M_PI;
  pole_cart->pole_angle = (random_double_in_range(-10.0,10.0) / 180.0) * M_PI;
  pole_cart->pole_angular_velocity = 0.0;
}

void destroyPoleCart(PoleCart *pole_cart) {
  if (pole_cart->window != NULL) EZX_EndWindow(pole_cart->window);
}

int positionPoleCartOK(PoleCart *pole_cart) {
  if (pole_cart->cart_x_position < MIN_X_POSITION) return 0;
  if (pole_cart->cart_x_position > MAX_X_POSITION) return 0;
  if (pole_cart->pole_angle < pole_cart->min_angle) return 0;
  if (pole_cart->pole_angle > pole_cart->max_angle) return 0;
  return 1;
}

void pushPoleCart(PoleCart *pole_cart, float force) {

  printf(" pushPoleCart with force %f \n", force );
  printf(" InitialPoleState: %f %f %f %f \n", pole_cart->cart_x_position, pole_cart->cart_x_velocity, pole_cart->pole_angle, pole_cart->pole_angular_velocity );

  int s;
  float angular_accelaration;
  float x_position_accelaration;

  s = 0;
  while (positionPoleCartOK(pole_cart) && (s <= pole_cart->time_steps)) {
    angular_accelaration = (GRAVITY * sin(pole_cart->pole_angle) + cos(pole_cart->pole_angle) * ((- force - pole_cart->cart_mass * pole_cart->pole_length * pole_cart->pole_angular_velocity * pole_cart->pole_angular_velocity * sin(pole_cart->pole_angle)) / (pole_cart->cart_mass + pole_cart->pole_mass))) / (pole_cart->pole_length * (4.0 / 3.0 - (pole_cart->pole_mass * cos(pole_cart->pole_angle) * cos(pole_cart->pole_angle))) / (pole_cart->cart_mass + pole_cart->pole_mass));
    x_position_accelaration = (force + pole_cart->pole_mass * pole_cart->pole_length * (pole_cart->pole_angular_velocity * pole_cart->pole_angular_velocity * sin(pole_cart->pole_angle) - angular_accelaration * cos(pole_cart->pole_angle))) / (pole_cart->cart_mass + pole_cart->pole_mass);
     
    pole_cart->cart_x_position += ((pole_cart->sim_length / pole_cart->time_steps) * pole_cart->cart_x_velocity);
    pole_cart->cart_x_velocity += ((pole_cart->sim_length / pole_cart->time_steps) * x_position_accelaration);
    pole_cart->pole_angle += ((pole_cart->sim_length / pole_cart->time_steps) * pole_cart->pole_angular_velocity);
    pole_cart->pole_angular_velocity += ((pole_cart->sim_length / pole_cart->time_steps) * angular_accelaration);

    //printf(" PoleState [%i]: %f %f %f %f \n", s, pole_cart->cart_x_position, pole_cart->cart_x_velocity, pole_cart->pole_angle, pole_cart->pole_angular_velocity );

    s++;
  }

  printf(" ResultingPoleState: %f %f %f %f \n", pole_cart->cart_x_position, pole_cart->cart_x_velocity, pole_cart->pole_angle, pole_cart->pole_angular_velocity );

  showPoleCart(pole_cart);
}
