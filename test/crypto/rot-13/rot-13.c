/*********************************************************************
* Filename:   rot-13.c
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Implementation of the ROT-13 encryption algorithm.
				  Algorithm specification can be found here:
				   *
				  This implementation uses little endian byte order.
*********************************************************************/

/*************************** HEADER FILES ***************************/
#include "rot-13.h"

/*********************** FUNCTION DEFINITIONS ***********************/
void rot13(char str[], int len)
{
   int idx;
   char this;

   for (idx = 0; idx < len; idx++) {
      this = str[idx];

      // If char is in the beginning of the alphabet, add 13
      if((this >= 'A' && this <= 'M') || (this >= 'a' && this <= 'm')) str[idx] += 13;

      // If char is in the end of the alphabet, subtract 13
      if((this >= 'N' && this <= 'Z') || (this >= 'n' && this <= 'z')) str[idx] -= 13;
   }
}
