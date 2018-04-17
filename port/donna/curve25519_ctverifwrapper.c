#include "ct-verif.h"
#include <stdint.h>
#include "curve25519.h"

int _curve25519_donna_wrapper(
  /*secret*/ uint8_t mypublic[],
  /*public*/ uint32_t __mypublic_len,
  /*secret*/ const uint8_t _secret[],
  /*public*/ uint32_t ___secret_len,
  /*secret*/ const uint8_t basepoint[],
  /*public*/ uint32_t __basepoint_len)
{
    // addresses are always public
    public_in(__SMACK_value(mypublic));
    public_in(__SMACK_value(_secret));
    public_in(__SMACK_value(basepoint));

    // public values
    public_in(__SMACK_value(__mypublic_len));
    public_in(__SMACK_value(___secret_len));
    public_in(__SMACK_value(__basepoint_len));

    // no public array contents

    return _curve25519_donna(
        mypublic, 
        __mypublic_len, 
        _secret,
        ___secret_len,
        basepoint,
        __basepoint_len);
}
