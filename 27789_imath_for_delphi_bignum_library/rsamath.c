/*
  Name:     rsamath.c
  Purpose:  Implements part of PKCS#1, v. 2.1, June 14, 2002 (RSA Labs)
  Author:   M. J. Fromberger <http://spinning-yarns.org/michael/>
  Info:     $Id: rsamath.c 635 2008-01-08 18:19:40Z sting $

  Copyright (C) 2002-2008 Michael J. Fromberger, All Rights Reserved.

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation files
  (the "Software"), to deal in the Software without restriction,
  including without limitation the rights to use, copy, modify, merge,
  publish, distribute, sublicense, and/or sell copies of the Software,
  and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 */

#include "rsamath.h"

#include <string.h>
#include <limits.h>

static mp_result s_rsa_transform(mp_int msg, mp_int exp, 
				 mp_int mod, mp_int out);

/* {{{ rsa_i2osp(z, out, len) */

/* Convert integer to octet string, per PKCS#1 v.2.1 */
mp_result rsa_i2osp(mp_int z, unsigned char *out, int len)
{
  int  excess_len = mp_int_binary_len(z);

  if(excess_len < len)
    return MP_RANGE;

  memset(out, 0, len);

  excess_len -= len;
  mp_int_to_binary(z, out + excess_len, len);

  return MP_OK;
}

/* }}} */

/* {{{ rsa_os2ip(z, in, len) */

/* Convert octet string to integer, per PKCS#1 v.2.1 */
mp_result rsa_os2ip(mp_int z, unsigned char *in, int len)
{
  return mp_int_read_binary(z, in, len);
}

/* }}} */

/* {{{ rsa_rsaep(msg, exp, mod, cipher) */

/* Primitive RSA encryption operation */
mp_result rsa_rsaep(mp_int msg, mp_int exp, mp_int mod, mp_int cipher)
{
  return s_rsa_transform(msg, exp, mod, cipher);
}

/* }}} */

/* {{{ rsa_rsadp(cipher, exp, mod, msg) */

/* Primitive RSA decryption operation */
mp_result rsa_rsadp(mp_int cipher, mp_int exp, mp_int mod, mp_int msg)
{
  return s_rsa_transform(cipher, exp, mod, msg);
}

/* }}} */

/* {{{ rsa_rsasp(msg, exp, mod, signature) */

/* Primitive RSA signing operation */
mp_result rsa_rsasp(mp_int msg, mp_int exp, mp_int mod, mp_int signature)
{
  return s_rsa_transform(msg, exp, mod, signature);
}

/* }}} */

/* {{{ rsa_rsavp(signature, exp, mod, msg) */

/* Primitive RSA verification operation */
mp_result rsa_rsavp(mp_int signature, mp_int exp, mp_int mod, mp_int msg)
{
  return s_rsa_transform(signature, exp, mod, msg);
}

/* }}} */

/* {{{ rsa_max_message_len(mod) */

/* Compute the maximum length in bytes a message can have using PKCS#1
   v.1.5 encoding with the given modulus */
int       rsa_max_message_len(mp_int mod)
{
  int  num_bits = mp_int_count_bits(mod);
  int  num_bytes = num_bits / CHAR_BIT;

  if(num_bytes < 11)
    return 0; /* at least eleven bytes are required for padding */
  else
    return num_bytes - 11;
}

/* }}} */

/* {{{ rsa_pkcs1v15_encode(buf, msg_len, buf_len, tag, filler) */

mp_result rsa_pkcs1v15_encode(unsigned char *buf, int msg_len, 
			      int buf_len, int tag, random_f filler)
{
  int  pad_len, msg_start;

  /* Make sure there is enough space for the encoded output */
  if(msg_len > (buf_len - 11))
    return MP_RANGE;

  msg_start = buf_len - msg_len;
  pad_len = msg_start - 3;

  /* Move message to top of buffer -- these might overlap, so we rely
     on the semantics of memmove() here */
  memmove(buf + msg_start, buf, msg_len);

  /* Set initial bytes as required by the specification */
  buf[0] = 0x00;
  buf[1] = (unsigned char)tag;

  /* Fill with random padding.  We'll just assume the filler function
     does the right thing and only writes the requested number of
     nonzero bytes */
  (filler)(buf + 2, pad_len);

  /* Write separator between pad and message body */
  buf[msg_start - 1] = 0x00;

  return MP_OK;
}

/* }}} */

/* {{{ rsa_pkcs1v15_decode(buf, buf_len, *msg_len) */

mp_result rsa_pkcs1v15_decode(unsigned char *buf, int buf_len, 
			      int tag, int *msg_len)
{
  int  pad_len = 0, data_len, data_start, i;

  /* Make sure the buffer is syntactically valid */
  if(buf_len < 11 || buf[0] != 0x00 || buf[1] != (unsigned char)tag)
    return MP_UNDEF;

  /* Figure out how many bytes of random padding there are */
  i = 2;
  while(buf[i++] != '\0')
    ++pad_len;

  data_start = i;
  data_len = buf_len - data_start;

  /* Shift the message to the front of the buffer */
  memmove(buf, buf + data_start, data_len);

  /* Zero out the rest of the buffer */
  memset(buf + data_len, 0, pad_len + 3);

  *msg_len = data_len;

  return MP_OK;
}

/* }}} */

/* {{{ s_rsa_transform(msg, exp, mod, out) */

static mp_result s_rsa_transform(mp_int msg, mp_int exp, 
				 mp_int mod, mp_int out)
{
  if(mp_int_compare_zero(msg) < 0 ||
     mp_int_compare(msg, mod) >= 0)
    return MP_RANGE;

  return mp_int_exptmod(msg, exp, mod, out);
}

/* }}} */

/* Here there be dragons */
