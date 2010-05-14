/*	$NetBSD: cryptodev.h,v 1.17 2009/09/04 08:58:44 he Exp $ */
/*	$FreeBSD: src/sys/opencrypto/cryptodev.h,v 1.2.2.6 2003/07/02 17:04:50 sam Exp $	*/
/*	$OpenBSD: cryptodev.h,v 1.33 2002/07/17 23:52:39 art Exp $	*/

/*-
 * Copyright (c) 2008 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Coyote Point Systems, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * The author of this code is Angelos D. Keromytis (angelos@cis.upenn.edu)
 *
 * This code was written by Angelos D. Keromytis in Athens, Greece, in
 * February 2000. Network Security Technologies Inc. (NSTI) kindly
 * supported the development of this code.
 *
 * Copyright (c) 2000 Angelos D. Keromytis
 *
 * Permission to use, copy, and modify this software with or without fee
 * is hereby granted, provided that this entire notice is included in
 * all source code copies of any software which is or includes a copy or
 * modification of this software.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTY. IN PARTICULAR, NONE OF THE AUTHORS MAKES ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE
 * MERCHANTABILITY OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR
 * PURPOSE.
 *
 * Copyright (c) 2001 Theo de Raadt
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Effort sponsored in part by the Defense Advanced Research Projects
 * Agency (DARPA) and Air Force Research Laboratory, Air Force
 * Materiel Command, USAF, under agreement number F30602-01-2-0537.
 *
 */

#ifndef _CRYPTO_CRYPTO_H_
#define _CRYPTO_CRYPTO_H_

#include <sys/ioccom.h>
#include <sys/condvar.h>

/* Some initial values */
#define CRYPTO_DRIVERS_INITIAL	4
#define CRYPTO_SW_SESSIONS	32

/* HMAC values */
#define HMAC_BLOCK_LEN		64
#define HMAC_IPAD_VAL		0x36
#define HMAC_OPAD_VAL		0x5C

/* Encryption algorithm block sizes */
#define DES_BLOCK_LEN		8
#define DES3_BLOCK_LEN		8
#define BLOWFISH_BLOCK_LEN	8
#define SKIPJACK_BLOCK_LEN	8
#define CAST128_BLOCK_LEN	8
#define RIJNDAEL128_BLOCK_LEN	16
#define EALG_MAX_BLOCK_LEN	16 /* Keep this updated */

/* Maximum hash algorithm result length */
#define AALG_MAX_RESULT_LEN	64 /* Keep this updated */

#define	CRYPTO_ALGORITHM_MIN	1
#define CRYPTO_DES_CBC		1
#define CRYPTO_3DES_CBC		2
#define CRYPTO_BLF_CBC		3
#define CRYPTO_CAST_CBC		4
#define CRYPTO_SKIPJACK_CBC	5
#define CRYPTO_MD5_HMAC		6
#define CRYPTO_SHA1_HMAC	7
#define CRYPTO_RIPEMD160_HMAC	8
#define CRYPTO_MD5_KPDK		9
#define CRYPTO_SHA1_KPDK	10
#define CRYPTO_RIJNDAEL128_CBC	11 /* 128 bit blocksize */
#define CRYPTO_AES_CBC		11 /* 128 bit blocksize -- the same as above */
#define CRYPTO_ARC4		12
#define CRYPTO_MD5		13
#define CRYPTO_SHA1		14
#define	CRYPTO_SHA2_HMAC	15
#define CRYPTO_NULL_HMAC	16
#define CRYPTO_NULL_CBC		17
#define CRYPTO_DEFLATE_COMP	18 /* Deflate compression algorithm */
#define CRYPTO_MD5_HMAC_96	19 
#define CRYPTO_SHA1_HMAC_96	20
#define CRYPTO_RIPEMD160_HMAC_96	21
#define CRYPTO_GZIP_COMP	22 /* Deflate compression algorithm */
#define CRYPTO_ALGORITHM_MAX	23 /* Keep updated - see below */

/* Algorithm flags */
#define	CRYPTO_ALG_FLAG_SUPPORTED	0x01 /* Algorithm is supported */
#define	CRYPTO_ALG_FLAG_RNG_ENABLE	0x02 /* Has HW RNG for DH/DSA */
#define	CRYPTO_ALG_FLAG_DSA_SHA		0x04 /* Can do SHA on msg */

struct session_op {
	u_int32_t	cipher;		/* ie. CRYPTO_DES_CBC */
	u_int32_t	mac;		/* ie. CRYPTO_MD5_HMAC */
	u_int32_t	comp_alg;	/* ie. CRYPTO_GZIP_COMP */

	u_int32_t	keylen;		/* cipher key */
	void *		key;
	int		mackeylen;	/* mac key */
	void *		mackey;

  	u_int32_t	ses;		/* returns: session # */
};

/* to support multiple session creation */

struct session_n_op {
	u_int32_t	cipher;		/* ie. CRYPTO_DES_CBC */
	u_int32_t	mac;		/* ie. CRYPTO_MD5_HMAC */
	u_int32_t	comp_alg;	/* ie. CRYPTO_GZIP_COMP */

	u_int32_t	keylen;		/* cipher key */
	void *		key;
	int		mackeylen;	/* mac key */
	void *		mackey;

	u_int32_t	ses;		/* returns: session # */
	int		status;
};

struct crypt_op {
	u_int32_t	ses;
	u_int16_t	op;		/* i.e. COP_ENCRYPT */
#define COP_ENCRYPT	1
#define COP_DECRYPT	2
#define COP_COMP	3
#define COP_DECOMP	4
	u_int16_t	flags;
#define	COP_F_BATCH 	0x0008		/* Dispatch as quickly as possible */
	u_int		len;		/* src len */
	void *		src, *dst;	/* become iov[] inside kernel */
	void *		mac;		/* must be big enough for chosen MAC */
	void *		iv;
	u_int		dst_len;	/* dst len if not 0 */
};

/* to support multiple session creation */
/*
 *
 * The reqid field is filled when the operation has 
 * been accepted and started, and can be used to later retrieve
 * the operation results via CIOCNCRYPTRET or identify the 
 * request in the completion list returned by CIOCNCRYPTRETM.
 *
 * The opaque pointer can be set arbitrarily by the user
 * and it is passed back in the crypt_result structure
 * when the request completes.  This field can be used for example
 * to track context for the request and avoid lookups in the
 * user application.
 */

struct crypt_n_op {
	u_int32_t	ses;
	u_int16_t	op;		/* i.e. COP_ENCRYPT */
#define COP_ENCRYPT	1
#define COP_DECRYPT	2
	u_int16_t	flags;
#define COP_F_BATCH	0x0008		/* Dispatch as quickly as possible */
#define COP_F_MORE	0x0010		/* more data to follow */
	u_int		len;		/* src len */

	u_int32_t	reqid;		/* request id */
	int		status;		/* status of request -accepted or not */	
	void		*opaque;	/* opaque pointer returned to user */
	u_int32_t	keylen;		/* cipher key - optional */
	void *		key;
	u_int32_t	mackeylen;	/* also optional */
	void *		mackey;

	void *		src, *dst;	/* become iov[] inside kernel */
	void *		mac;		/* must be big enough for chosen MAC */
	void *		iv;
	u_int		dst_len;	/* dst len if not 0 */
};

/* CIOCNCRYPTM ioctl argument, supporting one or more asynchronous
 * crypt_n_op operations.
 * Each crypt_n_op will receive a request id which can be used to check its
 * status via CIOCNCRYPTRET, or to watch for its completion in the list
 * obtained via CIOCNCRYPTRETM.
 */
struct crypt_mop {
	size_t 		count;		/* how many */
	struct crypt_n_op *	reqs;	/* where to get them */
};

struct crypt_sfop {
	size_t		count;
	u_int32_t	*sesid;
}; 

struct crypt_sgop {
	size_t		count;
	struct session_n_op * sessions;
};

#define CRYPTO_MAX_MAC_LEN	20

/* bignum parameter, in packed bytes, ... */
struct crparam {
	void *		crp_p;
	u_int		crp_nbits;
};

#define CRK_MAXPARAM	8

struct crypt_kop {
	u_int		crk_op;		/* ie. CRK_MOD_EXP or other */
	u_int		crk_status;	/* return status */
	u_short		crk_iparams;	/* # of input parameters */
	u_short		crk_oparams;	/* # of output parameters */
	u_int		crk_pad1;
	struct crparam	crk_param[CRK_MAXPARAM];
};

/*
 * Used with the CIOCNFKEYM ioctl.
 *
 * This structure allows the OCF to return a request id
 * for each of the kop operations specified in the CIOCNFKEYM call.
 * 
 * The crk_opaque pointer can be arbitrarily set by the user
 * and it is passed back in the crypt_result structure
 * when the request completes.  This field can be used for example
 * to track context for the request and avoid lookups in the
 * user application.
 */
struct crypt_n_kop {
	u_int		crk_op;		/* ie. CRK_MOD_EXP or other */
	u_int		crk_status;	/* return status */
	u_short		crk_iparams;	/* # of input parameters */
	u_short		crk_oparams;	/* # of output parameters */
        u_int32_t	crk_reqid;	/* request id */
	struct crparam	crk_param[CRK_MAXPARAM];
	void		*crk_opaque;	/* opaque pointer returned to user */
};

struct crypt_mkop {
	size_t	count;			/* how many */
	struct crypt_n_kop *	reqs;	/* where to get them */
};

/* Asynchronous key or crypto result.
 * Note that the status will be set in the crypt_result structure,
 * not in the original crypt_kop structure (crk_status).
 */
struct crypt_result {
	u_int32_t	reqid;		/* request id */
	u_int32_t	status;		/* status of request: 0 if successful */
	void *		opaque;		/* Opaque pointer from the user, passed along */
};

struct cryptret {
	size_t		count;		/* space for how many */
	struct crypt_result *	results;	/* where to put them */
}; 


/* Assymetric key operations */
#define	CRK_ALGORITM_MIN	0
#define CRK_MOD_EXP		0
#define CRK_MOD_EXP_CRT		1
#define CRK_DSA_SIGN		2
#define CRK_DSA_VERIFY		3
#define CRK_DH_COMPUTE_KEY	4
#define CRK_MOD_ADD		5
#define CRK_MOD_ADDINV		6
#define CRK_MOD_SUB		7
#define CRK_MOD_MULT		8
#define CRK_MOD_MULTINV		9
#define CRK_MOD			10
#define CRK_ALGORITHM_MAX	10 /* Keep updated - see below */

#define CRF_MOD_EXP		(1 << CRK_MOD_EXP)
#define CRF_MOD_EXP_CRT		(1 << CRK_MOD_EXP_CRT)
#define CRF_DSA_SIGN		(1 << CRK_DSA_SIGN)
#define CRF_DSA_VERIFY		(1 << CRK_DSA_VERIFY)
#define CRF_DH_COMPUTE_KEY	(1 << CRK_DH_COMPUTE_KEY)
#define CRF_MOD_ADD		(1 << CRK_MOD_ADD)
#define CRF_MOD_ADDINV		(1 << CRK_MOD_ADDINV)
#define CRF_MOD_SUB		(1 << CRK_MOD_SUB)
#define CRF_MOD_MULT		(1 << CRK_MOD_MULT)
#define CRF_MOD_MULTINV		(1 << CRK_MOD_MULTINV)
#define CRF_MOD			(1 << CRK_MOD)

/*
 * A large comment here once held descriptions of the ioctl
 * requests implemented by the device.  This text has been moved
 * to the crypto(4) manual page and, later, removed from this file
 * as it was always a step behind the times.
 */

/*
 * done against open of /dev/crypto, to get a cloned descriptor.
 * Please use F_SETFD against the cloned descriptor.  But this ioctl
 * is obsolete (the device now clones): please, just don't use it.
 */
#define	CRIOGET		_IOWR('c', 100, u_int32_t)

/* the following are done against the cloned descriptor */
#define	CIOCFSESSION	_IOW('c', 102, u_int32_t)
#define CIOCKEY		_IOWR('c', 104, struct crypt_kop)
#define CIOCNFKEYM	_IOWR('c', 108, struct crypt_mkop)
#define CIOCNFSESSION	_IOW('c', 109, struct crypt_sfop)
#define CIOCNCRYPTRETM	_IOWR('c', 110, struct cryptret)
#define CIOCNCRYPTRET	_IOWR('c', 111, struct crypt_result)

#define	CIOCGSESSION	_IOWR('c', 112, struct session_op)
#define	CIOCNGSESSION	_IOWR('c', 113, struct crypt_sgop)
#define CIOCCRYPT	_IOWR('c', 114, struct crypt_op)
#define CIOCNCRYPTM	_IOWR('c', 115, struct crypt_mop)

#define CIOCASYMFEAT	_IOR('c', 105, u_int32_t)

struct cryptotstat {
	struct timespec	acc;		/* total accumulated time */
	struct timespec	min;		/* max time */
	struct timespec	max;		/* max time */
	u_int32_t	count;		/* number of observations */
};

struct cryptostats {
	u_int32_t	cs_ops;		/* symmetric crypto ops submitted */
	u_int32_t	cs_errs;	/* symmetric crypto ops that failed */
	u_int32_t	cs_kops;	/* asymetric/key ops submitted */
	u_int32_t	cs_kerrs;	/* asymetric/key ops that failed */
	u_int32_t	cs_intrs;	/* crypto swi thread activations */
	u_int32_t	cs_rets;	/* crypto return thread activations */
	u_int32_t	cs_blocks;	/* symmetric op driver block */
	u_int32_t	cs_kblocks;	/* symmetric op driver block */
	/*
	 * When CRYPTO_TIMING is defined at compile time and the
	 * sysctl debug.crypto is set to 1, the crypto system will
	 * accumulate statistics about how long it takes to process
	 * crypto requests at various points during processing.
	 */
	struct cryptotstat cs_invoke;	/* crypto_dipsatch -> crypto_invoke */
	struct cryptotstat cs_done;	/* crypto_invoke -> crypto_done */
	struct cryptotstat cs_cb;	/* crypto_done -> callback */
	struct cryptotstat cs_finis;	/* callback -> callback return */
};

#ifdef _KERNEL
/* Standard initialization structure beginning */
struct cryptoini {
	int		cri_alg;	/* Algorithm to use */
	int		cri_klen;	/* Key length, in bits */
	int		cri_rnd;	/* Algorithm rounds, where relevant */
	char	       *cri_key;	/* key to use */
	u_int8_t	cri_iv[EALG_MAX_BLOCK_LEN];	/* IV to use */
	struct cryptoini *cri_next;
};

/* Describe boundaries of a single crypto operation */
struct cryptodesc {
	int		crd_skip;	/* How many bytes to ignore from start */
	int		crd_len;	/* How many bytes to process */
	int		crd_inject;	/* Where to inject results, if applicable */
	int		crd_flags;

#define	CRD_F_ENCRYPT		0x01	/* Set when doing encryption */
#define	CRD_F_IV_PRESENT	0x02	/* When encrypting, IV is already in
					   place, so don't copy. */
#define	CRD_F_IV_EXPLICIT	0x04	/* IV explicitly provided */
#define	CRD_F_DSA_SHA_NEEDED	0x08	/* Compute SHA-1 of buffer for DSA */
#define CRD_F_COMP		0x10    /* Set when doing compression */

	struct cryptoini	CRD_INI; /* Initialization/context data */
#define crd_iv		CRD_INI.cri_iv
#define crd_key		CRD_INI.cri_key
#define crd_rnd		CRD_INI.cri_rnd
#define crd_alg		CRD_INI.cri_alg
#define crd_klen	CRD_INI.cri_klen

	struct cryptodesc *crd_next;
};

/* Structure describing complete operation */
struct cryptop {
	TAILQ_ENTRY(cryptop) crp_next;
	u_int64_t	crp_sid;	/* Session ID */
	
	u_int32_t	crp_reqid;	/* request id */
	void *		crp_usropaque;	/* Opaque pointer from user, passed along */

	int		crp_ilen;	/* Input data total length */
	int		crp_olen;	/* Result total length */

	int		crp_etype;	/*
					 * Error type (zero means no error).
					 * All error codes except EAGAIN
					 * indicate possible data corruption (as in,
					 * the data have been touched). On all
					 * errors, the crp_sid may have changed
					 * (reset to a new one), so the caller
					 * should always check and use the new
					 * value on future requests.
					 */
	int		crp_flags;	/* Note: must hold mutext to modify */

#define CRYPTO_F_IMBUF		0x0001	/* Input/output are mbuf chains */
#define CRYPTO_F_IOV		0x0002	/* Input/output are uio */
#define CRYPTO_F_REL		0x0004	/* Must return data in same place */
#define	CRYPTO_F_BATCH		0x0008	/* Batch op if possible possible */
#define	CRYPTO_F_CBIMM		0x0010	/* Do callback immediately */
#define	CRYPTO_F_DONE		0x0020	/* Operation completed */
#define	CRYPTO_F_CBIFSYNC	0x0040	/* Do CBIMM if op is synchronous */
#define	CRYPTO_F_ONRETQ		0x0080	/* Request is on return queue */
#define	CRYPTO_F_USER		0x0100	/* Request is in user context */
#define	CRYPTO_F_MORE		0x0200	/* more data to follow */

	void *		crp_buf;	/* Data to be processed */
	void *		crp_opaque;	/* Opaque pointer, passed along */
	struct cryptodesc *crp_desc;	/* Linked list of processing descriptors */

	int (*crp_callback)(struct cryptop *); /* Callback function */

	void *		crp_mac;
	struct timespec	crp_tstamp;	/* performance time stamp */
	kcondvar_t	crp_cv;
	struct fcrypt 	*fcrp;
	void * 		dst;
	void *		mac;
	u_int		len;
	u_char		tmp_iv[EALG_MAX_BLOCK_LEN];
	u_char		tmp_mac[CRYPTO_MAX_MAC_LEN];
	
	struct iovec	iovec[1];
	struct uio	uio;
	uint32_t	magic;
};

#define CRYPTO_BUF_CONTIG	0x0
#define CRYPTO_BUF_IOV		0x1
#define CRYPTO_BUF_MBUF		0x2

#define CRYPTO_OP_DECRYPT	0x0
#define CRYPTO_OP_ENCRYPT	0x1

/*
 * Hints passed to process methods.
 */
#define	CRYPTO_HINT_MORE	0x1	/* more ops coming shortly */

struct cryptkop {
	TAILQ_ENTRY(cryptkop) krp_next;

	u_int32_t	krp_reqid;	/* request id */
	void *		krp_usropaque;	/* Opaque pointer from user, passed along */

	u_int		krp_op;		/* ie. CRK_MOD_EXP or other */
	u_int		krp_status;	/* return status */
	u_short		krp_iparams;	/* # of input parameters */
	u_short		krp_oparams;	/* # of output parameters */
	u_int32_t	krp_hid;
	struct crparam	krp_param[CRK_MAXPARAM];	/* kvm */
	int		(*krp_callback)(struct cryptkop *);
	int		krp_flags;	/* same values as crp_flags */
	kcondvar_t	krp_cv;
	struct fcrypt 	*fcrp;
	struct crparam	crk_param[CRK_MAXPARAM];
};

/* Crypto capabilities structure */
struct cryptocap {
	u_int32_t	cc_sessions;

	/*
	 * Largest possible operator length (in bits) for each type of
	 * encryption algorithm.
	 */
	u_int16_t	cc_max_op_len[CRYPTO_ALGORITHM_MAX + 1];

	u_int8_t	cc_alg[CRYPTO_ALGORITHM_MAX + 1];

	u_int8_t	cc_kalg[CRK_ALGORITHM_MAX + 1];

	u_int8_t	cc_flags;
	u_int8_t	cc_qblocked;		/* symmetric q blocked */
	u_int8_t	cc_kqblocked;		/* asymmetric q blocked */
#define CRYPTOCAP_F_CLEANUP	0x01		/* needs resource cleanup */
#define CRYPTOCAP_F_SOFTWARE	0x02		/* software implementation */
#define CRYPTOCAP_F_SYNC	0x04		/* operates synchronously */

	void		*cc_arg;		/* callback argument */
	int		(*cc_newsession)(void*, u_int32_t*, struct cryptoini*);
	int		(*cc_process) (void*, struct cryptop *, int);
	int		(*cc_freesession) (void*, u_int64_t);
	void		*cc_karg;		/* callback argument */
	int		(*cc_kprocess) (void*, struct cryptkop *, int);
};

/*
 * Session ids are 64 bits.  The lower 32 bits contain a "local id" which
 * is a driver-private session identifier.  The upper 32 bits contain a
 * "hardware id" used by the core crypto code to identify the driver and
 * a copy of the driver's capabilities that can be used by client code to
 * optimize operation.
 */
#define	CRYPTO_SESID2HID(_sid)	(((_sid) >> 32) & 0xffffff)
#define	CRYPTO_SESID2CAPS(_sid)	(((_sid) >> 56) & 0xff)
#define	CRYPTO_SESID2LID(_sid)	(((u_int32_t) (_sid)) & 0xffffffff)

MALLOC_DECLARE(M_CRYPTO_DATA);

extern	int crypto_newsession(u_int64_t *sid, struct cryptoini *cri, int hard);
extern	int crypto_freesession(u_int64_t sid);
extern	int32_t crypto_get_driverid(u_int32_t flags);
extern	int crypto_register(u_int32_t driverid, int alg, u_int16_t maxoplen,
	    u_int32_t flags,
	    int (*newses)(void*, u_int32_t*, struct cryptoini*),
	    int (*freeses)(void*, u_int64_t),
	    int (*process)(void*, struct cryptop *, int),
	    void *arg);
extern	int crypto_kregister(u_int32_t, int, u_int32_t,
	    int (*)(void*, struct cryptkop *, int),
	    void *arg);
extern	int crypto_unregister(u_int32_t driverid, int alg);
extern	int crypto_unregister_all(u_int32_t driverid);
extern	int crypto_dispatch(struct cryptop *crp);
extern	int crypto_kdispatch(struct cryptkop *);
#define	CRYPTO_SYMQ	0x1
#define	CRYPTO_ASYMQ	0x2
extern	int crypto_unblock(u_int32_t, int);
extern	void crypto_done(struct cryptop *crp);
extern	void crypto_kdone(struct cryptkop *);
extern	int crypto_getfeat(int *);

void	cuio_copydata(struct uio *, int, int, void *);
void	cuio_copyback(struct uio *, int, int, void *);
int	cuio_apply(struct uio *, int, int,
	    int (*f)(void *, void *, unsigned int), void *);

extern	int crypto_ret_q_remove(struct cryptop *);
extern	int crypto_ret_kq_remove(struct cryptkop *);
extern	void crypto_freereq(struct cryptop *crp);
extern	struct cryptop *crypto_getreq(int num);

extern	int crypto_usercrypto;		/* userland may do crypto requests */
extern	int crypto_userasymcrypto;	/* userland may do asym crypto reqs */
extern	int crypto_devallowsoft;	/* only use hardware crypto */

/*
 * Asymmetric operations are allocated in cryptodev.c but can be
 * freed in crypto.c.
 */
extern struct pool	cryptkop_pool;

/*
 * Mutual exclusion and its unwelcome friends.
 */

extern	kmutex_t	crypto_mtx;

/*
 * initialize the crypto framework subsystem (not the pseudo-device).
 * This must be called very early in boot, so the framework is ready
 * to handle registration requests when crpto hardware is autoconfigured.
 * (This declaration doesnt really belong here but there's no header
 * for the raw framework.)
 */
void	crypto_init(void);

/*
 * Crypto-related utility routines used mainly by drivers.
 *
 * XXX these don't really belong here; but for now they're
 *     kept apart from the rest of the system.
 */
struct mbuf;
struct	mbuf	*m_getptr(struct mbuf *, int, int *);

struct uio;
extern	void cuio_copydata(struct uio* uio, int off, int len, void *cp);
extern	void cuio_copyback(struct uio* uio, int off, int len, void *cp);
extern int	cuio_getptr(struct uio *, int loc, int *off);

#ifdef CRYPTO_DEBUG	/* yuck, netipsec defines these differently */
#ifndef DPRINTF
#define DPRINTF(a) uprintf a
#endif
#ifndef DCPRINTF
#define DCPRINTF(a) printf a
#endif
#else
#ifndef DPRINTF
#define DPRINTF(a)
#endif
#ifndef DCPRINTF
#define DCPRINTF(a)
#endif
#endif

#endif /* _KERNEL */
#endif /* _CRYPTO_CRYPTO_H_ */
