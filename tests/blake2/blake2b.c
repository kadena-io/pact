// blake2b.c
// A simple BLAKE2b Reference Implementation.

#include "blake2b.h"
#include <stdio.h>
#include <string.h>


// Cyclic right rotation.

#ifndef ROTR64
#define ROTR64(x, y)  (((x) >> (y)) ^ ((x) << (64 - (y))))
#endif

// Little-endian byte access.

#define B2B_GET64(p)                            \
    (((uint64_t) ((uint8_t *) (p))[0]) ^        \
    (((uint64_t) ((uint8_t *) (p))[1]) << 8) ^  \
    (((uint64_t) ((uint8_t *) (p))[2]) << 16) ^ \
    (((uint64_t) ((uint8_t *) (p))[3]) << 24) ^ \
    (((uint64_t) ((uint8_t *) (p))[4]) << 32) ^ \
    (((uint64_t) ((uint8_t *) (p))[5]) << 40) ^ \
    (((uint64_t) ((uint8_t *) (p))[6]) << 48) ^ \
    (((uint64_t) ((uint8_t *) (p))[7]) << 56))

// G Mixing function.

#define B2B_G(a, b, c, d, x, y) {   \
    v[a] = v[a] + v[b] + x;         \
    v[d] = ROTR64(v[d] ^ v[a], 32); \
    v[c] = v[c] + v[d];             \
    v[b] = ROTR64(v[b] ^ v[c], 24); \
    v[a] = v[a] + v[b] + y;         \
    v[d] = ROTR64(v[d] ^ v[a], 16); \
    v[c] = v[c] + v[d];             \
    v[b] = ROTR64(v[b] ^ v[c], 63); }

// Initialization Vector.

static const uint64_t blake2b_iv[8] = {
    0x6A09E667F3BCC908, 0xBB67AE8584CAA73B,
    0x3C6EF372FE94F82B, 0xA54FF53A5F1D36F1,
    0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
    0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179
};

static const uint8_t sigma[12][16] = {
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
        { 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 },
        { 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 },
        { 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 },
        { 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 },
        { 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 },
        { 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 },
        { 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 },
        { 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 },
        { 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 },
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
        { 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 }
    };


void printv(char* n, int len, uint64_t v[]) {
  printf("%s [",n);
    for (int i = 0; i < len; i++) {
      if (i>0) printf(",");
      printf("%llu",v[i]);
    }
  printf("]");
}

void printb(char* n, int len, uint8_t v[]) {
  printf("%s [",n);
    for (int i = 0; i < len; i++) {
      if (i>0) printf(",");
      printf("%d",v[i]);
    }
  printf("]");
}


void printctx(blake2b_ctx ctx) {
  printf("ctx: Blake2BCtx {");
  printb("_b=pack",128,ctx.b);
  printv(",_h=mk",8,ctx.h);
  printf(",_t0=%llu,_t1=%llu,_c=%lu,_outlen=%lu}\n",ctx.t[0],ctx.t[1],ctx.c,ctx.outlen);
}

// Compression function. "last" flag indicates last block.

static void blake2b_compress(blake2b_ctx *ctx, int last)
{

    int i;
    uint64_t v[16], m[16];

    for (i = 0; i < 8; i++) {           // init work variables
        v[i] = ctx->h[i];
        v[i + 8] = blake2b_iv[i];
    }

    v[12] ^= ctx->t[0];                 // low 64 bits of offset
    v[13] ^= ctx->t[1];                 // high 64 bits
    if (last)                           // last block flag set ?
        v[14] = ~v[14];
    //printv("v1",v);
    for (i = 0; i < 16; i++)            // get little-endian words
        m[i] = B2B_GET64(&ctx->b[8 * i]);
    //printv("m",m);
    for (i = 0; i < 12; i++) {          // twelve rounds
        B2B_G( 0, 4,  8, 12, m[sigma[i][ 0]], m[sigma[i][ 1]]);
        B2B_G( 1, 5,  9, 13, m[sigma[i][ 2]], m[sigma[i][ 3]]);
        B2B_G( 2, 6, 10, 14, m[sigma[i][ 4]], m[sigma[i][ 5]]);
        B2B_G( 3, 7, 11, 15, m[sigma[i][ 6]], m[sigma[i][ 7]]);
        B2B_G( 0, 5, 10, 15, m[sigma[i][ 8]], m[sigma[i][ 9]]);
        B2B_G( 1, 6, 11, 12, m[sigma[i][10]], m[sigma[i][11]]);
        B2B_G( 2, 7,  8, 13, m[sigma[i][12]], m[sigma[i][13]]);
        B2B_G( 3, 4,  9, 14, m[sigma[i][14]], m[sigma[i][15]]);
    }
    //printv("v2",v);
    for( i = 0; i < 8; ++i )
        ctx->h[i] ^= v[i] ^ v[i + 8];
}

// Initialize the hashing context "ctx" with optional key "key".
//      1 <= outlen <= 64 gives the digest size in bytes.
//      Secret key (also <= 64 bytes) is optional (keylen = 0).

int blake2b_init(blake2b_ctx *ctx, size_t outlen,
    const void *key, size_t keylen)        // (keylen=0: no key)
{
    size_t i;

    if (outlen == 0 || outlen > 64 || keylen > 64)
        return -1;                      // illegal parameters

    for (i = 0; i < 8; i++)             // state, "param block"
        ctx->h[i] = blake2b_iv[i];
    //printf("ctxh %llu -> ",ctx->h[0]);
    ctx->h[0] ^= 0x01010000 ^ (keylen << 8) ^ outlen;
    //printf("%llu\n",ctx->h[0]);

    ctx->t[0] = 0;                      // input count low word
    ctx->t[1] = 0;                      // input count high word
    ctx->c = 0;                         // pointer within buffer
    ctx->outlen = outlen;

    for (i = keylen; i < 128; i++)      // zero input block
        ctx->b[i] = 0;
    if (keylen > 0) {
        blake2b_update(ctx, key, keylen);
        ctx->c = 128;                   // at the end
    }


    return 0;
}

// Add "inlen" bytes from "in" into the hash.

void blake2b_update(blake2b_ctx *ctx,
    const void *in, size_t inlen)       // data bytes
{
    size_t i;

    for (i = 0; i < inlen; i++) {
        if (ctx->c == 128) {            // buffer full ?
            ctx->t[0] += ctx->c;        // add counters
            if (ctx->t[0] < ctx->c)     // carry overflow ?
                ctx->t[1]++;            // high word
            blake2b_compress(ctx, 0);   // compress (not last)
            ctx->c = 0;                 // counter to zero
        }
        ctx->b[ctx->c++] = ((const uint8_t *) in)[i];
    }
}

// Generate the message digest (size given in init).
//      Result placed in "out".

void blake2b_final(blake2b_ctx *ctx, void *out)
{
    size_t i;

    ctx->t[0] += ctx->c;                // mark last block offset
    if (ctx->t[0] < ctx->c)             // carry overflow
        ctx->t[1]++;                    // high word

    while (ctx->c < 128)                // fill up with zeros
        ctx->b[ctx->c++] = 0;
    blake2b_compress(ctx, 1);           // final block flag = 1

    // little endian convert and store
    for (i = 0; i < ctx->outlen; i++) {
        ((uint8_t *) out)[i] =
            (ctx->h[i >> 3] >> (8 * (i & 7))) & 0xFF;
    }
}

// Convenience function for all-in-one computation.

int blake2b(void *out, size_t outlen,
    const void *key, size_t keylen,
    const void *in, size_t inlen)
{
    blake2b_ctx ctx;

    if (blake2b_init(&ctx, outlen, key, keylen))
        return -1;
    printf("init: outlen=%lu keylen=%lu\n",outlen,keylen);
    printf("pre-update_");
    printctx(ctx);
    blake2b_update(&ctx, in, inlen);
    printf("post-update_");
    printctx(ctx);
    blake2b_final(&ctx, out);

    return 0;
}





static void selftest_seq(uint8_t *out, size_t len, uint32_t seed)
{
    size_t i;
    uint32_t t, a , b;

    a = 0xDEAD4BAD * seed;              // prime
    b = 1;

    for (i = 0; i < len; i++) {         // fill the buf
        t = a + b;
        a = b;
        b = t;
        out[i] = (t >> 24) & 0xFF;
    }
}

int blake2b_selftest() {
    // grand hash of hash results
    const uint8_t blake2b_res[32] = {
        0xC2, 0x3A, 0x78, 0x00, 0xD9, 0x81, 0x23, 0xBD,
        0x10, 0xF5, 0x06, 0xC6, 0x1E, 0x29, 0xDA, 0x56,
        0x03, 0xD7, 0x63, 0xB8, 0xBB, 0xAD, 0x2E, 0x73,
        0x7F, 0x5E, 0x76, 0x5A, 0x7B, 0xCC, 0xD4, 0x75
    };
    // parameter sets
    const size_t b2b_md_len[4] = { 20, 32, 48, 64 };
    const size_t b2b_in_len[6] = { 0, 3, 128, 129, 255, 1024 };

    size_t i, j, outlen, inlen;
    uint8_t in[1024], md[64], key[64];
    blake2b_ctx ctx;

    // 256-bit hash for testing
    if (blake2b_init(&ctx, 32, NULL, 0))
        return -1;

    for (i = 0; i < 4; i++) {
        outlen = b2b_md_len[i];
        for (j = 0; j < 6; j++) {
            inlen = b2b_in_len[j];
            printf("outlen: %lu,inlen: %lu\n",outlen,inlen);
            selftest_seq(in, inlen, inlen);     // unkeyed hash
            printb("in:",inlen,in);printf("\n");
            blake2b(md, outlen, NULL, 0, in, inlen);
            printb("bl1:",outlen,md);printf("\n");
            blake2b_update(&ctx, md, outlen);   // hash the hash
            printctx(ctx);
            selftest_seq(key, outlen, outlen);  // keyed hash
            printb("key:",outlen,key);printf("\n");
            blake2b(md, outlen, key, outlen, in, inlen);
            printb("bl2:",outlen,md);printf("\n");
            blake2b_update(&ctx, md, outlen);   // hash the hash
            printctx(ctx);
        }
    }

    // compute and compare the hash of hashes
    blake2b_final(&ctx, md);
    for (i = 0; i < 32; i++) {
        if (md[i] != blake2b_res[i])
            return -1;
    }

    return 0;
}


int main() {

  // printf("rotr64 31 47 = %llu\n",ROTR64((uint64_t)31,47));
  uint64_t v[16] = {7640891576939301192llu,13503953896175478587llu,4354685564936845355llu,11912009170470909681llu,5840696475078001361llu,11170449401992604703llu,2270897969802886507llu,6620516959819538809llu,7640891576956012808llu,13503953896175478587llu,4354685564936845355llu,11912009170470909681llu,5840696475078001361llu,11170449401992604703llu,2270897969802886507llu,6620516959819538809llu};
    uint64_t m[16] = {6513249llu,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  /*for (int i = 0; i < 16; i++) {
      v[i] = i;
      m[i] = i;
      }*/

    // printf("sigs: %llu %llu\n",m[sigma[0][ 0]], m[sigma[0][ 1]]);
        B2B_G( 0, 4,  8, 12, m[sigma[0][ 0]], m[sigma[0][ 1]]);
        B2B_G( 1, 5,  9, 13, m[sigma[0][ 2]], m[sigma[0][ 3]]);
        B2B_G( 2, 6, 10, 14, m[sigma[0][ 4]], m[sigma[0][ 5]]);
        B2B_G( 3, 7, 11, 15, m[sigma[0][ 6]], m[sigma[0][ 7]]);
        B2B_G( 0, 5, 10, 15, m[sigma[0][ 8]], m[sigma[0][ 9]]);
        B2B_G( 1, 6, 11, 12, m[sigma[0][10]], m[sigma[0][11]]);
        B2B_G( 2, 7,  8, 13, m[sigma[0][12]], m[sigma[0][13]]);
        B2B_G( 3, 4,  9, 14, m[sigma[0][14]], m[sigma[0][15]]);

       // printv("v",v);
    blake2b_ctx ctx;

    // 256-bit hash for testing
    if (blake2b_init(&ctx, 32, NULL, 0))
      return -1;
    /*
    ctx.b[0]='a';
    ctx.b[1]='b';
    ctx.b[2]='c';
    */
    printctx(ctx);

    // blake2b_compress(&ctx,0);
    char* d = "akjsvhsjkdfhvjsakhfvjkdhsvnfklsjanvskdjvkdjsvfksdhnvkjdfavksnksjnfajvjklsnkjsfdajkldahvfadvndfjvalkhfnvkdfjsnvkljnkfkajsdnaskjskdjvfsfvkkdsadshfjkalsvaklsdhvnlksfjvnskdhnhjdnshsdfkvnfvads";
    printf ("length: %lu\n", strlen(d));
    blake2b_update(&ctx,d,strlen(d));
    uint8_t o[32];
    blake2b_final(&ctx,o);
    printctx(ctx);
    printf("o: [");
    for (int i = 0 ; i < 32; i ++) {
      printf("%d,",o[i]);
    }
    printf("]\n");


    printf("test: %d", blake2b_selftest());

  return 0;
}
